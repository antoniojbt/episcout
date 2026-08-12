# episcout: neutral synthetic longitudinal data from PostgreSQL to an owned EDA delivery
#
# Run this file one numbered section at a time in an interactive R session.
# The example uses only neutral synthetic records and an approved disposable PostgreSQL database. The caller owns the database, connection, credentials, output root and disclosure decision. Explicit output actions below are caller declarations, not inferred by core EDA.

# 1. Load packages and locate the installed example ---------------------------

library(episcout)

required_packages <- c(
  "DBI", "RPostgres", "data.table", "ggplot2", "rmarkdown", "knitr"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Install the walkthrough packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}
example_dir <- system.file(
  "examples", "db-to-report",
  package = "episcout"
)
if (!nzchar(example_dir)) {
  example_dir <- file.path("inst", "examples", "db-to-report")
}
if (!dir.exists(example_dir)) {
  stop("Run from the episcout repository or an installed package.", call. = FALSE)
}

csv_path <- file.path(example_dir, "synthetic-longitudinal.csv")
walkthrough_data <- epi_read(csv_path)
walkthrough_data$visit_date <- as.Date(walkthrough_data$visit_date)

dim(walkthrough_data)
epi_head_and_tail(walkthrough_data, rows = 3, cols = 6)

# 2. Check the intentional duplicate before loading PostgreSQL ----------------

# A person can correctly have several longitudinal visits. The declared record key is person plus visit number, so inspect repeated values of that key.
walkthrough_data$visit_key <- paste(
  walkthrough_data$source_person_id,
  walkthrough_data$visit_number,
  sep = "::"
)
duplicate_visits <- epi_clean_get_dups(
  walkthrough_data,
  var = "visit_key",
  freq = 1
)
duplicate_visits

# compare is suggested rather than required. When installed, this confirms that the two PX002 visit-2 rows have no differing columns.
if (requireNamespace("compare", quietly = TRUE)) {
  duplicate_comparison <- epi_clean_compare_dup_rows(
    duplicate_visits,
    val_id = "PX002::2",
    col_id = "visit_key"
  )
  duplicate_comparison
}

# The source owner confirms this pair is an accidental exact duplicate. Remove only exact whole-row copies; never choose a winner between conflicting rows.
source_columns <- setdiff(names(walkthrough_data), "visit_key")
exact_duplicate <- duplicated(walkthrough_data[source_columns])
clean_longitudinal <- walkthrough_data[!exact_duplicate, source_columns]
stopifnot(sum(exact_duplicate) == 1L)
stopifnot(!anyDuplicated(clean_longitudinal[c(
  "source_person_id", "visit_number"
)]))

# Separate person-grain and visit-grain relations. The consistency assertion is important: one person must not have conflicting baseline attributes.
person_columns <- c(
  "source_person_id", "sex", "cohort_group", "baseline_age"
)
participants <- unique(clean_longitudinal[person_columns])
stopifnot(!anyDuplicated(participants$source_person_id))

visits <- clean_longitudinal[c(
  "source_person_id", "cohort_group", "visit_number", "visit_date",
  "systolic_bp", "outcome", "symptom_text", "site"
)]

# 3. Connect to an approved disposable PostgreSQL database --------------------

# Configure PGHOST, PGPORT, PGDATABASE and PGUSER outside this script. Use an approved password store; PGPASSWORD is passed only when it already exists.
connection_arguments <- list(
  drv = RPostgres::Postgres(),
  host = Sys.getenv("PGHOST", unset = "127.0.0.1"),
  port = as.integer(Sys.getenv("PGPORT", unset = "5432")),
  dbname = Sys.getenv("PGDATABASE", unset = "episcout_walkthrough"),
  user = Sys.getenv("PGUSER", unset = Sys.info()[["user"]])
)
pg_password <- Sys.getenv("PGPASSWORD", unset = "")
if (nzchar(pg_password)) {
  connection_arguments$password <- pg_password
}

con <- do.call(DBI::dbConnect, connection_arguments)
DBI::dbIsValid(con)

# If you stop before the final section, close this session with DBI::dbDisconnect(con). The generated schemas are uniquely named and are not overwritten; have the database owner review those exact names before removing an interrupted run.

# Use unique schema names so a rerun cannot overwrite earlier walkthrough data.
run_id <- Sys.getenv(
  "EPISCOUT_WALKTHROUGH_RUN_ID",
  unset = paste0(format(Sys.time(), "%y%m%d%H%M%S"), Sys.getpid())
)
if (!grepl("^[a-z0-9]{6,20}$", run_id)) {
  DBI::dbDisconnect(con)
  stop(
    "EPISCOUT_WALKTHROUGH_RUN_ID must contain 6 to 20 lower-case letters or digits.",
    call. = FALSE
  )
}

source_schema <- paste0("episcout_walk_source_", run_id)
registry_schema <- paste0("episcout_walk_registry_", run_id)
output_schema <- paste0("episcout_walk_output_", run_id)
walkthrough_schemas <- c(source_schema, registry_schema, output_schema)

for (schema in walkthrough_schemas) {
  quoted_schema <- as.character(DBI::dbQuoteIdentifier(con, schema))
  DBI::dbExecute(con, paste("CREATE SCHEMA", quoted_schema))
  DBI::dbExecute(
    con,
    paste("REVOKE ALL ON SCHEMA", quoted_schema, "FROM PUBLIC")
  )
}

# Load the declared, deduplicated synthetic relations. Source identifiers stay in PostgreSQL after this point in the worked database workflow.
DBI::dbWriteTable(
  con,
  DBI::Id(schema = source_schema, table = "participants"),
  participants
)
DBI::dbWriteTable(
  con,
  DBI::Id(schema = source_schema, table = "visits"),
  visits
)

# 4. Inventory metadata and define a reusable semantic dictionary -------------

inventory <- epi_db_inventory(
  con,
  schema = source_schema,
  tables = c("participants", "visits"),
  row_counts = "exact"
)
inventory$tables
inventory$columns
inventory$constraints

dictionary <- epi_eda_dictionary_scaffold(inventory)

# These semantic declarations are valid only for the documented synthetic fixture. Declare them explicitly for every real source.
dictionary$provenance <- "declared_synthetic_walkthrough_v2"

labels <- c(
  source_person_id = "Source person identifier",
  sex = "Recorded sex",
  cohort_group = "Cohort group",
  baseline_age = "Age at baseline",
  visit_number = "Visit number",
  visit_date = "Visit date",
  systolic_bp = "Systolic blood pressure",
  outcome = "Illustrative binary outcome",
  symptom_text = "Synthetic symptom note",
  site = "Collection site"
)
types <- c(
  source_person_id = "text",
  sex = "categorical",
  cohort_group = "categorical",
  baseline_age = "integer",
  visit_number = "integer",
  visit_date = "date",
  systolic_bp = "numeric",
  outcome = "binary",
  symptom_text = "text",
  site = "categorical"
)
roles <- c(
  source_person_id = "identifier",
  sex = "covariate",
  cohort_group = "exposure",
  baseline_age = "covariate",
  visit_number = "time",
  visit_date = "time",
  systolic_bp = "outcome",
  outcome = "outcome",
  symptom_text = "outcome",
  site = "covariate"
)

dictionary$label <- unname(labels[dictionary$source_column])
dictionary$type <- unname(types[dictionary$source_column])
dictionary$role <- unname(roles[dictionary$source_column])
dictionary$description <- paste(
  dictionary$label,
  "in the neutral synthetic walkthrough."
)
dictionary$required <- !dictionary$source_column %in% c(
  "sex", "systolic_bp", "symptom_text"
)
dictionary$units[dictionary$source_column == "baseline_age"] <- "years"
dictionary$units[dictionary$source_column == "systolic_bp"] <- "mmHg"
dictionary$min[dictionary$source_column == "baseline_age"] <- "18"
dictionary$max[dictionary$source_column == "baseline_age"] <- "110"
dictionary$min[dictionary$source_column == "systolic_bp"] <- "50"
dictionary$max[dictionary$source_column == "systolic_bp"] <- "250"
dictionary$missing_codes[dictionary$source_column == "sex"] <- "Not recorded"
dictionary$missing_codes[dictionary$source_column == "symptom_text"] <- "Not recorded"

dictionary$catalog_name[dictionary$source_column == "sex"] <- "sex"
dictionary$catalog_name[dictionary$source_column == "cohort_group"] <- "cohort_group"
dictionary$catalog_name[dictionary$source_column == "outcome"] <- "outcome"
dictionary$catalog_name[dictionary$source_column == "site"] <- "site"

# Profile only explicitly selected fields in this synthetic database.
catalogue_columns <- dictionary[
  dictionary$source_column %in% c("cohort_group", "outcome"),
  c("source_schema", "source_table", "source_column")
]
catalogue_profile <- epi_db_catalogue_profile(
  con,
  dictionary,
  columns = catalogue_columns,
  max_levels = 5L
)
catalogue_profile$values
catalogue_profile$missing

# Catalogue meaning comes from the known fixture contract, not from guessing at the observed profile. PostgreSQL NULL counts are reported separately; missingness is explicit for the synthetic sex field.
catalogues <- data.frame(
  catalog_name = c(
    "sex", "sex", "sex",
    "cohort_group", "cohort_group",
    "outcome", "outcome",
    "site", "site", "site"
  ),
  source_value = c(
    "Female", "Male", "Not recorded",
    "Comparator", "Intervention",
    "0", "1",
    "Central", "North", "South"
  ),
  label = c(
    "Female", "Male", "Not recorded",
    "Comparator", "Intervention",
    "No outcome", "Outcome",
    "Central", "North", "South"
  ),
  display_order = c(1L, 2L, 3L, 1L, 2L, 1L, 2L, 1L, 2L, 3L),
  is_missing = c(
    FALSE, FALSE, TRUE,
    FALSE, FALSE,
    FALSE, FALSE,
    FALSE, FALSE, FALSE
  ),
  provenance = "declared_synthetic_walkthrough_v2",
  stringsAsFactors = FALSE
)
epi_eda_dictionary_validate(dictionary, catalogues)

# 5. Declare longitudinal linkage and initialise the registry -----------------

linkage_draft <- epi_sec_linkage_scaffold(
  dictionary,
  tables = data.frame(
    source_schema = rep(source_schema, 2L),
    source_table = c("participants", "visits"),
    stringsAsFactors = FALSE
  )
)
linkage_draft$tables

linkage_tables <- linkage_draft$tables
linkage_tables$id_column <- "source_person_id"
linkage_tables$identity_namespace <- "synthetic_person"
linkage_tables$can_enrol <- linkage_tables$source_table == "participants"
linkage_tables$one_row_per_entity <- linkage_tables$source_table == "participants"
linkage_tables$destination_table <- paste0(
  linkage_tables$source_table,
  "_pseudonymised"
)
linkage_tables$provenance <- "synthetic_walkthrough_v2"

linkage_columns <- linkage_draft$columns
linkage_columns$output_action <- "retain"
identifier_row <- linkage_columns$source_column == "source_person_id"
linkage_columns$output_action[identifier_row] <- "pseudonymise"

record_keys <- data.frame(
  source_schema = source_schema,
  source_table = "visits",
  key_column = "visit_number",
  key_order = 1L,
  stringsAsFactors = FALSE
)
linkage <- epi_sec_linkage_spec(
  tables = linkage_tables,
  columns = linkage_columns,
  record_keys = record_keys
)

registry_audit <- epi_sec_identity_registry_init(
  con,
  registry_schema = registry_schema,
  mode = "audit"
)
registry_audit
stopifnot(registry_audit$status == "initialisation_required")

registry_apply <- epi_sec_identity_registry_init(
  con,
  registry_schema = registry_schema,
  token_prefix = "W",
  n_bytes = 24L,
  mode = "apply"
)
registry_apply
stopifnot(registry_apply$status == "ready")

# 6. Audit, then explicitly apply, pseudonymisation ----------------------------

pseudonym_audit <- epi_sec_pseudonymise_db(
  con,
  dictionary = dictionary,
  linkage = linkage,
  registry_schema = registry_schema,
  output_schema = output_schema,
  catalogues = catalogues,
  mode = "audit",
  exact_duplicates = "report"
)
pseudonym_audit
pseudonym_audit$table_audit
pseudonym_audit$duplicate_audit
pseudonym_audit$issues
stopifnot(pseudonym_audit$status == "audit_complete")

pseudonymised <- epi_sec_pseudonymise_db(
  con,
  dictionary = dictionary,
  linkage = linkage,
  registry_schema = registry_schema,
  output_schema = output_schema,
  catalogues = catalogues,
  mode = "apply",
  exact_duplicates = "report",
  existing = "error"
)
pseudonymised
pseudonymised$table_audit
pseudonymised$manifest
stopifnot(pseudonymised$status == "complete")
stopifnot(all(pseudonymised$table_audit$n_invalid_id == 0))
stopifnot(all(pseudonymised$table_audit$n_unmatched == 0))

# 7. Hand the pseudonymised visit dictionary into PostgreSQL-backed EDA --------

visits_table <- "visits_pseudonymised"
visits_spec <- epi_eda_dictionary_spec(
  pseudonymised$output_dictionary,
  table = paste(output_schema, visits_table, sep = "."),
  catalogues = pseudonymised$output_catalogues
)
visits_spec

postgres_source <- epi_eda_postgres_source(
  con,
  schema = output_schema,
  relation = visits_table
)
postgres_source

# PostgreSQL-backed EDA has no audit/apply preparation mode. The reviewed pseudonymised output relation and its semantic dictionary are the handoff contract.

schema_profile <- epi_eda_check_schema(postgres_source, visits_spec)
missing_profile <- epi_eda_profile_missing(postgres_source, visits_spec)
summary_profile <- epi_eda_profile_summaries(postgres_source, visits_spec)
plot_profile <- epi_eda_profile_plots(postgres_source, visits_spec)

schema_profile
missing_profile
summary_profile$variables
summary_profile$numeric
summary_profile$categorical
categorical_display <- epi_eda_categorical_display(summary_profile)
categorical_display
plot_profile$systolic_bp

output_root <- Sys.getenv(
  "EPISCOUT_WALKTHROUGH_OUTPUT",
  unset = file.path(
    getwd(),
    paste0("episcout-walkthrough-output-", run_id)
  )
)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

delivery_dir <- file.path(
  output_root,
  paste0(format(Sys.Date(), "%Y%m%d"), "_eda_cycle")
)
database_bundle <- epi_eda_db_run(
  postgres_source,
  visits_spec,
  output_dir = delivery_dir,
  plots = TRUE,
  max_plot_levels = 10L,
  maps = FALSE,
  layout = "delivery",
  quiet = TRUE
)
database_bundle$status
database_bundle$manifest
database_bundle$identifier_qa
frequency_companion_paths <- database_bundle$manifest$path[
  database_bundle$manifest$type == "plot_data" &
    grepl("-frequency\\.csv$", database_bundle$manifest$path)
]
frequency_companions <- lapply(
  file.path(delivery_dir, frequency_companion_paths),
  read.csv,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
frequency_companions
database_report <- file.path(delivery_dir, "reports", "eda-report.html")
stopifnot(file.exists(database_report))

# episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared. With maps disabled, the delivery contains aggregate CSV and plot inputs rather than source rows, raw text examples, identifiers, map coordinates or thematic row values. The HTML renderer ran after the repeatable-read snapshot closed and consumed only the validated bundle; it did not receive this connection or source observations. Aggregate outputs and pseudonymised observations still require disclosure and restricted-access review.

# 8. Reconcile outputs, disconnect and optionally remove disposable schemas ----

stopifnot(identical(
  names(database_bundle$manifest),
  c("artifact", "type", "path", "status", "checksum_md5")
))
stopifnot(database_bundle$metadata$status == "complete")
stopifnot(database_bundle$status == "complete")
stopifnot(identical(database_bundle$metadata$maps, FALSE))

cat("\nWalkthrough schemas:\n", paste(walkthrough_schemas, collapse = "\n"), "\n")
cat("\nWalkthrough outputs:\n", normalizePath(output_root), "\n")
cat("\nOpen the EDA report:\n", normalizePath(database_report), "\n")

cleanup_schemas <- identical(
  Sys.getenv("EPISCOUT_WALKTHROUGH_CLEANUP", unset = "0"),
  "1"
)
if (cleanup_schemas) {
  for (schema in rev(walkthrough_schemas)) {
    quoted_schema <- as.character(DBI::dbQuoteIdentifier(con, schema))
    DBI::dbExecute(
      con,
      paste("DROP SCHEMA", quoted_schema, "CASCADE")
    )
  }
}

DBI::dbDisconnect(con)

cat("\nComplete. Open the report, retain the complete manifest-owned delivery root and review it before sharing.\n")
