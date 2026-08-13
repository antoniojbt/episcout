library(episcout)
library(testthat)

context("live PostgreSQL longitudinal pseudonymisation")

pg_pseudonym_connection <- function() {
  required <- c("PGHOST", "PGDATABASE", "PGUSER")
  configured <- Sys.getenv(required, unset = "")
  skip_if(
    any(!nzchar(configured)),
    paste("Set", paste(required, collapse = ", "), "to run live PostgreSQL tests.")
  )

  arguments <- list(
    drv = RPostgres::Postgres(),
    host = configured[["PGHOST"]],
    dbname = configured[["PGDATABASE"]],
    user = configured[["PGUSER"]]
  )
  port <- Sys.getenv("PGPORT", unset = "")
  password <- Sys.getenv("PGPASSWORD", unset = "")
  if (nzchar(port)) arguments$port <- as.integer(port)
  if (nzchar(password)) arguments$password <- password
  do.call(DBI::dbConnect, arguments)
}

pg_pseudonym_suffix <- function() {
  suffix <- paste(sprintf("%02x", as.integer(openssl::rand_bytes(8L))), collapse = "")
  stopifnot(grepl("^[a-f0-9]{16}$", suffix))
  suffix
}

pg_pseudonym_schema <- function(prefix, suffix) {
  schema <- paste(prefix, suffix, sep = "_")
  stopifnot(
    prefix %in% c("source_data", "identity_registry", "analysis_data", "identity_data"),
    grepl("^[a-z][a-z0-9_]{1,62}$", schema),
    nchar(schema, type = "bytes") <= 63L
  )
  schema
}

pg_pseudonym_quote <- function(connection, schema, table = NULL) {
  identifier <- if (is.null(table)) schema else DBI::Id(schema = schema, table = table)
  as.character(DBI::dbQuoteIdentifier(connection, identifier))
}

pg_advisory_lock_count <- function(connection) {
  DBI::dbGetQuery(
    connection,
    paste(
      "SELECT COUNT(*)::integer AS n FROM pg_locks",
      "WHERE pid = pg_backend_pid() AND locktype = 'advisory' AND granted"
    )
  )$n[[1]]
}

pg_capture_conditions <- function(expr) {
  conditions <- character()
  value <- withCallingHandlers(
    force(expr),
    warning = function(condition) {
      conditions <<- c(conditions, conditionMessage(condition))
      invokeRestart("muffleWarning")
    },
    message = function(condition) {
      conditions <<- c(conditions, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, conditions = conditions)
}

test_that("session lock cleanup retains only unlock errors", {
  attempts <- character()
  failed <- with_mocked_bindings(
    sec_release_session_locks(NULL, c("released", "error", "not_owned")),
    dbGetQuery = function(con, statement, params) {
      attempts <<- c(attempts, params[[1]])
      if (identical(params[[1]], "error")) stop("simulated unlock error")
      data.frame(released = !identical(params[[1]], "not_owned"))
    },
    .package = "DBI"
  )

  expect_identical(attempts, c("not_owned", "error", "released"))
  expect_identical(failed, "error")
})

pg_pseudonym_dictionary <- function(connection, source_schema) {
  inventory <- epi_db_inventory(
    connection,
    source_schema,
    tables = c("entities", "events"),
    row_counts = "none"
  )
  dictionary <- epi_eda_dictionary_scaffold(inventory)
  dictionary$analysis_type[dictionary$source_column == "entity_kind"] <- "categorical"
  dictionary$catalog_name[dictionary$source_column == "entity_kind"] <- "entity_kinds"
  dictionary$provenance <- "reviewed_synthetic_fixture"
  dictionary
}

pg_pseudonym_columns <- function(dictionary, id_columns) {
  identifier <- dictionary$source_column %in% id_columns
  data.frame(
    dictionary[c("source_schema", "source_table", "source_column")],
    output_action = ifelse(identifier, "pseudonymise", "retain"),
    stringsAsFactors = FALSE
  )
}

pg_pseudonym_catalogues <- function() {
  data.frame(
    catalog_name = c("entity_kinds", "entity_kinds", "unused_kinds"),
    source_value = c("kind-a", "kind-b", "kind-z"),
    label = c("Kind A", "Kind B", "Unused kind"),
    display_order = c(1L, 2L, 1L),
    is_missing = c(FALSE, FALSE, FALSE),
    provenance = rep("reviewed_synthetic_fixture", 3L),
    stringsAsFactors = FALSE
  )
}

pg_family_dictionary <- function(connection, source_schema, tables, id_columns) {
  inventory <- epi_db_inventory(
    connection,
    source_schema,
    tables = tables,
    row_counts = "none"
  )
  dictionary <- epi_eda_dictionary_scaffold(inventory)
  dictionary$provenance <- "reviewed_runtime_fixture"
  dictionary
}

pg_runtime_uuid <- function() {
  hexadecimal <- paste(
    sprintf("%02x", as.integer(openssl::rand_bytes(16L))),
    collapse = ""
  )
  paste(
    substr(hexadecimal, 1L, 8L),
    substr(hexadecimal, 9L, 12L),
    substr(hexadecimal, 13L, 16L),
    substr(hexadecimal, 17L, 20L),
    substr(hexadecimal, 21L, 32L),
    sep = "-"
  )
}

test_that("live PostgreSQL workflow is stable, neutral, and atomic", {
  skip_if_not(identical(Sys.getenv("EPISCOUT_TEST_POSTGRES", unset = ""), "1"))
  skip_if_not_installed("RPostgres")

  connection <- pg_pseudonym_connection()

  suffix <- pg_pseudonym_suffix()
  source_ids <- setNames(
    paste0(
      "runtime-", suffix, "-",
      c("enrol-a", "enrol-b", "alias-a", "alias-b", "unused", "unmatched")
    ),
    c("enrol_a", "enrol_b", "alias_a", "alias_b", "unused", "unmatched")
  )
  schemas <- c(
    source = pg_pseudonym_schema("source_data", suffix),
    registry = pg_pseudonym_schema("identity_registry", suffix),
    output = pg_pseudonym_schema("analysis_data", suffix),
    crosswalk = pg_pseudonym_schema("identity_data", suffix)
  )
  on.exit(
    {
      for (schema in rev(unname(schemas))) {
        stopifnot(grepl(paste0("^(source_data|identity_registry|analysis_data|identity_data)_", suffix, "$"), schema))
        try(
          DBI::dbExecute(
            connection,
            paste("DROP SCHEMA IF EXISTS", pg_pseudonym_quote(connection, schema), "CASCADE")
          ),
          silent = TRUE
        )
      }
      if (DBI::dbIsValid(connection)) DBI::dbDisconnect(connection)
    },
    add = TRUE
  )

  for (schema in unname(schemas)) {
    DBI::dbExecute(
      connection,
      paste("CREATE SCHEMA", pg_pseudonym_quote(connection, schema))
    )
  }
  for (schema in schemas[c("registry", "output", "crosswalk")]) {
    DBI::dbExecute(
      connection,
      paste(
        "GRANT CREATE, USAGE ON SCHEMA",
        pg_pseudonym_quote(connection, schema),
        "TO PUBLIC"
      )
    )
  }
  public_registry_audit <- epi_sec_identity_registry_init(
    connection,
    schemas[["registry"]],
    mode = "audit"
  )
  expect_identical(public_registry_audit$status, "initialisation_required")
  expect_false(public_registry_audit$writes)
  expect_false("schema_restricted" %in% names(public_registry_audit))
  expect_false(DBI::dbExistsTable(
    connection,
    DBI::Id(schema = schemas[["registry"]], table = "registry_metadata")
  ))
  for (schema in schemas[c("registry", "output")]) {
    DBI::dbExecute(
      connection,
      paste(
        "ALTER DEFAULT PRIVILEGES IN SCHEMA",
        pg_pseudonym_quote(connection, schema),
        "GRANT SELECT ON TABLES TO PUBLIC"
      )
    )
  }
  on.exit(
    {
      for (schema in schemas[c("registry", "output")]) {
        try(
          DBI::dbExecute(
            connection,
            paste(
              "ALTER DEFAULT PRIVILEGES IN SCHEMA",
              pg_pseudonym_quote(connection, schema),
              "REVOKE SELECT ON TABLES FROM PUBLIC"
            )
          ),
          silent = TRUE
        )
      }
    },
    add = TRUE
  )

  entities <- pg_pseudonym_quote(connection, schemas[["source"]], "entities")
  events <- pg_pseudonym_quote(connection, schemas[["source"]], "events")
  aliases <- pg_pseudonym_quote(connection, schemas[["crosswalk"]], "entity_aliases")
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", entities,
      "(entity_code text NOT NULL, entity_kind text NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", events,
      "(event_entity_code text NOT NULL, event_sequence integer NOT NULL, event_value text NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", aliases,
      "(alias_code text NOT NULL, canonical_code text NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste("GRANT SELECT ON TABLE", aliases, "TO PUBLIC")
  )
  crosswalk_access <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT has_schema_privilege('public', $1, 'CREATE') AS can_create,",
      "has_schema_privilege('public', $1, 'USAGE') AS can_use,",
      "has_table_privilege('public', $2, 'SELECT') OR",
      "has_table_privilege('public', $2, 'INSERT') OR",
      "has_table_privilege('public', $2, 'UPDATE') OR",
      "has_table_privilege('public', $2, 'DELETE') AS table_access"
    ),
    params = list(
      schemas[["crosswalk"]],
      paste(schemas[["crosswalk"]], "entity_aliases", sep = ".")
    )
  )
  expect_true(crosswalk_access$can_create[[1]])
  expect_true(crosswalk_access$can_use[[1]])
  expect_true(crosswalk_access$table_access[[1]])
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["source"]], table = "entities"),
    data.frame(
      entity_code = c(source_ids[["enrol_a"]], source_ids[["enrol_a"]], source_ids[["enrol_b"]]),
      entity_kind = c("kind-a", "kind-a", "kind-b"),
      stringsAsFactors = FALSE
    )
  )
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["source"]], table = "events"),
    data.frame(
      event_entity_code = c(
        source_ids[["alias_a"]], source_ids[["alias_a"]],
        source_ids[["alias_a"]], source_ids[["alias_b"]]
      ),
      event_sequence = c(1L, 1L, 2L, 1L),
      event_value = c("value-a", "value-a", "value-b", "value-c"),
      stringsAsFactors = FALSE
    )
  )
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["crosswalk"]], table = "entity_aliases"),
    data.frame(
      alias_code = c(source_ids[["alias_a"]], source_ids[["alias_b"]], source_ids[["unused"]]),
      canonical_code = c(source_ids[["enrol_a"]], source_ids[["enrol_b"]], source_ids[["enrol_a"]]),
      stringsAsFactors = FALSE
    )
  )

  source_before <- list(
    entities = DBI::dbGetQuery(connection, paste("SELECT * FROM", entities, "ORDER BY entity_code, entity_kind")),
    events = DBI::dbGetQuery(connection, paste("SELECT * FROM", events, "ORDER BY event_entity_code, event_sequence, event_value"))
  )
  dictionary <- pg_pseudonym_dictionary(connection, schemas[["source"]])
  catalogues <- pg_pseudonym_catalogues()
  linkage <- epi_sec_linkage_spec(
    tables = data.frame(
      source_schema = rep(schemas[["source"]], 2L),
      source_table = c("entities", "events"),
      id_column = c("entity_code", "event_entity_code"),
      identity_namespace = c("entity_codes", "event_codes"),
      can_enrol = c(TRUE, FALSE),
      one_row_per_entity = c(FALSE, FALSE),
      destination_table = c("entities", "events"),
      provenance = rep("synthetic_fixture", 2L),
      stringsAsFactors = FALSE
    ),
    columns = pg_pseudonym_columns(
      dictionary, c("entity_code", "event_entity_code")
    ),
    record_keys = data.frame(
      source_schema = schemas[["source"]],
      source_table = "events",
      key_column = "event_sequence",
      key_order = 1L,
      stringsAsFactors = FALSE
    ),
    crosswalks = data.frame(
      crosswalk_schema = schemas[["crosswalk"]],
      crosswalk_table = "entity_aliases",
      alias_namespace = "event_codes",
      alias_id_column = "alias_code",
      canonical_namespace = "entity_codes",
      canonical_id_column = "canonical_code",
      provenance = "synthetic_fixture",
      stringsAsFactors = FALSE
    )
  )

  registry_audit <- epi_sec_identity_registry_init(
    connection,
    schemas[["registry"]],
    mode = "audit"
  )
  expect_s3_class(registry_audit, "epi_sec_registry_result")
  expect_identical(registry_audit$status, "initialisation_required")
  expect_false(registry_audit$writes)
  expect_true(all(registry_audit$objects$status == "planned"))

  public_access_before <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT has_schema_privilege('public', $1, 'CREATE') AS can_create,",
      "has_schema_privilege('public', $1, 'USAGE') AS can_use"
    ),
    params = list(schemas[["registry"]])
  )
  original_db_execute <- DBI::dbExecute
  original_db_get_query <- DBI::dbGetQuery
  observed_sql <- new.env(parent = emptyenv())
  observed_sql$statements <- character()
  registry_apply <- with_mocked_bindings(
    epi_sec_identity_registry_init(
      connection,
      schemas[["registry"]],
      mode = "apply"
    ),
    dbExecute = function(con, statement, ...) {
      observed_sql$statements <- c(observed_sql$statements, statement)
      original_db_execute(con, statement, ...)
    },
    dbGetQuery = function(con, statement, ...) {
      observed_sql$statements <- c(observed_sql$statements, statement)
      original_db_get_query(con, statement, ...)
    },
    .package = "DBI"
  )
  expect_identical(registry_apply$status, "ready")
  expect_true(registry_apply$writes)
  expect_equal(nrow(registry_apply$metadata), 1L)
  public_access <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT has_schema_privilege('public', $1, 'CREATE') AS can_create,",
      "has_schema_privilege('public', $1, 'USAGE') AS can_use"
    ),
    params = list(schemas[["registry"]])
  )
  expect_identical(public_access, public_access_before)

  registry_aliases <- pg_pseudonym_quote(connection, schemas[["registry"]], "aliases")
  registry_privilege_audit <- epi_sec_identity_registry_init(
    connection,
    schemas[["registry"]],
    mode = "audit"
  )
  expect_identical(registry_privilege_audit$status, "ready")
  expect_false(registry_privilege_audit$writes)
  expect_identical(
    registry_privilege_audit$objects$status[
      registry_privilege_audit$objects$object == "aliases"
    ],
    "present"
  )
  registry_reaudit <- epi_sec_identity_registry_init(
    connection,
    schemas[["registry"]],
    mode = "audit"
  )
  expect_identical(registry_reaudit$status, "ready")
  expect_false(registry_reaudit$writes)

  registry_tables <- paste(
    vapply(
      sec_registry_tables(),
      function(table) pg_pseudonym_quote(connection, schemas[["registry"]], table),
      character(1)
    ),
    collapse = ", "
  )
  current_user <- DBI::dbGetQuery(connection, "SELECT current_user AS role")$role[[1]]
  foreign_role <- paste0("episcout_registry_owner_", suffix)
  foreign_role_guard <- foreign_role
  on.exit(
    {
      if (nzchar(foreign_role_guard) && DBI::dbIsValid(connection)) {
        for (table in sec_registry_tables()) {
          try(
            DBI::dbExecute(
              connection,
              paste(
                "ALTER TABLE",
                pg_pseudonym_quote(connection, schemas[["registry"]], table),
                "OWNER TO",
                as.character(DBI::dbQuoteIdentifier(connection, current_user))
              )
            ),
            silent = TRUE
          )
        }
        try(
          DBI::dbExecute(
            connection,
            paste(
              "DROP ROLE IF EXISTS",
              as.character(DBI::dbQuoteIdentifier(connection, foreign_role_guard))
            )
          ),
          silent = TRUE
        )
      }
    },
    add = TRUE
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE ROLE",
      as.character(DBI::dbQuoteIdentifier(connection, foreign_role))
    )
  )
  for (table in sec_registry_tables()) {
    DBI::dbExecute(
      connection,
      paste(
        "ALTER TABLE",
        pg_pseudonym_quote(connection, schemas[["registry"]], table),
        "OWNER TO",
        as.character(DBI::dbQuoteIdentifier(connection, foreign_role))
      )
    )
  }
  DBI::dbExecute(
    connection,
    paste(
      "GRANT ALL PRIVILEGES ON TABLE",
      registry_tables,
      "TO",
      as.character(DBI::dbQuoteIdentifier(connection, current_user))
    )
  )
  registry_acl_query <- paste(
    "SELECT c.relname, c.relacl::text AS acl",
    "FROM pg_catalog.pg_class AS c",
    "JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace",
    "WHERE n.nspname = $1 AND c.relname IN (",
    paste(
      DBI::dbQuoteString(connection, sec_registry_tables()),
      collapse = ", "
    ),
    ") ORDER BY c.relname"
  )
  named_acl_before <- DBI::dbGetQuery(
    connection,
    registry_acl_query,
    params = list(schemas[["registry"]])
  )
  foreign_owned_registry <- epi_sec_identity_registry_init(
    connection,
    schemas[["registry"]],
    mode = "audit"
  )
  expect_identical(foreign_owned_registry$status, "ready")
  expect_true(all(foreign_owned_registry$objects$status == "present"))
  foreign_owned_pseudonymisation <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    catalogues = catalogues,
    mode = "audit"
  )
  expect_identical(foreign_owned_pseudonymisation$status, "audit_complete")
  expect_identical(
    DBI::dbGetQuery(
      connection,
      registry_acl_query,
      params = list(schemas[["registry"]])
    ),
    named_acl_before
  )
  for (table in sec_registry_tables()) {
    DBI::dbExecute(
      connection,
      paste(
        "ALTER TABLE",
        pg_pseudonym_quote(connection, schemas[["registry"]], table),
        "OWNER TO",
        as.character(DBI::dbQuoteIdentifier(connection, current_user))
      )
    )
  }
  DBI::dbExecute(
    connection,
    paste(
      "DROP ROLE",
      as.character(DBI::dbQuoteIdentifier(connection, foreign_role))
    )
  )
  foreign_role_guard <- ""

  incomplete_dictionary <- dictionary[
    dictionary$source_column != "event_value", ,
    drop = FALSE
  ]
  dictionary_error <- expect_error(
    epi_sec_pseudonymise_db(
      connection,
      incomplete_dictionary,
      linkage,
      schemas[["registry"]],
      schemas[["output"]],
      catalogues = catalogues,
      mode = "audit"
    ),
    "dictionary does not exactly cover"
  )
  expect_false(inherits(
    dictionary_error,
    c("epi_sec_governance", "epi_sec_blocked")
  ))

  incomplete_linkage <- linkage
  incomplete_linkage$columns <- incomplete_linkage$columns[
    incomplete_linkage$columns$source_column != "event_value", ,
    drop = FALSE
  ]
  action_error <- expect_error(
    epi_sec_pseudonymise_db(
      connection,
      dictionary,
      incomplete_linkage,
      schemas[["registry"]],
      schemas[["output"]],
      catalogues = catalogues,
      mode = "audit"
    ),
    "output actions do not exactly cover"
  )
  expect_false(inherits(
    action_error,
    c("epi_sec_governance", "epi_sec_blocked")
  ))

  incomplete_catalogues <- catalogues[
    catalogues$catalog_name != "entity_kinds", ,
    drop = FALSE
  ]
  catalogue_error <- expect_error(
    epi_sec_pseudonymise_db(
      connection,
      dictionary,
      linkage,
      schemas[["registry"]],
      schemas[["output"]],
      catalogues = incomplete_catalogues,
      mode = "audit"
    ),
    "catalogue"
  )
  expect_false(inherits(
    catalogue_error,
    c("epi_sec_governance", "epi_sec_blocked")
  ))

  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["source"]], table = "events"),
    data.frame(
      event_entity_code = source_ids[["unmatched"]],
      event_sequence = 9L,
      event_value = "unmatched-value",
      stringsAsFactors = FALSE
    )
  )
  unmatched_default <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    catalogues = catalogues,
    mode = "audit"
  )
  expect_identical(unmatched_default$status, "audit_complete")
  expect_true("unmatched_identifier" %in% unmatched_default$issues$issue_code)
  expect_true(all(unmatched_default$issues$severity %in% c("error", "warning")))
  expect_identical(
    unmatched_default$issues$severity[
      unmatched_default$issues$issue_code == "unmatched_identifier"
    ],
    "error"
  )
  expect_named(unmatched_default$issues, c(
    "issue_code", "severity", "stage", "source_schema", "source_table",
    "source_column", "n_affected", "message", "recommended_action"
  ))
  expect_false("issue_values" %in% names(unmatched_default))
  expect_false(grepl(
    source_ids[["unmatched"]],
    paste(capture.output(str(unmatched_default)), collapse = "\n"),
    fixed = TRUE
  ))

  unmatched_values <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    catalogues = catalogues,
    mode = "audit",
    include_issue_values = TRUE
  )
  expect_identical(unmatched_values$status, "audit_complete")
  expect_named(
    unmatched_values$issue_values,
    c("issue_code", "source_schema", "source_table", "source_column", "source_value")
  )
  expect_s3_class(unmatched_values$issue_values, "data.frame")
  expect_identical(class(unmatched_values$issue_values), "data.frame")
  expect_null(attr(unmatched_values$issue_values, "sensitive"))
  expect_identical(unmatched_values$issue_values$source_value, source_ids[["unmatched"]])
  expect_false(grepl(
    source_ids[["unmatched"]],
    paste(capture.output(print(unmatched_values)), collapse = "\n"),
    fixed = TRUE
  ))
  ordinary_display <- paste(
    c(
      capture.output(print(unmatched_values$issue_values)),
      capture.output(str(unmatched_values$issue_values))
    ),
    collapse = "\n"
  )
  expect_true(grepl(source_ids[["unmatched"]], ordinary_display, fixed = TRUE))
  expect_warning(
    unmatched_legacy <- epi_sec_pseudonymise_db(
      connection,
      dictionary,
      linkage,
      schemas[["registry"]],
      schemas[["output"]],
      catalogues = catalogues,
      mode = "audit",
      sensitive_issues = TRUE
    ),
    "deprecated.*ordinary data"
  )
  expect_identical(unmatched_legacy$issue_values, unmatched_legacy$sensitive_issues)
  expect_warning(
    unmatched_legacy_false <- epi_sec_pseudonymise_db(
      connection,
      dictionary,
      linkage,
      schemas[["registry"]],
      schemas[["output"]],
      catalogues = catalogues,
      mode = "audit",
      sensitive_issues = FALSE
    ),
    "deprecated.*ordinary data"
  )
  expect_false("issue_values" %in% names(unmatched_legacy_false))
  expect_false("sensitive_issues" %in% names(unmatched_legacy_false))
  expect_error(
    suppressWarnings(epi_sec_pseudonymise_db(
      connection,
      dictionary,
      linkage,
      schemas[["registry"]],
      schemas[["output"]],
      catalogues = catalogues,
      mode = "audit",
      sensitive_issues = TRUE,
      include_issue_values = FALSE
    )),
    "must not conflict"
  )
  DBI::dbExecute(
    connection,
    paste("DELETE FROM", events, "WHERE event_entity_code = $1"),
    params = list(source_ids[["unmatched"]])
  )

  legacy_linkage <- linkage
  legacy_linkage$columns$validation_status <- "confirmed"
  expect_error(
    epi_sec_pseudonymise_db(
      connection,
      dictionary,
      legacy_linkage,
      schemas[["registry"]],
      schemas[["output"]],
      catalogues = catalogues,
      mode = "audit"
    ),
    "regenerate saved or modified linkage objects"
  )
  expect_equal(DBI::dbGetQuery(
    connection,
    paste("SELECT COUNT(*)::integer AS n FROM", registry_aliases)
  )$n[[1]], 0L)
  expect_false(DBI::dbExistsTable(
    connection,
    DBI::Id(schema = schemas[["output"]], table = "entities")
  ))

  audit <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    catalogues = catalogues,
    mode = "audit"
  )
  expect_s3_class(audit, "epi_sec_pseudonymisation_result")
  expect_identical(audit$status, "audit_complete")
  expect_false(audit$metadata$writes[[1]])
  expect_false(any(audit$issues$severity == "error"))
  expect_true("record_key_not_declared" %in% audit$issues$issue_code)
  expect_equal(audit$identity_audit$n_crosswalk_rows, 3)
  expect_equal(audit$identity_audit$n_unused_crosswalk_rows, 1)
  expect_equal(
    setNames(audit$duplicate_audit$n_exact_excess, audit$duplicate_audit$source_table),
    c(entities = 1, events = 1)
  )
  expect_equal(
    DBI::dbGetQuery(
      connection,
      paste("SELECT COUNT(*)::integer AS n FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "aliases"))
    )$n[[1]],
    0L
  )
  expect_false(DBI::dbExistsTable(connection, DBI::Id(schema = schemas[["output"]], table = "entities")))
  expect_false(DBI::dbExistsTable(connection, DBI::Id(schema = schemas[["output"]], table = "events")))

  audit_values <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    catalogues = catalogues,
    mode = "audit",
    include_issue_values = TRUE
  )
  expect_identical(audit_values$status, "audit_complete")
  expect_named(
    audit_values$issue_values,
    c("issue_code", "source_schema", "source_table", "source_column", "source_value")
  )
  expect_equal(nrow(audit_values$issue_values), 0L)

  DBI::dbBegin(connection)
  expect_error(
    epi_sec_pseudonymise_db(
      connection,
      dictionary,
      linkage,
      schemas[["registry"]],
      schemas[["output"]],
      catalogues = catalogues,
      mode = "apply"
    ),
    "caller-managed transaction"
  )
  expect_equal(DBI::dbGetQuery(
    connection,
    paste("SELECT COUNT(*)::integer AS n FROM", registry_aliases)
  )$n[[1]], 0L)
  expect_false(DBI::dbExistsTable(
    connection,
    DBI::Id(schema = schemas[["output"]], table = "entities")
  ))
  DBI::dbRollback(connection)

  applied <- pg_capture_conditions(
    with_mocked_bindings(
      epi_sec_pseudonymise_db(
        connection,
        dictionary,
        linkage,
        schemas[["registry"]],
        schemas[["output"]],
        catalogues = catalogues,
        mode = "apply",
        exact_duplicates = "report"
      ),
      dbExecute = function(con, statement, ...) {
        observed_sql$statements <- c(observed_sql$statements, statement)
        original_db_execute(con, statement, ...)
      },
      dbGetQuery = function(con, statement, ...) {
        observed_sql$statements <- c(observed_sql$statements, statement)
        original_db_get_query(con, statement, ...)
      },
      .package = "DBI"
    )
  )
  applied_report <- applied$value
  expect_false(any(grepl("you don't own a lock", applied$conditions, fixed = TRUE)))
  expect_identical(pg_advisory_lock_count(connection), 0L)
  expect_identical(applied_report$status, "complete")
  expect_true(applied_report$metadata$writes[[1]])
  expect_false(any(grepl(
    "has_(schema|table)_privilege|\\b(GRANT|REVOKE)\\b",
    observed_sql$statements,
    ignore.case = TRUE,
    perl = TRUE
  )))
  expect_equal(
    setNames(applied_report$table_audit$n_output, applied_report$table_audit$source_table),
    c(entities = 3, events = 4)
  )
  expect_true(all(applied_report$manifest$status == "created"))
  expect_false("issue_values" %in% names(applied_report))
  expect_named(applied_report$manifest, c(
    "source_schema", "source_table", "output_schema", "output_table",
    "status", "output_type"
  ))
  expect_true(all(applied_report$manifest$output_type == "pseudonymised_table"))
  expect_silent(epi_eda_dictionary_validate(
    applied_report$output_dictionary,
    applied_report$output_catalogues
  ))
  expect_false(any(c(
    "privacy_class", "analytic_action", "validation_status",
    "profile_catalogue"
  ) %in% names(applied_report$output_dictionary)))
  expect_false("validation_status" %in% names(applied_report$output_catalogues))
  expect_identical(unique(applied_report$output_catalogues$catalog_name), "entity_kinds")
  output_spec <- epi_eda_dictionary_spec(
    applied_report$output_dictionary,
    table = paste(schemas[["output"]], "entities", sep = "."),
    catalogues = applied_report$output_catalogues
  )
  expect_equal(output_spec$name, c("entity_token", "entity_kind"))
  expect_equal(output_spec$levels, c("", "kind-a;kind-b"))

  output_entities <- pg_pseudonym_quote(connection, schemas[["output"]], "entities")
  output_events <- pg_pseudonym_quote(connection, schemas[["output"]], "events")
  output_public_access <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT c.relname AS output_table,",
      "has_table_privilege('public', c.oid, 'SELECT') OR",
      "has_table_privilege('public', c.oid, 'INSERT') OR",
      "has_table_privilege('public', c.oid, 'UPDATE') OR",
      "has_table_privilege('public', c.oid, 'DELETE') OR",
      "has_table_privilege('public', c.oid, 'TRUNCATE') OR",
      "has_table_privilege('public', c.oid, 'REFERENCES') OR",
      "has_table_privilege('public', c.oid, 'TRIGGER') AS public_access",
      "FROM pg_class c INNER JOIN pg_namespace n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 AND c.relname IN ('entities', 'events')",
      "ORDER BY c.relname"
    ),
    params = list(schemas[["output"]])
  )
  expect_equal(output_public_access$output_table, c("entities", "events"))
  expect_true(all(output_public_access$public_access))
  expect_identical(
    DBI::dbGetQuery(
      connection,
      paste(
        "SELECT has_schema_privilege('public', $1, 'CREATE') AS can_create,",
        "has_schema_privilege('public', $1, 'USAGE') AS can_use"
      ),
      params = list(schemas[["registry"]])
    ),
    public_access_before
  )
  expect_identical(
    DBI::dbGetQuery(
      connection,
      paste(
        "SELECT has_schema_privilege('public', $1, 'CREATE') AS can_create,",
        "has_schema_privilege('public', $1, 'USAGE') AS can_use,",
        "has_table_privilege('public', $2, 'SELECT') OR",
        "has_table_privilege('public', $2, 'INSERT') OR",
        "has_table_privilege('public', $2, 'UPDATE') OR",
        "has_table_privilege('public', $2, 'DELETE') AS table_access"
      ),
      params = list(
        schemas[["crosswalk"]],
        paste(schemas[["crosswalk"]], "entity_aliases", sep = ".")
      )
    ),
    crosswalk_access
  )

  output_columns <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT table_schema AS source_schema, table_name AS source_table,",
      "column_name AS source_column, ordinal_position AS source_ordinal,",
      "data_type AS source_data_type, udt_name AS source_udt_name,",
      "is_nullable AS source_is_nullable,",
      "character_maximum_length AS source_character_maximum_length,",
      "numeric_precision AS source_numeric_precision, numeric_scale AS source_numeric_scale",
      "FROM information_schema.columns",
      "WHERE table_schema = $1 AND table_name IN ('entities', 'events')",
      "ORDER BY table_name, ordinal_position"
    ),
    params = list(schemas[["output"]])
  )
  metadata_columns <- names(output_columns)
  output_dictionary <- applied_report$output_dictionary
  output_key <- paste(
    output_columns$source_schema,
    output_columns$source_table,
    output_columns$source_column,
    sep = "\r"
  )
  dictionary_key <- paste(
    output_dictionary$source_schema,
    output_dictionary$source_table,
    output_dictionary$source_column,
    sep = "\r"
  )
  dictionary_match <- match(output_key, dictionary_key)
  expect_false(anyNA(dictionary_match))
  expected_columns <- output_dictionary[dictionary_match, metadata_columns, drop = FALSE]
  rownames(expected_columns) <- NULL
  expect_equal(output_columns, expected_columns)
  entities_report <- DBI::dbGetQuery(
    connection,
    paste("SELECT entity_token, entity_kind FROM", output_entities, "ORDER BY entity_kind, entity_token")
  )
  events_report <- DBI::dbGetQuery(
    connection,
    paste("SELECT entity_token, event_sequence, event_value FROM", output_events, "ORDER BY event_value, event_sequence")
  )
  expect_named(entities_report, c("entity_token", "entity_kind"))
  expect_named(events_report, c("entity_token", "event_sequence", "event_value"))
  expect_false(any(unlist(entities_report) %in% unname(source_ids)))
  expect_false(any(unlist(events_report) %in% unname(source_ids)))
  expect_identical(
    unique(events_report$entity_token[events_report$event_value %in% c("value-a", "value-b")]),
    unique(entities_report$entity_token[entities_report$entity_kind == "kind-a"])
  )
  expect_identical(
    unique(events_report$entity_token[events_report$event_value == "value-c"]),
    unique(entities_report$entity_token[entities_report$entity_kind == "kind-b"])
  )
  value_free_structure <- paste(capture.output(str(applied_report)), collapse = "\n")
  expect_false(any(vapply(
    unname(source_ids),
    grepl,
    logical(1),
    x = value_free_structure,
    fixed = TRUE
  )))

  applied_drop <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    catalogues = catalogues,
    mode = "apply",
    exact_duplicates = "drop",
    existing = "replace"
  )
  expect_identical(applied_drop$status, "complete")
  expect_equal(
    setNames(applied_drop$table_audit$n_output, applied_drop$table_audit$source_table),
    c(entities = 2, events = 3)
  )
  expect_equal(
    setNames(applied_drop$table_audit$n_exact_removed, applied_drop$table_audit$source_table),
    c(entities = 1, events = 1)
  )
  entities_drop <- DBI::dbGetQuery(
    connection,
    paste("SELECT entity_token, entity_kind FROM", output_entities, "ORDER BY entity_kind")
  )
  events_drop <- DBI::dbGetQuery(
    connection,
    paste("SELECT entity_token, event_sequence, event_value FROM", output_events, "ORDER BY event_value, event_sequence")
  )
  expect_setequal(entities_drop$entity_token, unique(entities_report$entity_token))
  expect_setequal(events_drop$entity_token, unique(events_report$entity_token))
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT * FROM", entities, "ORDER BY entity_code, entity_kind")),
    source_before$entities
  )
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT * FROM", events, "ORDER BY event_entity_code, event_sequence, event_value")),
    source_before$events
  )

  rollback_conditions <- character()
  rollback_runs <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT COUNT(*)::integer AS n FROM",
      pg_pseudonym_quote(connection, schemas[["registry"]], "runs")
    )
  )$n[[1]]
  original_sec_apply_outputs <- sec_apply_outputs
  rollback_error <- expect_error(
    withCallingHandlers(
      with_mocked_bindings(
        epi_sec_pseudonymise_db(
          connection,
          dictionary,
          linkage,
          schemas[["registry"]],
          schemas[["output"]],
          catalogues = catalogues,
          mode = "apply",
          exact_duplicates = "drop",
          existing = "replace"
        ),
        sec_apply_outputs = function(...) {
          result <- original_sec_apply_outputs(...)
          stop("simulated post-output failure")
          result
        },
        .package = "episcout"
      ),
      warning = function(condition) {
        rollback_conditions <<- c(rollback_conditions, conditionMessage(condition))
        invokeRestart("muffleWarning")
      },
      message = function(condition) {
        rollback_conditions <<- c(rollback_conditions, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    ),
    "rolled back safely"
  )
  expect_false(any(grepl("you don't own a lock", rollback_conditions, fixed = TRUE)))
  expect_false(grepl("simulated post-output failure", conditionMessage(rollback_error), fixed = TRUE))
  expect_identical(pg_advisory_lock_count(connection), 0L)
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT entity_token, entity_kind FROM", output_entities, "ORDER BY entity_kind")),
    entities_drop
  )
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT entity_token, event_sequence, event_value FROM", output_events, "ORDER BY event_value, event_sequence")),
    events_drop
  )
  expect_identical(
    DBI::dbGetQuery(
      connection,
      paste(
        "SELECT COUNT(*)::integer AS n FROM",
        pg_pseudonym_quote(connection, schemas[["registry"]], "runs")
      )
    )$n[[1]],
    rollback_runs
  )

  registry_before_timeout <- list(
    aliases = DBI::dbGetQuery(
      connection,
      paste("SELECT * FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "aliases"), "ORDER BY identity_namespace, source_id")
    ),
    runs = DBI::dbGetQuery(
      connection,
      paste("SELECT * FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "runs"), "ORDER BY completed_at, run_id")
    )
  )
  output_before_timeout <- list(entities = entities_drop, events = events_drop)
  lock_connection <- pg_pseudonym_connection()
  on.exit(
    {
      if (DBI::dbIsValid(lock_connection)) {
        try(DBI::dbRollback(lock_connection), silent = TRUE)
        DBI::dbDisconnect(lock_connection)
      }
    },
    add = TRUE
  )
  DBI::dbBegin(lock_connection)
  lock_result <- DBI::dbSendQuery(
    lock_connection,
    "SELECT pg_advisory_xact_lock(hashtextextended($1, 0))"
  )
  on.exit(
    {
      if (DBI::dbIsValid(lock_result)) DBI::dbClearResult(lock_result)
    },
    add = TRUE
  )
  DBI::dbBind(
    lock_result,
    params = list(paste0("output:", schemas[["output"]], ".entities"))
  )
  invisible(DBI::dbFetch(lock_result))
  DBI::dbClearResult(lock_result)
  timed_out <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    catalogues = catalogues,
    mode = "apply",
    exact_duplicates = "drop",
    existing = "replace",
    lock_timeout = 1L
  )
  expect_identical(timed_out$status, "not_written")
  expect_false(timed_out$metadata$writes[[1]])
  expect_true("lock_timeout" %in% timed_out$issues$issue_code)
  expect_false("issue_values" %in% names(timed_out))
  expect_identical(pg_advisory_lock_count(connection), 0L)
  timed_out_print <- paste(capture.output(print(timed_out)), collapse = "\n")
  expect_false(any(vapply(
    unname(source_ids),
    grepl,
    logical(1),
    x = timed_out_print,
    fixed = TRUE
  )))
  expect_identical(
    DBI::dbGetQuery(
      connection,
      paste("SELECT * FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "aliases"), "ORDER BY identity_namespace, source_id")
    ),
    registry_before_timeout$aliases
  )
  expect_identical(
    DBI::dbGetQuery(
      connection,
      paste("SELECT * FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "runs"), "ORDER BY completed_at, run_id")
    ),
    registry_before_timeout$runs
  )
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT entity_token, entity_kind FROM", output_entities, "ORDER BY entity_kind")),
    output_before_timeout$entities
  )
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT entity_token, event_sequence, event_value FROM", output_events, "ORDER BY event_value, event_sequence")),
    output_before_timeout$events
  )
  DBI::dbRollback(lock_connection)
  DBI::dbDisconnect(lock_connection)

  registry_before_no_write <- list(
    aliases = DBI::dbGetQuery(
      connection,
      paste("SELECT * FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "aliases"), "ORDER BY identity_namespace, source_id")
    ),
    runs = DBI::dbGetQuery(
      connection,
      paste("SELECT * FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "runs"), "ORDER BY completed_at, run_id")
    )
  )
  output_before_no_write <- list(entities = entities_drop, events = events_drop)
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["source"]], table = "events"),
    data.frame(
      event_entity_code = source_ids[["alias_a"]],
      event_sequence = 1L,
      event_value = "conflicting-value",
      stringsAsFactors = FALSE
    )
  )
  source_with_conflict <- DBI::dbGetQuery(
    connection,
    paste("SELECT * FROM", events, "ORDER BY event_entity_code, event_sequence, event_value")
  )

  not_written <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    catalogues = catalogues,
    mode = "apply",
    exact_duplicates = "drop",
    existing = "replace"
  )
  expect_identical(not_written$status, "not_written")
  expect_false(not_written$metadata$writes[[1]])
  expect_true("conflicting_record_key" %in% not_written$issues$issue_code)
  expect_identical(
    DBI::dbGetQuery(
      connection,
      paste("SELECT * FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "aliases"), "ORDER BY identity_namespace, source_id")
    ),
    registry_before_no_write$aliases
  )
  expect_identical(
    DBI::dbGetQuery(
      connection,
      paste("SELECT * FROM", pg_pseudonym_quote(connection, schemas[["registry"]], "runs"), "ORDER BY completed_at, run_id")
    ),
    registry_before_no_write$runs
  )
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT entity_token, entity_kind FROM", output_entities, "ORDER BY entity_kind")),
    output_before_no_write$entities
  )
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT entity_token, event_sequence, event_value FROM", output_events, "ORDER BY event_value, event_sequence")),
    output_before_no_write$events
  )
  expect_identical(
    DBI::dbGetQuery(connection, paste("SELECT * FROM", events, "ORDER BY event_entity_code, event_sequence, event_value")),
    source_with_conflict
  )

  namespaces_table <- pg_pseudonym_quote(connection, schemas[["registry"]], "namespaces")
  type_check <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT k.conname FROM pg_constraint k",
      "INNER JOIN pg_class c ON c.oid = k.conrelid",
      "INNER JOIN pg_namespace n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 AND c.relname = 'namespaces' AND k.contype = 'c'"
    ),
    params = list(schemas[["registry"]])
  )$conname[[1]]
  DBI::dbExecute(
    connection,
    paste("ALTER TABLE", namespaces_table, "DROP CONSTRAINT", as.character(DBI::dbQuoteIdentifier(connection, type_check)))
  )
  DBI::dbExecute(
    connection,
    paste("ALTER TABLE", namespaces_table, "ADD CONSTRAINT", as.character(DBI::dbQuoteIdentifier(connection, type_check)), "CHECK (TRUE)")
  )
  malformed_registry <- epi_sec_identity_registry_init(
    connection,
    schemas[["registry"]],
    mode = "audit"
  )
  expect_identical(malformed_registry$status, "incompatible")
  expect_true(all(malformed_registry$objects$status == "incompatible_structure"))
})

test_that("PostgreSQL registry permission denials are sanitised technical errors", {
  skip_if_not(identical(Sys.getenv("EPISCOUT_TEST_POSTGRES", unset = ""), "1"))
  skip_if_not_installed("RPostgres")

  connection <- pg_pseudonym_connection()
  suffix <- pg_pseudonym_suffix()
  registry_schema <- paste0("permission_registry_", suffix)
  restricted_role <- paste0("episcout_permission_", suffix)
  stopifnot(
    grepl("^permission_registry_[a-f0-9]{16}$", registry_schema),
    grepl("^episcout_permission_[a-f0-9]{16}$", restricted_role)
  )
  on.exit(
    {
      if (DBI::dbIsValid(connection)) {
        try(DBI::dbExecute(connection, "RESET ROLE"), silent = TRUE)
        try(
          DBI::dbExecute(
            connection,
            paste(
              "DROP SCHEMA IF EXISTS",
              pg_pseudonym_quote(connection, registry_schema),
              "CASCADE"
            )
          ),
          silent = TRUE
        )
        try(
          DBI::dbExecute(
            connection,
            paste(
              "DROP ROLE IF EXISTS",
              as.character(DBI::dbQuoteIdentifier(connection, restricted_role))
            )
          ),
          silent = TRUE
        )
        DBI::dbDisconnect(connection)
      }
    },
    add = TRUE
  )

  DBI::dbExecute(
    connection,
    paste("CREATE SCHEMA", pg_pseudonym_quote(connection, registry_schema))
  )
  registry <- epi_sec_identity_registry_init(
    connection,
    registry_schema,
    mode = "apply"
  )
  expect_identical(registry$status, "ready")
  DBI::dbExecute(
    connection,
    paste(
      "CREATE ROLE",
      as.character(DBI::dbQuoteIdentifier(connection, restricted_role))
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "GRANT USAGE ON SCHEMA",
      pg_pseudonym_quote(connection, registry_schema),
      "TO",
      as.character(DBI::dbQuoteIdentifier(connection, restricted_role))
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "GRANT SELECT ON ALL TABLES IN SCHEMA",
      pg_pseudonym_quote(connection, registry_schema),
      "TO",
      as.character(DBI::dbQuoteIdentifier(connection, restricted_role))
    )
  )
  metadata_table <- pg_pseudonym_quote(
    connection,
    registry_schema,
    "registry_metadata"
  )
  DBI::dbExecute(
    connection,
    paste(
      "REVOKE SELECT ON TABLE",
      metadata_table,
      "FROM",
      as.character(DBI::dbQuoteIdentifier(connection, restricted_role))
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "GRANT UPDATE ON TABLE",
      metadata_table,
      "TO",
      as.character(DBI::dbQuoteIdentifier(connection, restricted_role))
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "SET ROLE",
      as.character(DBI::dbQuoteIdentifier(connection, restricted_role))
    )
  )

  denied <- expect_error(
    epi_sec_identity_registry_init(
      connection,
      registry_schema,
      mode = "audit"
    ),
    "Identity registry inspection or initialisation could not complete"
  )
  DBI::dbExecute(connection, "RESET ROLE")
  expect_false(grepl("permission denied", conditionMessage(denied), ignore.case = TRUE))
  expect_false(grepl(registry_schema, conditionMessage(denied), fixed = TRUE))
  expect_false(grepl("registry_metadata", conditionMessage(denied), fixed = TRUE))
})

test_that("live PostgreSQL identifier families preserve exact declared identity", {
  skip_if_not(identical(Sys.getenv("EPISCOUT_TEST_POSTGRES", unset = ""), "1"))
  skip_if_not_installed("RPostgres")

  connection <- pg_pseudonym_connection()

  suffix <- pg_pseudonym_suffix()
  schemas <- c(
    source = pg_pseudonym_schema("source_data", suffix),
    registry = pg_pseudonym_schema("identity_registry", suffix),
    output = pg_pseudonym_schema("analysis_data", suffix),
    crosswalk = pg_pseudonym_schema("identity_data", suffix)
  )
  on.exit(
    {
      for (schema in rev(unname(schemas))) {
        stopifnot(grepl(
          paste0(
            "^(source_data|identity_registry|analysis_data|identity_data)_",
            suffix,
            "$"
          ),
          schema
        ))
        try(
          DBI::dbExecute(
            connection,
            paste(
              "DROP SCHEMA IF EXISTS",
              pg_pseudonym_quote(connection, schema),
              "CASCADE"
            )
          ),
          silent = TRUE
        )
      }
      if (DBI::dbIsValid(connection)) DBI::dbDisconnect(connection)
    },
    add = TRUE
  )

  for (schema in unname(schemas)) {
    DBI::dbExecute(
      connection,
      paste("CREATE SCHEMA", pg_pseudonym_quote(connection, schema))
    )
  }
  for (schema in schemas[c("registry", "output", "crosswalk")]) {
    DBI::dbExecute(
      connection,
      paste(
        "REVOKE ALL ON SCHEMA",
        pg_pseudonym_quote(connection, schema),
        "FROM PUBLIC"
      )
    )
  }

  text_base <- paste0("id", suffix)
  family_ids <- c(
    base = text_base,
    case = toupper(text_base),
    leading_zero = paste0("0", text_base),
    leading_space = paste0(" ", text_base),
    trailing_space = paste0(text_base, " "),
    alias_a = paste0("alias-a-", suffix),
    alias_b = paste0("alias-b-", suffix),
    fixed = paste0("fixed-", suffix),
    nondeterministic = paste0("nondeterministic-", suffix)
  )
  integer_ids <- c(
    first = as.numeric(strtoi(substr(suffix, 1L, 7L), base = 16L)),
    second = as.numeric(strtoi(substr(suffix, 1L, 7L), base = 16L)) + 1
  )
  uuid_id <- pg_runtime_uuid()
  uuid_lexemes <- c(
    canonical = uuid_id,
    alternate = paste0("{", toupper(uuid_id), "}")
  )

  source_tables <- c(
    text_entities = pg_pseudonym_quote(connection, schemas[["source"]], "text_entities"),
    text_events = pg_pseudonym_quote(connection, schemas[["source"]], "text_events"),
    integer_events = pg_pseudonym_quote(connection, schemas[["source"]], "integer_events"),
    uuid_events = pg_pseudonym_quote(connection, schemas[["source"]], "uuid_events"),
    fixed_ids = pg_pseudonym_quote(connection, schemas[["source"]], "fixed_ids")
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", source_tables[["text_entities"]],
      "(entity_code text NOT NULL, variant text NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", source_tables[["text_events"]],
      "(event_code varchar(200) NOT NULL, event_no integer NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", source_tables[["integer_events"]],
      "(integer_code bigint NOT NULL, event_no integer NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", source_tables[["uuid_events"]],
      "(uuid_code uuid NOT NULL, event_no integer NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", source_tables[["fixed_ids"]],
      "(fixed_code char(80) NOT NULL, variant text NOT NULL)"
    )
  )

  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["source"]], table = "text_entities"),
    data.frame(
      entity_code = unname(family_ids[c(
        "base", "case", "leading_zero", "leading_space", "trailing_space"
      )]),
      variant = c("base", "case", "leading-zero", "leading-space", "trailing-space"),
      stringsAsFactors = FALSE
    )
  )
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["source"]], table = "text_events"),
    data.frame(
      event_code = unname(family_ids[c(
        "base", "case", "leading_zero", "leading_space", "trailing_space",
        "alias_a", "alias_b"
      )]),
      event_no = seq_len(7L),
      stringsAsFactors = FALSE
    )
  )
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["source"]], table = "integer_events"),
    data.frame(
      integer_code = unname(integer_ids[c("first", "first", "second")]),
      event_no = seq_len(3L),
      stringsAsFactors = FALSE
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO", source_tables[["uuid_events"]],
      "(uuid_code, event_no) VALUES ($1::uuid, 1), ($2::uuid, 2)"
    ),
    params = as.list(unname(uuid_lexemes))
  )
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["source"]], table = "fixed_ids"),
    data.frame(
      fixed_code = family_ids[["fixed"]],
      variant = "fixed",
      stringsAsFactors = FALSE
    )
  )

  crosswalk_tables <- c(
    text = pg_pseudonym_quote(connection, schemas[["crosswalk"]], "text_aliases"),
    integer = pg_pseudonym_quote(connection, schemas[["crosswalk"]], "integer_aliases"),
    uuid = pg_pseudonym_quote(connection, schemas[["crosswalk"]], "uuid_aliases")
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", crosswalk_tables[["text"]],
      "(alias_code varchar(200) NOT NULL, canonical_code text NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", crosswalk_tables[["integer"]],
      "(alias_code bigint NOT NULL, canonical_code text NOT NULL)"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE", crosswalk_tables[["uuid"]],
      "(alias_code uuid NOT NULL, canonical_code text NOT NULL)"
    )
  )
  for (table in unname(crosswalk_tables)) {
    DBI::dbExecute(
      connection,
      paste("REVOKE ALL ON TABLE", table, "FROM PUBLIC")
    )
  }
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["crosswalk"]], table = "text_aliases"),
    data.frame(
      alias_code = unname(family_ids[c("alias_a", "alias_b")]),
      canonical_code = rep(family_ids[["base"]], 2L),
      stringsAsFactors = FALSE
    )
  )
  DBI::dbAppendTable(
    connection,
    DBI::Id(schema = schemas[["crosswalk"]], table = "integer_aliases"),
    data.frame(
      alias_code = unname(integer_ids),
      canonical_code = unname(family_ids[c("case", "leading_zero")]),
      stringsAsFactors = FALSE
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO", crosswalk_tables[["uuid"]],
      "(alias_code, canonical_code) VALUES ($1::uuid, $2)"
    ),
    params = list(uuid_lexemes[["canonical"]], family_ids[["base"]])
  )

  dictionary <- pg_family_dictionary(
    connection,
    schemas[["source"]],
    tables = c("text_entities", "text_events", "integer_events", "uuid_events"),
    id_columns = c("entity_code", "event_code", "integer_code", "uuid_code")
  )
  linkage <- epi_sec_linkage_spec(
    tables = data.frame(
      source_schema = rep(schemas[["source"]], 4L),
      source_table = c("text_entities", "text_events", "integer_events", "uuid_events"),
      id_column = c("entity_code", "event_code", "integer_code", "uuid_code"),
      identity_namespace = c("text_codes", "text_codes", "integer_codes", "uuid_codes"),
      can_enrol = c(TRUE, FALSE, FALSE, FALSE),
      one_row_per_entity = c(TRUE, FALSE, FALSE, FALSE),
      destination_table = c("text_entities", "text_events", "integer_events", "uuid_events"),
      provenance = rep("runtime_fixture", 4L),
      stringsAsFactors = FALSE
    ),
    columns = pg_pseudonym_columns(
      dictionary, c("entity_code", "event_code", "integer_code", "uuid_code")
    ),
    record_keys = data.frame(
      source_schema = rep(schemas[["source"]], 3L),
      source_table = c("text_events", "integer_events", "uuid_events"),
      key_column = rep("event_no", 3L),
      key_order = rep(1L, 3L),
      stringsAsFactors = FALSE
    ),
    crosswalks = data.frame(
      crosswalk_schema = rep(schemas[["crosswalk"]], 3L),
      crosswalk_table = c("text_aliases", "integer_aliases", "uuid_aliases"),
      alias_namespace = c("text_codes", "integer_codes", "uuid_codes"),
      alias_id_column = rep("alias_code", 3L),
      canonical_namespace = rep("text_codes", 3L),
      canonical_id_column = rep("canonical_code", 3L),
      provenance = rep("runtime_fixture", 3L),
      stringsAsFactors = FALSE
    )
  )

  registry <- epi_sec_identity_registry_init(
    connection,
    schemas[["registry"]],
    mode = "apply"
  )
  expect_identical(registry$status, "ready")

  fixed_dictionary <- pg_family_dictionary(
    connection,
    schemas[["source"]],
    tables = "fixed_ids",
    id_columns = "fixed_code"
  )
  fixed_linkage <- epi_sec_linkage_spec(
    data.frame(
      source_schema = schemas[["source"]],
      source_table = "fixed_ids",
      id_column = "fixed_code",
      identity_namespace = "fixed_codes",
      can_enrol = TRUE,
      one_row_per_entity = TRUE,
      destination_table = "fixed_ids",
      provenance = "runtime_fixture",
      stringsAsFactors = FALSE
    ),
    pg_pseudonym_columns(fixed_dictionary, "fixed_code")
  )
  expect_error(
    epi_sec_pseudonymise_db(
      connection,
      fixed_dictionary,
      fixed_linkage,
      schemas[["registry"]],
      schemas[["output"]],
      mode = "audit"
    ),
    "unsupported PostgreSQL type"
  )

  collation <- pg_pseudonym_quote(connection, schemas[["source"]], "runtime_nondeterministic")
  collation_available <- tryCatch(
    {
      DBI::dbExecute(
        connection,
        paste(
          "CREATE COLLATION", collation,
          "(provider = icu, locale = 'und', deterministic = false)"
        )
      )
      TRUE
    },
    error = function(error) FALSE
  )
  if (collation_available) {
    nondeterministic_table <- pg_pseudonym_quote(
      connection,
      schemas[["source"]],
      "nondeterministic_ids"
    )
    DBI::dbExecute(
      connection,
      paste(
        "CREATE TABLE", nondeterministic_table,
        "(identifier text COLLATE", collation,
        "NOT NULL, variant text NOT NULL)"
      )
    )
    DBI::dbAppendTable(
      connection,
      DBI::Id(schema = schemas[["source"]], table = "nondeterministic_ids"),
      data.frame(
        identifier = family_ids[["nondeterministic"]],
        variant = "nondeterministic",
        stringsAsFactors = FALSE
      )
    )
    nondeterministic_dictionary <- pg_family_dictionary(
      connection,
      schemas[["source"]],
      tables = "nondeterministic_ids",
      id_columns = "identifier"
    )
    nondeterministic_linkage <- epi_sec_linkage_spec(
      data.frame(
        source_schema = schemas[["source"]],
        source_table = "nondeterministic_ids",
        id_column = "identifier",
        identity_namespace = "nondeterministic_codes",
        can_enrol = TRUE,
        one_row_per_entity = TRUE,
        destination_table = "nondeterministic_ids",
        provenance = "runtime_fixture",
        stringsAsFactors = FALSE
      ),
      pg_pseudonym_columns(nondeterministic_dictionary, "identifier")
    )
    expect_error(
      epi_sec_pseudonymise_db(
        connection,
        nondeterministic_dictionary,
        nondeterministic_linkage,
        schemas[["registry"]],
        schemas[["output"]],
        mode = "audit"
      ),
      "nondeterministic PostgreSQL collation"
    )
  } else {
    succeed()
  }

  audit <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    mode = "audit"
  )
  expect_identical(audit$status, "audit_complete")
  expect_false(any(audit$issues$severity == "error"))
  expect_equal(audit$identity_audit$n_crosswalk_rows, 5)

  applied <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    mode = "apply"
  )
  expect_identical(applied$status, "complete")

  output <- lapply(
    c("text_entities", "text_events", "integer_events", "uuid_events"),
    function(table) {
      DBI::dbGetQuery(
        connection,
        paste(
          "SELECT * FROM",
          pg_pseudonym_quote(connection, schemas[["output"]], table),
          "ORDER BY 2"
        )
      )
    }
  )
  names(output) <- c("text_entities", "text_events", "integer_events", "uuid_events")
  entity_tokens <- setNames(
    output$text_entities$entity_token,
    output$text_entities$variant
  )
  expect_equal(length(unique(entity_tokens)), 5L)
  expect_equal(
    length(unique(output$text_events$entity_token[output$text_events$event_no <= 5L])),
    5L
  )
  expect_identical(
    output$text_events$entity_token[output$text_events$event_no %in% c(6L, 7L)],
    rep(entity_tokens[["base"]], 2L)
  )
  expect_identical(
    output$integer_events$entity_token,
    c(entity_tokens[["case"]], entity_tokens[["case"]], entity_tokens[["leading-zero"]])
  )
  expect_identical(
    output$uuid_events$entity_token,
    rep(entity_tokens[["base"]], 2L)
  )
  normalised_uuid <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT uuid_code::text AS uuid_code FROM",
      source_tables[["uuid_events"]],
      "ORDER BY event_no"
    )
  )$uuid_code
  expect_identical(normalised_uuid, rep(uuid_lexemes[["canonical"]], 2L))

  registered_text_aliases <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT source_id, entity_token FROM",
      pg_pseudonym_quote(connection, schemas[["registry"]], "aliases"),
      "WHERE identity_namespace = $1"
    ),
    params = list("text_codes")
  )
  registered_tokens <- setNames(
    registered_text_aliases$entity_token,
    registered_text_aliases$source_id
  )
  expect_identical(
    unname(registered_tokens[unname(family_ids[c("alias_a", "alias_b")])]),
    rep(registered_tokens[[family_ids[["base"]]]], 2L)
  )

  reapplied <- epi_sec_pseudonymise_db(
    connection,
    dictionary,
    linkage,
    schemas[["registry"]],
    schemas[["output"]],
    mode = "apply",
    existing = "replace"
  )
  expect_identical(reapplied$status, "complete")
  for (table in names(output)) {
    observed <- DBI::dbGetQuery(
      connection,
      paste(
        "SELECT * FROM",
        pg_pseudonym_quote(connection, schemas[["output"]], table),
        "ORDER BY 2"
      )
    )
    expect_identical(observed, output[[table]])
  }
})
