linkage_table_columns <- function() {
  c(
    "source_schema",
    "source_table",
    "id_column",
    "identity_namespace",
    "can_enrol",
    "one_row_per_entity",
    "destination_table",
    "provenance",
    "validation_status"
  )
}

linkage_record_key_columns <- function() {
  c("source_schema", "source_table", "key_column", "key_order")
}

linkage_column_policy_columns <- function() {
  c(
    "source_schema", "source_table", "source_column",
    "privacy_class", "analytic_action", "validation_status"
  )
}

linkage_crosswalk_columns <- function() {
  c(
    "crosswalk_schema",
    "crosswalk_table",
    "alias_namespace",
    "alias_id_column",
    "canonical_namespace",
    "canonical_id_column",
    "provenance",
    "validation_status"
  )
}

#' Create a longitudinal linkage metadata scaffold
#'
#' Create editable, value-free linkage and column-policy metadata from a reusable semantic database dictionary. The function reads dictionary metadata only; it never reads source rows or identifier values. Identity columns, namespaces, enrolment permission, row grain, record keys, provenance and validation states remain explicitly unreviewed.
#'
#' @param dictionary An extended EDA dictionary data frame or a path to a CSV file containing one.
#' @param tables Optional data frame or CSV path containing only `source_schema` and `source_table`, used to select a subset of active dictionary tables. `NULL` selects every active table.
#'
#' @return An `epi_sec_linkage_scaffold` list containing editable `tables`,
#'   `columns`, `record_keys` and `crosswalks` data frames.
#'
#' @details The `tables` component contains `source_schema`, `source_table`, `id_column`, `identity_namespace`, `can_enrol`, `one_row_per_entity`, `destination_table`, `provenance` and `validation_status`. `columns` covers every active selected dictionary column and adds `privacy_class`, `analytic_action` and `validation_status`, initially set to `unclassified`, `review` and `unreviewed`. `record_keys` and `crosswalks` initially have no rows. Complete the scaffold outside this function and pass all four components to [epi_sec_linkage_spec()].
#'
#' This function reads dictionary metadata only and performs no database or file writes. It does not inspect values, infer identity matches or detect personal information. See `vignette("longitudinal-pseudonymisation")` for the friendly audit-first workflow.
#'
#' @seealso [epi_sec_linkage_spec()], [epi_sec_identity_registry_init()], [epi_sec_pseudonymise_db()], [epi_eda_dictionary_scaffold()]
#' @family longitudinal pseudonymisation
#' @export
epi_sec_linkage_scaffold <- function(dictionary, tables = NULL) {
  dictionary <- read_linkage_csv_or_data(dictionary, "dictionary")
  validate_dictionary_shape(dictionary)
  validate_dictionary_values(dictionary)

  active <- dictionary[dictionary$drift_status != "removed", , drop = FALSE]
  available <- unique(active[c("source_schema", "source_table")])
  available <- available[order(available$source_schema, available$source_table), , drop = FALSE]
  rownames(available) <- NULL

  if (!is.null(tables)) {
    selected <- read_linkage_csv_or_data(tables, "tables")
    validate_linkage_columns(
      selected,
      c("source_schema", "source_table"),
      "tables"
    )
    selected <- normalise_linkage_char_cols(
      selected,
      c("source_schema", "source_table"),
      "tables"
    )
    if (nrow(selected) == 0L) {
      stop("tables must select at least one source table.", call. = FALSE)
    }
    selected_key <- linkage_source_key(selected)
    if (anyDuplicated(selected_key)) {
      stop("tables must not contain duplicate source_schema and source_table pairs.", call. = FALSE)
    }
    missing <- !(selected_key %in% linkage_source_key(available))
    if (any(missing)) {
      stop("Every selected table must be present and active in dictionary.", call. = FALSE)
    }
    available <- available[match(selected_key, linkage_source_key(available)), , drop = FALSE]
  }

  draft <- empty_linkage_tables(nrow(available))
  if (nrow(available) > 0L) {
    draft$source_schema <- as.character(available$source_schema)
    draft$source_table <- as.character(available$source_table)
    draft$destination_table <- as.character(available$source_table)

  }

  selected_key <- linkage_source_key(available)
  policy_rows <- active[
    linkage_source_key(active) %in% selected_key,
    dictionary_key_columns(),
    drop = FALSE
  ]
  policy <- empty_linkage_columns(nrow(policy_rows))
  if (nrow(policy_rows) > 0L) {
    policy[dictionary_key_columns()] <- policy_rows
  }

  structure(
    list(
      tables = draft,
      columns = policy,
      record_keys = empty_linkage_record_keys(),
      crosswalks = empty_linkage_crosswalks()
    ),
    class = c("epi_sec_linkage_scaffold", "list")
  )
}

#' Validate reviewed longitudinal linkage metadata
#'
#' Read and validate a metadata-only linkage contract for PostgreSQL pseudonymisation. Each source table declares exactly one identifier column, exactly one table may enrol previously unseen entities, and every review status must be `confirmed`. Crosswalk arguments describe a restricted PostgreSQL relation and its columns; they never contain identifier values.
#'
#' @param tables A data frame or CSV path with `source_schema`, `source_table`, `id_column`, `identity_namespace`, `can_enrol`, `one_row_per_entity`, `destination_table`, `provenance` and `validation_status`.
#' @param columns A data frame or CSV path with exact source-column keys,
#'   `privacy_class`, `analytic_action` and `validation_status`.
#' @param record_keys Optional data frame or CSV path with `source_schema`, `source_table`, `key_column` and `key_order`. The generated entity token is implicitly prepended to each declared record key.
#' @param crosswalks Optional data frame or CSV path with `crosswalk_schema`, `crosswalk_table`, `alias_namespace`, `alias_id_column`, `canonical_namespace`, `canonical_id_column`, `provenance` and `validation_status`. These fields describe a database relation; identifier values are not accepted.
#'
#' @return A validated, normalised `epi_sec_linkage_spec` list containing
#'   `tables`, `columns`, `record_keys` and `crosswalks` data frames.
#'
#' @details Exactly one row in `tables` must set `can_enrol = TRUE`. Set `one_row_per_entity = TRUE` only when one output record per entity is a reviewed rule. Otherwise, declare ordered record-key columns or deliberately leave that table without a record key. Crosswalk rows name restricted PostgreSQL relations and columns; source identifier values must not be placed in this portable specification.
#'
#' Validation errors indicate malformed or unconfirmed metadata and do not write anything. A valid specification is permission to audit the declared linkage, not evidence that identities match or that output is safe to disclose. See `vignette("longitudinal-pseudonymisation")` for preparation, audit, blocker and recovery guidance.
#'
#' @seealso [epi_sec_linkage_scaffold()], [epi_sec_identity_registry_init()], [epi_sec_pseudonymise_db()]
#' @family longitudinal pseudonymisation
#' @export
epi_sec_linkage_spec <- function(tables,
                                 columns = NULL,
                                 record_keys = NULL,
                                 crosswalks = NULL) {
  if (is.null(columns)) {
    stop(
      "The linkage columns component is required. Move privacy_class, analytic_action and validation_status from the old combined dictionary into epi_sec_linkage_spec(columns = ...).",
      call. = FALSE
    )
  }
  tables <- read_linkage_csv_or_data(tables, "tables")
  tables <- validate_linkage_tables(tables)
  columns <- validate_linkage_column_policy(columns, tables)
  record_keys <- validate_linkage_record_keys(record_keys, tables, columns)
  crosswalks <- validate_linkage_crosswalks(crosswalks, tables)

  structure(
    list(
      tables = tables,
      columns = columns,
      record_keys = record_keys,
      crosswalks = crosswalks
    ),
    class = c("epi_sec_linkage_spec", "list")
  )
}

#' @export
print.epi_sec_linkage_scaffold <- function(x, ...) {
  cat("<epi_sec_linkage_scaffold>\n")
  cat("  Review required for", nrow(x$tables), "source table(s).\n")
  cat("  Column policies:", nrow(x$columns), "\n")
  cat("  Record-key columns:", nrow(x$record_keys), "\n")
  cat("  Crosswalk relations:", nrow(x$crosswalks), "\n")
  cat("  Next: complete every field, then call epi_sec_linkage_spec().\n")
  invisible(x)
}

#' @export
print.epi_sec_linkage_spec <- function(x, ...) {
  cat("<epi_sec_linkage_spec>\n")
  cat("  Confirmed metadata for", nrow(x$tables), "source table(s).\n")
  cat("  Enrolment tables:", sum(x$tables$can_enrol), "\n")
  cat("  Confirmed column policies:", nrow(x$columns), "\n")
  cat("  Record-key columns:", nrow(x$record_keys), "\n")
  cat("  Crosswalk relations:", nrow(x$crosswalks), "\n")
  cat("  Identifier values: not present\n")
  cat("  Next: audit the pseudonymisation workflow before applying writes.\n")
  invisible(x)
}

validate_linkage_tables <- function(tables) {
  validate_linkage_columns(tables, linkage_table_columns(), "tables")
  if (nrow(tables) == 0L) {
    stop("tables must contain at least one source table.", call. = FALSE)
  }
  tables <- tables[linkage_table_columns()]

  character_columns <- setdiff(
    linkage_table_columns(),
    c("can_enrol", "one_row_per_entity")
  )
  tables <- normalise_linkage_char_cols(tables, character_columns, "tables")
  tables$can_enrol <- parse_linkage_logical(tables$can_enrol, "tables can_enrol")
  tables$one_row_per_entity <- parse_linkage_logical(
    tables$one_row_per_entity,
    "tables one_row_per_entity"
  )

  if (sum(tables$can_enrol) != 1L) {
    stop("tables must declare exactly one can_enrol = TRUE row.", call. = FALSE)
  }
  if (any(tables$validation_status != "confirmed")) {
    stop("Every tables validation_status must be 'confirmed'.", call. = FALSE)
  }
  if (anyDuplicated(linkage_source_key(tables))) {
    stop("tables source_schema and source_table pairs must be unique.", call. = FALSE)
  }
  if (anyDuplicated(tables$destination_table)) {
    stop("tables destination_table values must be unique.", call. = FALSE)
  }

  rownames(tables) <- NULL
  tables
}

validate_linkage_column_policy <- function(columns, tables) {
  columns <- read_linkage_csv_or_data(columns, "columns")
  validate_linkage_columns(
    columns, linkage_column_policy_columns(), "columns"
  )
  columns <- columns[linkage_column_policy_columns()]
  columns <- normalise_linkage_char_cols(
    columns, linkage_column_policy_columns(), "columns"
  )
  if (nrow(columns) == 0L) {
    stop("columns must contain policy for every declared source table.", call. = FALSE)
  }
  key <- paste(
    columns$source_schema, columns$source_table, columns$source_column,
    sep = "\r"
  )
  if (anyDuplicated(key)) {
    stop("columns source keys must be unique.", call. = FALSE)
  }
  table_match <- match(linkage_source_key(columns), linkage_source_key(tables))
  if (anyNA(table_match)) {
    stop("Every columns row must refer to a declared source table.", call. = FALSE)
  }
  if (!setequal(unique(linkage_source_key(columns)), linkage_source_key(tables))) {
    stop("columns must cover every declared source table.", call. = FALSE)
  }
  allowed_privacy <- c(
    "unclassified", "direct_identifier", "quasi_identifier", "sensitive",
    "non_sensitive"
  )
  allowed_actions <- c(
    "review", "bridge", "drop", "retain_restricted", "retain", "derive"
  )
  allowed_status <- c("unreviewed", "pending", "confirmed")
  if (any(!columns$privacy_class %in% allowed_privacy)) {
    stop("columns privacy_class contains invalid values.", call. = FALSE)
  }
  if (any(!columns$analytic_action %in% allowed_actions)) {
    stop("columns analytic_action contains invalid values.", call. = FALSE)
  }
  if (any(!columns$validation_status %in% allowed_status)) {
    stop("columns validation_status contains invalid values.", call. = FALSE)
  }
  if (any(columns$validation_status != "confirmed")) {
    stop("Every columns validation_status must be 'confirmed'.", call. = FALSE)
  }
  if (any(columns$privacy_class == "unclassified") ||
        any(columns$analytic_action %in% c("review", "derive"))) {
    stop("Every columns row must have a classified supported output action.", call. = FALSE)
  }
  for (index in seq_len(nrow(tables))) {
    rows <- columns[
      linkage_source_key(columns) == linkage_source_key(tables[index, , drop = FALSE]),
      , drop = FALSE
    ]
    bridge <- rows$privacy_class == "direct_identifier" &
      rows$analytic_action == "bridge"
    if (sum(bridge) != 1L || rows$source_column[bridge][[1]] != tables$id_column[[index]]) {
      stop("Each table id_column must match exactly one direct_identifier bridge policy.", call. = FALSE)
    }
    other_direct <- rows$privacy_class == "direct_identifier" & !bridge
    if (any(rows$analytic_action[other_direct] != "drop")) {
      stop("Every additional direct identifier must use analytic_action = 'drop'.", call. = FALSE)
    }
    if (any(rows$analytic_action == "bridge" & !bridge)) {
      stop("Only the declared direct identifier may use analytic_action = 'bridge'.", call. = FALSE)
    }
  }
  columns$.table_order <- table_match
  columns <- columns[
    order(columns$.table_order, seq_len(nrow(columns))),
    linkage_column_policy_columns(),
    drop = FALSE
  ]
  rownames(columns) <- NULL
  columns
}

validate_linkage_record_keys <- function(record_keys, tables, columns) {
  if (is.null(record_keys)) {
    return(empty_linkage_record_keys())
  }
  record_keys <- read_linkage_csv_or_data(record_keys, "record_keys")
  validate_linkage_columns(
    record_keys,
    linkage_record_key_columns(),
    "record_keys"
  )
  record_keys <- record_keys[linkage_record_key_columns()]
  record_keys <- normalise_linkage_char_cols(
    record_keys,
    c("source_schema", "source_table", "key_column"),
    "record_keys"
  )
  record_keys$key_order <- parse_linkage_positive_integer(
    record_keys$key_order,
    "record_keys key_order"
  )
  if (nrow(record_keys) == 0L) {
    return(empty_linkage_record_keys())
  }

  table_match <- match(linkage_source_key(record_keys), linkage_source_key(tables))
  if (anyNA(table_match)) {
    stop("Every record_keys row must refer to a declared source table.", call. = FALSE)
  }
  if (any(tables$one_row_per_entity[table_match])) {
    stop("record_keys must not be declared for a table with one_row_per_entity = TRUE.", call. = FALSE)
  }
  if (any(record_keys$key_column == tables$id_column[table_match])) {
    stop("record_keys key_column must not repeat the table's id_column.", call. = FALSE)
  }
  policy_key <- paste(
    columns$source_schema, columns$source_table, columns$source_column,
    sep = "\r"
  )
  key_policy <- match(
    paste(
      record_keys$source_schema, record_keys$source_table,
      record_keys$key_column, sep = "\r"
    ),
    policy_key
  )
  retained <- c("retain", "retain_restricted")
  if (anyNA(key_policy) ||
        any(!columns$analytic_action[key_policy] %in% retained)) {
    stop("Every record-key column must use retain or retain_restricted policy.", call. = FALSE)
  }

  key_column_key <- paste(linkage_source_key(record_keys), record_keys$key_column, sep = "\r")
  if (anyDuplicated(key_column_key)) {
    stop("record_keys key_column values must be unique within each source table.", call. = FALSE)
  }
  key_order_key <- paste(linkage_source_key(record_keys), record_keys$key_order, sep = "\r")
  if (anyDuplicated(key_order_key)) {
    stop("record_keys key_order values must be unique within each source table.", call. = FALSE)
  }

  record_keys$.table_order <- table_match
  record_keys <- record_keys[
    order(record_keys$.table_order, record_keys$key_order),
    linkage_record_key_columns(),
    drop = FALSE
  ]
  rownames(record_keys) <- NULL
  record_keys
}

validate_linkage_crosswalks <- function(crosswalks, tables) {
  if (is.null(crosswalks)) {
    return(empty_linkage_crosswalks())
  }
  crosswalks <- read_linkage_csv_or_data(crosswalks, "crosswalks")
  validate_linkage_columns(
    crosswalks,
    linkage_crosswalk_columns(),
    "crosswalks"
  )
  crosswalks <- crosswalks[linkage_crosswalk_columns()]
  crosswalks <- normalise_linkage_char_cols(
    crosswalks,
    linkage_crosswalk_columns(),
    "crosswalks"
  )
  if (nrow(crosswalks) == 0L) {
    return(empty_linkage_crosswalks())
  }
  if (any(crosswalks$validation_status != "confirmed")) {
    stop("Every crosswalks validation_status must be 'confirmed'.", call. = FALSE)
  }

  declared_namespaces <- unique(tables$identity_namespace)
  if (any(!(crosswalks$alias_namespace %in% declared_namespaces))) {
    stop("Every crosswalks alias_namespace must be declared by a source table.", call. = FALSE)
  }
  enrolment_namespace <- tables$identity_namespace[tables$can_enrol][[1]]
  if (any(crosswalks$canonical_namespace != enrolment_namespace)) {
    stop("Every crosswalks canonical_namespace must match the enrolment table namespace.", call. = FALSE)
  }
  if (anyDuplicated(crosswalks$alias_namespace)) {
    stop("crosswalks must declare at most one relation per alias_namespace.", call. = FALSE)
  }

  crosswalks <- crosswalks[order(crosswalks$alias_namespace), , drop = FALSE]
  rownames(crosswalks) <- NULL
  crosswalks
}

read_linkage_csv_or_data <- function(value, argument) {
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    if (!file.exists(value) || dir.exists(value) || !utils::file_test("-f", value)) {
      stop(argument, " must name an existing regular CSV file.", call. = FALSE)
    }
    value <- tryCatch(
      utils::read.csv(
        value,
        check.names = FALSE,
        stringsAsFactors = FALSE,
        na.strings = character()
      ),
      error = function(error) {
        stop("Could not read ", argument, " CSV file.", call. = FALSE)
      }
    )
  } else if (is.data.frame(value)) {
    value <- as.data.frame(value, stringsAsFactors = FALSE)
  } else {
    stop(argument, " must be a data frame or a CSV path.", call. = FALSE)
  }
  value
}

validate_linkage_columns <- function(data, required, argument) {
  missing <- setdiff(required, names(data))
  unexpected <- setdiff(names(data), required)
  if (length(missing) > 0L) {
    stop(argument, " is missing required columns: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (length(unexpected) > 0L) {
    stop(
      argument,
      " contains unexpected columns; linkage metadata must not contain row values: ",
      paste(unexpected, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

normalise_linkage_char_cols <- function(data, columns, argument) {
  for (column in columns) {
    values <- as.character(data[[column]])
    if (anyNA(values) || any(trimws(values) == "")) {
      stop(argument, " column '", column, "' must contain non-empty values.", call. = FALSE)
    }
    data[[column]] <- values
  }
  data
}

parse_linkage_logical <- function(values, field) {
  if (is.logical(values)) {
    if (anyNA(values)) {
      stop(field, " must contain non-missing logical values.", call. = FALSE)
    }
    return(values)
  }

  parsed <- trimws(tolower(as.character(values)))
  output <- rep(NA, length(parsed))
  output[parsed %in% c("true", "t", "1", "yes", "y")] <- TRUE
  output[parsed %in% c("false", "f", "0", "no", "n")] <- FALSE
  if (anyNA(output)) {
    stop(field, " must contain non-missing logical values.", call. = FALSE)
  }
  output
}

parse_linkage_positive_integer <- function(values, field) {
  parsed <- suppressWarnings(as.numeric(as.character(values)))
  invalid <- is.na(parsed) | !is.finite(parsed) | parsed < 1 | parsed != floor(parsed) |
    parsed > .Machine$integer.max
  if (any(invalid)) {
    stop(field, " must contain positive whole numbers.", call. = FALSE)
  }
  as.integer(parsed)
}

linkage_source_key <- function(data) {
  paste(data$source_schema, data$source_table, sep = "\r")
}

empty_linkage_tables <- function(n = 0L) {
  data.frame(
    source_schema = rep("", n),
    source_table = rep("", n),
    id_column = rep("", n),
    identity_namespace = rep("", n),
    can_enrol = rep(NA, n),
    one_row_per_entity = rep(NA, n),
    destination_table = rep("", n),
    provenance = rep("", n),
    validation_status = rep("unreviewed", n),
    stringsAsFactors = FALSE
  )
}

empty_linkage_columns <- function(n = 0L) {
  data.frame(
    source_schema = rep("", n),
    source_table = rep("", n),
    source_column = rep("", n),
    privacy_class = rep("unclassified", n),
    analytic_action = rep("review", n),
    validation_status = rep("unreviewed", n),
    stringsAsFactors = FALSE
  )
}

empty_linkage_record_keys <- function() {
  data.frame(
    source_schema = character(),
    source_table = character(),
    key_column = character(),
    key_order = integer(),
    stringsAsFactors = FALSE
  )
}

empty_linkage_crosswalks <- function() {
  data.frame(
    crosswalk_schema = character(),
    crosswalk_table = character(),
    alias_namespace = character(),
    alias_id_column = character(),
    canonical_namespace = character(),
    canonical_id_column = character(),
    provenance = character(),
    validation_status = character(),
    stringsAsFactors = FALSE
  )
}
