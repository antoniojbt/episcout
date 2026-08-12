linkage_table_columns <- function() {
  c(
    "source_schema",
    "source_table",
    "id_column",
    "identity_namespace",
    "can_enrol",
    "one_row_per_entity",
    "destination_table",
    "provenance"
  )
}

linkage_legacy_table_columns <- function() {
  c(linkage_table_columns(), "validation_status")
}

linkage_record_key_columns <- function() {
  c("source_schema", "source_table", "key_column", "key_order")
}

linkage_output_action_columns <- function() {
  c("source_schema", "source_table", "source_column", "output_action")
}

linkage_legacy_policy_columns <- function() {
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
    "provenance"
  )
}

linkage_legacy_xwalk_columns <- function() {
  c(linkage_crosswalk_columns(), "validation_status")
}

#' Create a longitudinal linkage metadata scaffold
#'
#' Create editable, value-free linkage and output-action metadata from a reusable semantic database dictionary. The function reads dictionary metadata only; it never reads source rows or identifier values. The caller declares identity columns, namespaces, the enrolment source, row grain, record keys, provenance and output actions.
#'
#' @param dictionary An extended EDA dictionary data frame or a path to a CSV file containing one.
#' @param tables Optional data frame or CSV path containing only `source_schema` and `source_table`, used to select a subset of active dictionary tables. `NULL` selects every active table.
#'
#' @return An `epi_sec_linkage_scaffold` list containing editable `tables`,
#'   `columns`, `record_keys` and `crosswalks` data frames.
#'
#' @details The `tables` component contains `source_schema`, `source_table`, `id_column`, `identity_namespace`, `can_enrol`, `one_row_per_entity`, `destination_table` and `provenance`. `columns` covers every active selected dictionary column and adds a blank `output_action`; set every action to `pseudonymise`, `retain` or `drop`. `record_keys` and `crosswalks` initially have no rows. Complete the scaffold outside this function and pass all four components to [epi_sec_linkage_spec()].
#'
#' This function reads dictionary metadata only and performs no database or file writes. It does not inspect values, infer identity matches, classify columns or decide whether an output may be used or disclosed. See `vignette("longitudinal-pseudonymisation")` for a worked technical workflow.
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
  action_rows <- active[
    linkage_source_key(active) %in% selected_key,
    dictionary_key_columns(),
    drop = FALSE
  ]
  actions <- empty_linkage_columns(nrow(action_rows))
  if (nrow(action_rows) > 0L) {
    actions[dictionary_key_columns()] <- action_rows
  }

  structure(
    list(
      tables = draft,
      columns = actions,
      record_keys = empty_linkage_record_keys(),
      crosswalks = empty_linkage_crosswalks()
    ),
    class = c("epi_sec_linkage_scaffold", "list")
  )
}

#' Validate longitudinal linkage metadata
#'
#' Read and validate a metadata-only linkage contract for PostgreSQL pseudonymisation. Each source table declares exactly one identifier column, exactly one table may enrol previously unseen entities and every selected source column has an explicit output action. Crosswalk arguments describe a PostgreSQL relation and its columns; they never contain identifier values.
#'
#' @param tables A data frame or CSV path with `source_schema`, `source_table`, `id_column`, `identity_namespace`, `can_enrol`, `one_row_per_entity`, `destination_table` and `provenance`.
#' @param columns A data frame or CSV path with exact source-column keys and `output_action`, which must be `pseudonymise`, `retain` or `drop`.
#' @param record_keys Optional data frame or CSV path with `source_schema`, `source_table`, `key_column` and `key_order`. The generated entity token is implicitly prepended to each declared record key.
#' @param crosswalks Optional data frame or CSV path with `crosswalk_schema`, `crosswalk_table`, `alias_namespace`, `alias_id_column`, `canonical_namespace`, `canonical_id_column` and `provenance`. These fields describe a database relation; identifier values are not accepted.
#'
#' @return A validated, normalised `epi_sec_linkage_spec` list containing
#'   `tables`, `columns`, `record_keys` and `crosswalks` data frames.
#'
#' @details Exactly one row in `tables` must set `can_enrol = TRUE`. Set `one_row_per_entity = TRUE` only when the requested output has one record per entity. Otherwise, declare ordered record-key columns or deliberately leave that table without a record key. Exactly the declared `id_column` must use `output_action = "pseudonymise"`; record-key columns must use `retain`. Crosswalk rows name PostgreSQL relations and columns; source identifier values must not be placed in this portable specification.
#'
#' For one development cycle, exact legacy component schemas are accepted with one deprecation warning. Legacy `validation_status` and `privacy_class` values are ignored; executable actions map from `bridge`, `retain`, `retain_restricted` and `drop`. `review` and `derive` cannot be converted. Returned objects always use the new schemas.
#'
#' Validation errors indicate malformed or structurally inconsistent metadata and do not write anything. A valid specification defines a technical operation; it does not establish that identities match or decide whether an output may be used or disclosed. See `vignette("longitudinal-pseudonymisation")` for preparation, inspection and recovery guidance.
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
      "The linkage columns component is required. Rebuild older three-component linkage metadata with an explicit output_action for every selected source column.",
      call. = FALSE
    )
  }
  tables <- read_linkage_csv_or_data(tables, "tables")
  columns <- read_linkage_csv_or_data(columns, "columns")
  if (!is.null(crosswalks)) {
    crosswalks <- read_linkage_csv_or_data(crosswalks, "crosswalks")
  }
  legacy <- normalise_legacy_linkage(tables, columns, crosswalks)
  tables <- legacy$tables
  columns <- legacy$columns
  crosswalks <- legacy$crosswalks
  if (legacy$used) {
    warning(
      "Legacy linkage confirmation/privacy fields are deprecated and ignored; use the neutral tables, columns output_action and crosswalk schemas.",
      call. = FALSE
    )
  }
  tables <- validate_linkage_tables(tables)
  columns <- validate_linkage_actions(columns, tables)
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

normalise_legacy_linkage <- function(tables, columns, crosswalks) {
  used <- FALSE
  if (linkage_has_exact_columns(tables, linkage_legacy_table_columns())) {
    tables <- tables[linkage_table_columns()]
    used <- TRUE
  }
  if (linkage_has_exact_columns(columns, linkage_legacy_policy_columns())) {
    legacy_actions <- as.character(columns$analytic_action)
    supported <- c("bridge", "retain", "retain_restricted", "drop")
    if (anyNA(legacy_actions) || any(!(legacy_actions %in% supported))) {
      stop(
        "Legacy columns analytic_action must be bridge, retain, retain_restricted or drop; review, derive and other actions cannot be converted.",
        call. = FALSE
      )
    }
    output_action <- c(
      bridge = "pseudonymise",
      retain = "retain",
      retain_restricted = "retain",
      drop = "drop"
    )[legacy_actions]
    columns <- data.frame(
      columns[c("source_schema", "source_table", "source_column")],
      output_action = unname(output_action),
      stringsAsFactors = FALSE
    )
    used <- TRUE
  }
  if (!is.null(crosswalks) &&
        linkage_has_exact_columns(crosswalks, linkage_legacy_xwalk_columns())) {
    crosswalks <- crosswalks[linkage_crosswalk_columns()]
    used <- TRUE
  }
  list(tables = tables, columns = columns, crosswalks = crosswalks, used = used)
}

linkage_has_exact_columns <- function(data, expected) {
  length(names(data)) == length(expected) && setequal(names(data), expected)
}

#' @export
print.epi_sec_linkage_scaffold <- function(x, ...) {
  cat("<epi_sec_linkage_scaffold>\n")
  cat("  Source tables:", nrow(x$tables), "\n")
  cat("  Output actions to set:", nrow(x$columns), "\n")
  cat("  Record-key columns:", nrow(x$record_keys), "\n")
  cat("  Crosswalk relations:", nrow(x$crosswalks), "\n")
  cat("  Next: complete every field, then call epi_sec_linkage_spec().\n")
  invisible(x)
}

#' @export
print.epi_sec_linkage_spec <- function(x, ...) {
  cat("<epi_sec_linkage_spec>\n")
  cat("  Metadata for", nrow(x$tables), "source table(s).\n")
  cat("  Enrolment tables:", sum(x$tables$can_enrol), "\n")
  cat("  Output actions:", nrow(x$columns), "\n")
  cat("  Record-key columns:", nrow(x$record_keys), "\n")
  cat("  Crosswalk relations:", nrow(x$crosswalks), "\n")
  cat("  Identifier values: not present\n")
  cat("  Next: inspect or apply the declared pseudonymisation operation.\n")
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
  if (anyDuplicated(linkage_source_key(tables))) {
    stop("tables source_schema and source_table pairs must be unique.", call. = FALSE)
  }
  if (anyDuplicated(tables$destination_table)) {
    stop("tables destination_table values must be unique.", call. = FALSE)
  }

  rownames(tables) <- NULL
  tables
}

validate_linkage_actions <- function(columns, tables) {
  validate_linkage_columns(
    columns, linkage_output_action_columns(), "columns"
  )
  columns <- columns[linkage_output_action_columns()]
  columns <- normalise_linkage_char_cols(
    columns, linkage_output_action_columns(), "columns"
  )
  if (nrow(columns) == 0L) {
    stop("columns must contain output actions for every declared source table.", call. = FALSE)
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
  if (any(!columns$output_action %in% c("pseudonymise", "retain", "drop"))) {
    stop("columns output_action must be 'pseudonymise', 'retain' or 'drop'.", call. = FALSE)
  }
  for (index in seq_len(nrow(tables))) {
    rows <- columns[
      linkage_source_key(columns) == linkage_source_key(tables[index, , drop = FALSE]), ,
      drop = FALSE
    ]
    pseudonymised <- rows$output_action == "pseudonymise"
    if (sum(pseudonymised) != 1L || rows$source_column[pseudonymised][[1]] != tables$id_column[[index]]) {
      stop("Each table id_column must match exactly one output_action = 'pseudonymise' row, and no other column may use that action.", call. = FALSE)
    }
  }
  columns$.table_order <- table_match
  columns <- columns[
    order(columns$.table_order, seq_len(nrow(columns))),
    linkage_output_action_columns(),
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
  action_key <- paste(
    columns$source_schema, columns$source_table, columns$source_column,
    sep = "\r"
  )
  key_action <- match(
    paste(
      record_keys$source_schema, record_keys$source_table,
      record_keys$key_column,
      sep = "\r"
    ),
    action_key
  )
  if (anyNA(key_action) ||
        any(columns$output_action[key_action] != "retain")) {
    stop("Every record-key column must use output_action = 'retain'.", call. = FALSE)
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
    stringsAsFactors = FALSE
  )
}

empty_linkage_columns <- function(n = 0L) {
  data.frame(
    source_schema = rep("", n),
    source_table = rep("", n),
    source_column = rep("", n),
    output_action = rep("", n),
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
    stringsAsFactors = FALSE
  )
}
