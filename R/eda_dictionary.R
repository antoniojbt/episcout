dictionary_key_columns <- function() {
  c("source_schema", "source_table", "source_column")
}

dictionary_source_columns <- function() {
  c(
    dictionary_key_columns(),
    "source_ordinal",
    "source_data_type",
    "source_udt_name",
    "source_is_nullable",
    "source_character_maximum_length",
    "source_numeric_precision",
    "source_numeric_scale",
    "source_column_comment"
  )
}

dictionary_curated_columns <- function() {
  c(
    "label",
    "type",
    "role",
    "units",
    "min",
    "max",
    "missing_codes",
    "required",
    "group",
    "description",
    "catalog_name",
    "privacy_class",
    "analytic_action",
    "analytic_order",
    "provenance",
    "validation_status",
    "profile_catalogue"
  )
}

#' Create an editable data dictionary scaffold
#'
#' Convert an [epi_db_inventory()] result into a deterministic dictionary scaffold. Technical database types are mapped to the existing episcout EDA types, while semantic fields remain explicitly unreviewed.
#'
#' @param inventory An `epi_db_inventory` object.
#'
#' @return A data frame with one row per source column.
#'
#' @export
epi_eda_dictionary_scaffold <- function(inventory) {
  validate_inventory_object(inventory)
  columns <- inventory$columns
  if (nrow(columns) == 0) {
    return(empty_dictionary())
  }

  dictionary <- columns[dictionary_source_columns()]
  dictionary$label <- dictionary$source_column
  dictionary$type <- vapply(dictionary$source_data_type, dictionary_eda_type, character(1))
  dictionary$role <- ""
  dictionary$units <- ""
  dictionary$min <- ""
  dictionary$max <- ""
  dictionary$missing_codes <- ""
  dictionary$required <- TRUE
  dictionary$group <- ""
  dictionary$description <- ""
  dictionary$catalog_name <- ""
  dictionary$privacy_class <- "unclassified"
  dictionary$analytic_action <- "review"
  dictionary$analytic_order <- as.integer(dictionary$source_ordinal)
  dictionary$provenance <- "database_inventory"
  dictionary$validation_status <- "unreviewed"
  dictionary$profile_catalogue <- FALSE
  dictionary$drift_status <- "current"

  order_dictionary(dictionary)
}

#' Refresh a curated dictionary from a database inventory
#'
#' Update database-owned metadata while preserving curated fields. Removed columns remain in the returned dictionary with `drift_status = "removed"`; new and technically changed columns are marked `"added"` and `"modified"`.
#'
#' @param dictionary An existing dictionary data frame.
#' @param inventory A current `epi_db_inventory` object.
#'
#' @return A refreshed dictionary data frame.
#'
#' @export
epi_eda_dictionary_refresh <- function(dictionary, inventory) {
  validate_dictionary_shape(dictionary)
  fresh <- epi_eda_dictionary_scaffold(inventory)
  if (nrow(dictionary) == 0) {
    fresh$drift_status <- "added"
    return(fresh)
  }

  old_key <- dictionary_key(dictionary)
  fresh_key <- dictionary_key(fresh)
  matched_old <- match(fresh_key, old_key)
  existing <- !is.na(matched_old)

  for (column in dictionary_curated_columns()) {
    fresh[[column]][existing] <- dictionary[[column]][matched_old[existing]]
  }

  source_compare <- setdiff(dictionary_source_columns(), dictionary_key_columns())
  fresh$drift_status <- "added"
  if (any(existing)) {
    changed <- vapply(which(existing), function(index) {
      old_index <- matched_old[[index]]
      any(vapply(source_compare, function(column) {
        !dictionary_values_equal(fresh[[column]][[index]], dictionary[[column]][[old_index]])
      }, logical(1)))
    }, logical(1))
    fresh$drift_status[which(existing)] <- ifelse(changed, "modified", "current")
  }

  removed <- dictionary[!(old_key %in% fresh_key), , drop = FALSE]
  if (nrow(removed) > 0) {
    removed$drift_status <- "removed"
    fresh <- rbind(fresh, removed[names(fresh)])
  }
  order_dictionary(fresh)
}

#' Validate an extended EDA data dictionary
#'
#' Validate the reusable multi-table dictionary contract and, when supplied, its normalised catalogue definitions.
#'
#' @param dictionary A data frame returned by [epi_eda_dictionary_scaffold()] or [epi_eda_dictionary_refresh()].
#' @param catalogues Optional data frame of normalised catalogue values.
#'
#' @return The validated dictionary, invisibly.
#'
#' @export
epi_eda_dictionary_validate <- function(dictionary, catalogues = NULL) {
  validate_dictionary_shape(dictionary)
  if (nrow(dictionary) == 0) {
    return(invisible(dictionary))
  }

  validate_dictionary_values(dictionary)
  validate_catalogues(dictionary, catalogues)
  invisible(dictionary)
}

#' Create an episcout EDA specification from a dictionary
#'
#' Select one active source table from a reusable dictionary and produce a specification accepted by [epi_eda_spec()]. Catalogue values are encoded into `levels` and `missing_codes` in display order.
#'
#' @param dictionary A validated extended dictionary.
#' @param table A table name or a `schema.table` qualified name.
#' @param catalogues Optional normalised catalogue data frame.
#'
#' @return A validated episcout EDA specification data frame.
#'
#' @export
epi_eda_dictionary_spec <- function(dictionary, table, catalogues = NULL) {
  epi_eda_dictionary_validate(dictionary, catalogues)
  selected <- select_dictionary_table(dictionary, table)
  selected <- selected[selected$drift_status != "removed", , drop = FALSE]
  if (nrow(selected) == 0) {
    stop("No active columns were found for table '", table, "'.", call. = FALSE)
  }
  selected <- selected[order(selected$analytic_order, selected$source_ordinal), , drop = FALSE]

  levels <- rep("", nrow(selected))
  missing_codes <- selected$missing_codes
  if (!is.null(catalogues)) {
    for (index in seq_len(nrow(selected))) {
      catalogue_name <- selected$catalog_name[[index]]
      if (catalogue_name == "") next
      values <- catalogues[catalogues$catalog_name == catalogue_name, , drop = FALSE]
      values <- values[order(values$display_order, values$source_value, na.last = TRUE), , drop = FALSE]
      levels[[index]] <- paste(values$source_value, collapse = ";")
      catalogue_missing <- values$source_value[values$is_missing]
      missing_codes[[index]] <- combine_dictionary_codes(
        selected$missing_codes[[index]],
        catalogue_missing
      )
    }
  }

  spec <- data.frame(
    name = selected$source_column,
    label = selected$label,
    type = selected$type,
    role = selected$role,
    units = selected$units,
    levels = levels,
    min = selected$min,
    max = selected$max,
    missing_codes = missing_codes,
    required = selected$required,
    group = selected$group,
    description = selected$description,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  epi_eda_spec(spec)
}

#' Profile approved PostgreSQL catalogue columns
#'
#' Return value counts only for active dictionary rows explicitly marked with `profile_catalogue = TRUE`. Direct identifiers, quasi-identifiers, unclassified fields and columns exceeding `max_levels` are rejected before value counts are returned.
#'
#' @param con An open DBI connection created with RPostgres.
#' @param dictionary A validated extended dictionary.
#' @param max_levels Maximum distinct non-missing values allowed for each profiled column.
#'
#' @return A data frame containing source keys, `source_value` and `n`.
#'
#' @export
epi_db_catalogue_profile <- function(con, dictionary, max_levels = 50) {
  validate_dictionary_shape(dictionary)
  validate_dictionary_values(dictionary)
  profile_rows <- dictionary[
    dictionary$drift_status != "removed" & dictionary$profile_catalogue, ,
    drop = FALSE
  ]
  validate_profile_rows(profile_rows)
  if (nrow(profile_rows) == 0) {
    return(empty_catalogue_profile())
  }
  validate_postgres_connection(con)
  if (!is.numeric(max_levels) || length(max_levels) != 1 || is.na(max_levels) || max_levels < 1 || max_levels != floor(max_levels)) {
    stop("max_levels must be a positive whole number.", call. = FALSE)
  }

  results <- lapply(seq_len(nrow(profile_rows)), function(index) {
    row <- profile_rows[index, , drop = FALSE]
    identifiers <- quote_dictionary_identifiers(con, row)
    cardinality_query <- paste(
      "SELECT COUNT(DISTINCT", identifiers$column, ") AS n_levels FROM", identifiers$table
    )
    n_levels <- as.numeric(DBI::dbGetQuery(con, cardinality_query)$n_levels[[1]])
    if (n_levels > max_levels) {
      stop(
        "Catalogue profiling refused ", qualified_dictionary_column(row),
        ": ", n_levels, " distinct values exceed max_levels = ", max_levels, ".",
        call. = FALSE
      )
    }
    profile_query <- paste(
      "SELECT", identifiers$column, "AS source_value, COUNT(*) AS n FROM",
      identifiers$table, "GROUP BY", identifiers$column, "ORDER BY", identifiers$column, "NULLS FIRST"
    )
    result <- as.data.frame(DBI::dbGetQuery(con, profile_query), stringsAsFactors = FALSE)
    result$source_schema <- row$source_schema[[1]]
    result$source_table <- row$source_table[[1]]
    result$source_column <- row$source_column[[1]]
    result <- result[c("source_schema", "source_table", "source_column", "source_value", "n")]
    result$n <- as.numeric(result$n)
    result
  })
  do.call(rbind, results)
}

validate_inventory_object <- function(inventory) {
  if (!inherits(inventory, "epi_db_inventory") || !is.list(inventory) || !is.data.frame(inventory$columns)) {
    stop("inventory must be an epi_db_inventory object.", call. = FALSE)
  }
  missing_columns <- setdiff(dictionary_source_columns(), names(inventory$columns))
  if (length(missing_columns) > 0) {
    stop(
      "inventory columns are missing required fields: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

empty_dictionary <- function() {
  data.frame(
    source_schema = character(),
    source_table = character(),
    source_column = character(),
    source_ordinal = integer(),
    source_data_type = character(),
    source_udt_name = character(),
    source_is_nullable = character(),
    source_character_maximum_length = numeric(),
    source_numeric_precision = numeric(),
    source_numeric_scale = numeric(),
    source_column_comment = character(),
    label = character(),
    type = character(),
    role = character(),
    units = character(),
    min = character(),
    max = character(),
    missing_codes = character(),
    required = logical(),
    group = character(),
    description = character(),
    catalog_name = character(),
    privacy_class = character(),
    analytic_action = character(),
    analytic_order = integer(),
    provenance = character(),
    validation_status = character(),
    profile_catalogue = logical(),
    drift_status = character(),
    stringsAsFactors = FALSE
  )
}

dictionary_eda_type <- function(source_type) {
  type <- tolower(as.character(source_type))
  if (type %in% c("smallint", "integer", "bigint")) {
    return("integer")
  }
  if (type %in% c("numeric", "decimal", "real", "double precision")) {
    return("numeric")
  }
  if (type == "boolean") {
    return("binary")
  }
  if (type == "date") {
    return("date")
  }
  if (grepl("timestamp", type, fixed = TRUE)) {
    return("datetime")
  }
  "text"
}

validate_dictionary_shape <- function(dictionary) {
  if (!is.data.frame(dictionary)) {
    stop("dictionary must be a data frame.", call. = FALSE)
  }
  required <- c(dictionary_source_columns(), dictionary_curated_columns(), "drift_status")
  missing <- setdiff(required, names(dictionary))
  if (length(missing) > 0) {
    stop("dictionary is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(dictionary) > 0 && anyDuplicated(dictionary_key(dictionary))) {
    stop("dictionary source_schema, source_table and source_column keys must be unique.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_dictionary_values <- function(dictionary) {
  character_required <- c(
    dictionary_key_columns(),
    "source_data_type",
    "label",
    "type",
    "privacy_class",
    "analytic_action",
    "provenance",
    "validation_status",
    "drift_status"
  )
  for (column in character_required) {
    values <- as.character(dictionary[[column]])
    if (anyNA(values) || any(trimws(values) == "")) {
      stop("dictionary column '", column, "' must contain non-empty values.", call. = FALSE)
    }
  }
  allowed_types <- c("numeric", "integer", "categorical", "binary", "date", "datetime", "text")
  validate_dictionary_choice(dictionary$type, allowed_types, "type")
  validate_dictionary_choice(
    dictionary$privacy_class,
    c("unclassified", "direct_identifier", "quasi_identifier", "sensitive", "non_sensitive"),
    "privacy_class"
  )
  validate_dictionary_choice(
    dictionary$analytic_action,
    c("review", "bridge", "drop", "retain_restricted", "retain", "derive"),
    "analytic_action"
  )
  validate_dictionary_choice(
    dictionary$validation_status,
    c("unreviewed", "pending", "confirmed"),
    "validation_status"
  )
  validate_dictionary_choice(
    dictionary$drift_status,
    c("current", "added", "modified", "removed"),
    "drift_status"
  )
  if (!is.logical(dictionary$required) || anyNA(dictionary$required)) {
    stop("dictionary required must contain non-missing logical values.", call. = FALSE)
  }
  if (!is.logical(dictionary$profile_catalogue) || anyNA(dictionary$profile_catalogue)) {
    stop("dictionary profile_catalogue must contain non-missing logical values.", call. = FALSE)
  }
  valid_order <- is.numeric(dictionary$analytic_order) & !is.na(dictionary$analytic_order) &
    dictionary$analytic_order >= 1 & dictionary$analytic_order == floor(dictionary$analytic_order)
  if (!all(valid_order)) {
    stop("dictionary analytic_order must contain positive whole numbers.", call. = FALSE)
  }
  active <- dictionary[dictionary$drift_status != "removed", , drop = FALSE]
  order_keys <- paste(active$source_schema, active$source_table, active$analytic_order, sep = "\r")
  if (anyDuplicated(order_keys)) {
    stop("dictionary analytic_order values must be unique within each active table.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_dictionary_choice <- function(values, allowed, column) {
  invalid <- is.na(values) | !(values %in% allowed)
  if (any(invalid)) {
    stop(
      "dictionary column '", column, "' contains invalid values: ",
      paste(unique(values[invalid]), collapse = ", "),
      call. = FALSE
    )
  }
}

validate_catalogues <- function(dictionary, catalogues) {
  referenced <- unique(dictionary$catalog_name[dictionary$catalog_name != ""])
  if (is.null(catalogues)) {
    if (length(referenced) > 0) {
      stop("catalogues must be supplied when dictionary catalog_name values are present.", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  if (!is.data.frame(catalogues)) {
    stop("catalogues must be NULL or a data frame.", call. = FALSE)
  }
  required <- c(
    "catalog_name", "source_value", "label", "display_order",
    "is_missing", "provenance", "validation_status"
  )
  missing <- setdiff(required, names(catalogues))
  if (length(missing) > 0) {
    stop("catalogues is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyNA(catalogues$catalog_name) || any(trimws(catalogues$catalog_name) == "")) {
    stop("catalogues catalog_name must contain non-empty values.", call. = FALSE)
  }
  if (anyNA(catalogues$source_value) || any(grepl(";", catalogues$source_value, fixed = TRUE))) {
    stop("catalogues source_value must be non-missing and must not contain semicolons.", call. = FALSE)
  }
  for (column in c("label", "provenance", "validation_status")) {
    values <- as.character(catalogues[[column]])
    if (anyNA(values) || any(trimws(values) == "")) {
      stop("catalogues column '", column, "' must contain non-empty values.", call. = FALSE)
    }
  }
  invalid_status <- !(catalogues$validation_status %in% c("unreviewed", "pending", "confirmed"))
  if (any(invalid_status)) {
    stop("catalogues validation_status contains invalid values.", call. = FALSE)
  }
  catalogue_keys <- paste(catalogues$catalog_name, catalogues$source_value, sep = "\r")
  if (anyDuplicated(catalogue_keys)) {
    stop("catalogues catalog_name and source_value keys must be unique.", call. = FALSE)
  }
  if (!is.logical(catalogues$is_missing) || anyNA(catalogues$is_missing)) {
    stop("catalogues is_missing must contain non-missing logical values.", call. = FALSE)
  }
  valid_order <- is.numeric(catalogues$display_order) & !is.na(catalogues$display_order) &
    catalogues$display_order >= 1 & catalogues$display_order == floor(catalogues$display_order)
  if (!all(valid_order)) {
    stop("catalogues display_order must contain positive whole numbers.", call. = FALSE)
  }
  catalogue_order_keys <- paste(catalogues$catalog_name, catalogues$display_order, sep = "\r")
  if (anyDuplicated(catalogue_order_keys)) {
    stop("catalogues display_order values must be unique within each catalogue.", call. = FALSE)
  }
  missing_references <- setdiff(referenced, unique(catalogues$catalog_name))
  if (length(missing_references) > 0) {
    stop("dictionary references missing catalogues: ", paste(missing_references, collapse = ", "), call. = FALSE)
  }
  invalid_catalogue_types <- dictionary$catalog_name != "" &
    !(dictionary$type %in% c("categorical", "binary"))
  if (any(invalid_catalogue_types)) {
    stop("dictionary catalog_name is only valid for categorical or binary fields.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_profile_rows <- function(rows) {
  if (nrow(rows) == 0) {
    return(invisible(TRUE))
  }
  blocked <- rows$privacy_class %in% c("unclassified", "direct_identifier", "quasi_identifier") |
    rows$analytic_action %in% c("review", "bridge", "drop") |
    rows$type == "text"
  if (any(blocked)) {
    names <- vapply(which(blocked), function(index) {
      qualified_dictionary_column(rows[index, , drop = FALSE])
    }, character(1))
    stop("Catalogue profiling is not allowed for: ", paste(names, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

select_dictionary_table <- function(dictionary, table) {
  if (!is.character(table) || length(table) != 1 || is.na(table) || trimws(table) == "") {
    stop("table must be a single non-empty character value.", call. = FALSE)
  }
  qualified <- paste(dictionary$source_schema, dictionary$source_table, sep = ".")
  if (grepl(".", table, fixed = TRUE)) {
    selected <- dictionary[qualified == table, , drop = FALSE]
  } else {
    matching_schemas <- unique(dictionary$source_schema[dictionary$source_table == table])
    if (length(matching_schemas) > 1) {
      stop("table is ambiguous across schemas; use a schema.table qualified name.", call. = FALSE)
    }
    selected <- dictionary[dictionary$source_table == table, , drop = FALSE]
  }
  if (nrow(selected) == 0) {
    stop("Table was not found in dictionary: ", table, ".", call. = FALSE)
  }
  selected
}

combine_dictionary_codes <- function(existing, additional) {
  current <- if (is.na(existing) || trimws(existing) == "") character() else strsplit(existing, ";", fixed = TRUE)[[1]]
  paste(unique(c(current, as.character(additional))), collapse = ";")
}

dictionary_key <- function(dictionary) {
  do.call(paste, c(dictionary[dictionary_key_columns()], list(sep = "\r")))
}

dictionary_values_equal <- function(left, right) {
  if (length(left) != 1 || length(right) != 1) {
    return(FALSE)
  }
  if (is.na(left) && is.na(right)) {
    return(TRUE)
  }
  if (is.na(left) || is.na(right)) {
    return(FALSE)
  }
  identical(as.character(left), as.character(right))
}

order_dictionary <- function(dictionary) {
  ordering <- order(
    dictionary$source_schema,
    dictionary$source_table,
    dictionary$source_ordinal,
    dictionary$source_column,
    na.last = TRUE
  )
  dictionary <- dictionary[ordering, , drop = FALSE]
  row.names(dictionary) <- NULL
  dictionary
}

quote_dictionary_identifiers <- function(con, row) {
  table <- DBI::Id(schema = row$source_schema[[1]], table = row$source_table[[1]])
  list(
    table = as.character(DBI::dbQuoteIdentifier(con, table)),
    column = as.character(DBI::dbQuoteIdentifier(con, row$source_column[[1]]))
  )
}

qualified_dictionary_column <- function(row) {
  paste(row$source_schema[[1]], row$source_table[[1]], row$source_column[[1]], sep = ".")
}

empty_catalogue_profile <- function() {
  data.frame(
    source_schema = character(),
    source_table = character(),
    source_column = character(),
    source_value = character(),
    n = numeric(),
    stringsAsFactors = FALSE
  )
}
