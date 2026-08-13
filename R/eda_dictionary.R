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
    "database_type",
    "analysis_type",
    "role",
    "units",
    "min",
    "max",
    "missing_codes",
    "required",
    "group",
    "description",
    "geo_role",
    "geo_pair",
    "geo_crs",
    "catalog_name",
    "analytic_order",
    "provenance"
  )
}

dictionary_removed_fields <- function() {
  c("privacy_class", "analytic_action", "validation_status", "profile_catalogue")
}

#' Create an editable data dictionary scaffold
#'
#' Convert an [epi_db_inventory()] result into a deterministic semantic dictionary scaffold. `database_type` records the PostgreSQL storage family and `analysis_type` records the EDA treatment; semantic and geographic fields remain unset.
#'
#' @param inventory An `epi_db_inventory` object.
#'
#' @return A data frame with one row per source column.
#'
#' @details The scaffold contains technical and semantic metadata only. Privacy and pseudonymisation policy belongs to [epi_sec_linkage_scaffold()]. episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared.
#'
#' @seealso [epi_db_inventory()], [epi_sec_linkage_scaffold()], [epi_sec_pseudonymise_db()]
#' @export
epi_eda_dictionary_scaffold <- function(inventory) {
  validate_inventory_object(inventory)
  columns <- inventory$columns
  if (nrow(columns) == 0) {
    return(empty_dictionary())
  }

  dictionary <- columns[dictionary_source_columns()]
  dictionary$label <- dictionary$source_column
  dictionary$database_type <- vapply(dictionary$source_data_type, dictionary_database_type, character(1))
  dictionary$analysis_type <- vapply(dictionary$source_data_type, dictionary_analysis_type, character(1))
  dictionary$role <- ""
  dictionary$units <- ""
  dictionary$min <- ""
  dictionary$max <- ""
  dictionary$missing_codes <- ""
  dictionary$required <- TRUE
  dictionary$group <- ""
  dictionary$description <- ""
  dictionary$geo_role <- ""
  dictionary$geo_pair <- ""
  dictionary$geo_crs <- ""
  dictionary$catalog_name <- ""
  dictionary$analytic_order <- as.integer(dictionary$source_ordinal)
  dictionary$provenance <- "database_inventory"
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
#' @param catalogues Optional data frame of normalised catalogue values with `catalog_name`, `source_value`, `label`, `display_order`, `is_missing` and `provenance`.
#'
#' @return The validated dictionary, invisibly.
#'
#' @details Validation checks technical and semantic contracts only. Specialised privacy and retained-column policy is declared separately through [epi_sec_linkage_spec()].
#'
#' @seealso [epi_eda_dictionary_scaffold()], [epi_sec_linkage_scaffold()], [epi_sec_pseudonymise_db()]
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
#' @param catalogues Optional normalised catalogue data frame with `catalog_name`, `source_value`, `label`, `display_order`, `is_missing` and `provenance`.
#'
#' @return A validated episcout EDA specification data frame.
#'
#' @details A pseudonymisation result's semantic `output_dictionary` and `output_catalogues` can be passed here directly.
#'
#' @seealso [epi_sec_pseudonymise_db()], [epi_eda_dictionary_validate()]
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
    database_type = selected$database_type,
    analysis_type = selected$analysis_type,
    role = selected$role,
    units = selected$units,
    levels = levels,
    min = selected$min,
    max = selected$max,
    missing_codes = missing_codes,
    required = selected$required,
    group = selected$group,
    description = selected$description,
    geo_role = selected$geo_role,
    geo_pair = selected$geo_pair,
    geo_crs = selected$geo_crs,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  epi_eda_spec(spec)
}

#' Profile explicitly selected PostgreSQL catalogue columns
#'
#' Return bounded value counts for an explicit metadata-only selection of active
#' dictionary columns.
#'
#' @param con An open DBI connection created with RPostgres.
#' @param dictionary A validated extended dictionary.
#' @param columns A data frame containing exactly `source_schema`,
#'   `source_table` and `source_column` keys.
#' @param max_levels Maximum distinct non-missing values allowed for each
#'   profiled column.
#'
#' @return An `epi_db_catalogue_profile` list with two data frames. `values`
#'   contains source keys, non-missing `source_value`, and `n`; `missing`
#'   contains one source-key row per profiled column and its aggregate
#'   `n_missing`. A selected empty or all-NULL column therefore has no `values`
#'   rows but still has one `missing` row. When no dictionary row is selected,
#'   both components are typed zero-row data frames.
#'
#' @details `max_levels` bounds only rows in `values`; PostgreSQL `NULL` is
#'   counted separately and is never returned as a catalogue `source_value`.
#'   This makes `values` directly usable when drafting the normalised catalogue
#'   contract, but does not classify any observed value as a missing code.
#'   The selector requests technical profiling only and makes no privacy,
#'   approval or sharing decision.
#'
#' @export
epi_db_catalogue_profile <- function(con, dictionary, columns, max_levels = 50) {
  validate_dictionary_shape(dictionary)
  validate_dictionary_values(dictionary)
  columns <- validate_profile_columns(columns)
  active <- dictionary[dictionary$drift_status != "removed", , drop = FALSE]
  matched <- match(dictionary_key(columns), dictionary_key(active))
  if (anyNA(matched)) {
    stop("Every catalogue profile column must identify an active dictionary row.", call. = FALSE)
  }
  profile_rows <- active[matched, , drop = FALSE]
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
      "SELECT COUNT(DISTINCT", identifiers$column, ") AS n_levels,",
      "COUNT(*) FILTER (WHERE", identifiers$column, "IS NULL) AS n_missing FROM",
      identifiers$table
    )
    cardinality <- DBI::dbGetQuery(con, cardinality_query)
    n_levels <- as.numeric(cardinality$n_levels[[1]])
    n_missing <- as.numeric(cardinality$n_missing[[1]])
    if (n_levels > max_levels) {
      stop(
        "Catalogue profiling refused ", qualified_dictionary_column(row),
        ": ", n_levels, " distinct values exceed max_levels = ", max_levels, ".",
        call. = FALSE
      )
    }
    profile_query <- paste(
      "SELECT", identifiers$column, "AS source_value, COUNT(*) AS n FROM",
      identifiers$table, "WHERE", identifiers$column, "IS NOT NULL GROUP BY",
      identifiers$column, "ORDER BY", identifiers$column
    )
    result <- as.data.frame(DBI::dbGetQuery(con, profile_query), stringsAsFactors = FALSE)
    result$source_value <- as.character(result$source_value)
    result$source_schema <- rep(row$source_schema[[1]], nrow(result))
    result$source_table <- rep(row$source_table[[1]], nrow(result))
    result$source_column <- rep(row$source_column[[1]], nrow(result))
    result <- result[c("source_schema", "source_table", "source_column", "source_value", "n")]
    result$n <- as.numeric(result$n)
    missing <- data.frame(
      source_schema = row$source_schema[[1]],
      source_table = row$source_table[[1]],
      source_column = row$source_column[[1]],
      n_missing = n_missing,
      stringsAsFactors = FALSE
    )
    list(values = result, missing = missing)
  })
  values <- do.call(rbind, lapply(results, `[[`, "values"))
  missing <- do.call(rbind, lapply(results, `[[`, "missing"))
  row.names(values) <- NULL
  row.names(missing) <- NULL
  new_catalogue_profile(values, missing)
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
    database_type = character(),
    analysis_type = character(),
    role = character(),
    units = character(),
    min = character(),
    max = character(),
    missing_codes = character(),
    required = logical(),
    group = character(),
    description = character(),
    geo_role = character(),
    geo_pair = character(),
    geo_crs = character(),
    catalog_name = character(),
    analytic_order = integer(),
    provenance = character(),
    drift_status = character(),
    stringsAsFactors = FALSE
  )
}

dictionary_database_type <- function(source_type) {
  type <- tolower(as.character(source_type))
  if (type %in% c("smallint", "integer", "bigint")) {
    return("integer")
  }
  if (type %in% c("numeric", "decimal", "real", "double precision")) {
    return("numeric")
  }
  if (type == "boolean") {
    return("boolean")
  }
  if (type == "date") {
    return("date")
  }
  if (grepl("timestamp", type, fixed = TRUE)) {
    return("datetime")
  }
  "text"
}

dictionary_analysis_type <- function(source_type) {
  database_type <- dictionary_database_type(source_type)
  if (database_type == "boolean") return("binary")
  database_type
}

validate_dictionary_shape <- function(dictionary) {
  if (!is.data.frame(dictionary)) {
    stop("dictionary must be a data frame.", call. = FALSE)
  }
  deprecated <- intersect(names(dictionary), dictionary_removed_fields())
  if (length(deprecated) > 0L) {
    stop(
      "This dictionary uses the removed combined EDA/security schema (",
      paste(deprecated, collapse = ", "),
      "). Regenerate the semantic dictionary and move column policy into epi_sec_linkage_spec().",
      call. = FALSE
    )
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
    "database_type",
    "analysis_type",
    "provenance",
    "drift_status"
  )
  for (column in character_required) {
    values <- as.character(dictionary[[column]])
    if (anyNA(values) || any(trimws(values) == "")) {
      stop("dictionary column '", column, "' must contain non-empty values.", call. = FALSE)
    }
  }
  allowed_database_types <- c("numeric", "integer", "boolean", "date", "datetime", "text")
  allowed_analysis_types <- c("numeric", "integer", "categorical", "binary", "date", "datetime", "text")
  validate_dictionary_choice(dictionary$database_type, allowed_database_types, "database_type")
  validate_dictionary_choice(dictionary$analysis_type, allowed_analysis_types, "analysis_type")
  validate_dictionary_choice(
    dictionary$drift_status,
    c("current", "added", "modified", "removed"),
    "drift_status"
  )
  if (!is.logical(dictionary$required) || anyNA(dictionary$required)) {
    stop("dictionary required must contain non-missing logical values.", call. = FALSE)
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
  validate_dictionary_geo(active)
  invisible(TRUE)
}

validate_dictionary_geo <- function(dictionary) {
  if (nrow(dictionary) == 0L) {
    return(invisible(TRUE))
  }
  tables <- unique(paste(dictionary$source_schema, dictionary$source_table, sep = "\r"))
  for (table in tables) {
    rows <- dictionary[paste(dictionary$source_schema, dictionary$source_table, sep = "\r") == table, , drop = FALSE]
    spec <- data.frame(
      name = rows$source_column,
      label = rows$label,
      database_type = rows$database_type,
      analysis_type = rows$analysis_type,
      role = rows$role,
      geo_role = rows$geo_role,
      geo_pair = rows$geo_pair,
      geo_crs = rows$geo_crs,
      stringsAsFactors = FALSE
    )
    epi_eda_validate_spec(spec)
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
  if ("validation_status" %in% names(catalogues)) {
    stop(
      "catalogues validation_status was removed. Supply semantic catalogue metadata without approval fields.",
      call. = FALSE
    )
  }
  required <- c(
    "catalog_name", "source_value", "label", "display_order",
    "is_missing", "provenance"
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
  for (column in c("label", "provenance")) {
    values <- as.character(catalogues[[column]])
    if (anyNA(values) || any(trimws(values) == "")) {
      stop("catalogues column '", column, "' must contain non-empty values.", call. = FALSE)
    }
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
    !(dictionary$analysis_type %in% c("categorical", "binary"))
  if (any(invalid_catalogue_types)) {
    stop("dictionary catalog_name is only valid for categorical or binary fields.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_profile_columns <- function(columns) {
  if (!is.data.frame(columns)) {
    stop("columns must be a data frame with the three source key fields.", call. = FALSE)
  }
  required <- dictionary_key_columns()
  if (!identical(names(columns), required)) {
    stop(
      "columns must contain exactly source_schema, source_table and source_column in that order.",
      call. = FALSE
    )
  }
  columns <- as.data.frame(columns, stringsAsFactors = FALSE)
  for (column in required) {
    values <- as.character(columns[[column]])
    if (anyNA(values) || any(trimws(values) == "")) {
      stop("columns source keys must be non-empty.", call. = FALSE)
    }
    columns[[column]] <- values
  }
  if (anyDuplicated(dictionary_key(columns))) {
    stop("columns source keys must be unique.", call. = FALSE)
  }
  columns
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
  new_catalogue_profile(
    values = data.frame(
      source_schema = character(),
      source_table = character(),
      source_column = character(),
      source_value = character(),
      n = numeric(),
      stringsAsFactors = FALSE
    ),
    missing = data.frame(
      source_schema = character(),
      source_table = character(),
      source_column = character(),
      n_missing = numeric(),
      stringsAsFactors = FALSE
    )
  )
}

new_catalogue_profile <- function(values, missing) {
  structure(
    list(values = values, missing = missing),
    class = c("epi_db_catalogue_profile", "list")
  )
}
