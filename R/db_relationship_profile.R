# This module profiles explicitly selected same-table PostgreSQL column pairs.
# It returns bounded aggregate evidence only; semantic interpretation and any
# resulting dictionary or source-data changes remain the analyst's responsibility.

relationship_pair_columns <- function() {
  c(
    "left_schema", "left_table", "left_column",
    "right_schema", "right_table", "right_column"
  )
}

relationship_supported_types <- function() {
  c(
    "boolean", "character", "character varying", "text", "smallint",
    "integer", "bigint", "numeric", "decimal", "real",
    "double precision", "date", "time without time zone",
    "time with time zone", "timestamp without time zone",
    "timestamp with time zone", "uuid"
  )
}

#' Profile explicit PostgreSQL code and label relationships
#'
#' Inspect bounded aggregate relationships between explicitly selected pairs of
#' active dictionary columns from the same PostgreSQL table.
#'
#' @param con An open PostgreSQL-compatible DBI connection.
#' @param dictionary A validated extended dictionary.
#' @param pairs A data frame containing exactly `left_schema`, `left_table`,
#'   `left_column`, `right_schema`, `right_table` and `right_column`, in that
#'   order. Reversed pairs are separate requests.
#' @param max_levels Maximum distinct non-NULL `(left, right)` combinations
#'   allowed for each pair.
#'
#' @return An `epi_db_relationship_profile` list containing exactly `summary`,
#'   `mappings` and `conflicts`. All components start with the six pair keys.
#'   `summary` then contains `total_rows`, `both_present`, `left_missing`,
#'   `right_missing`, `both_missing`, `distinct_left`,
#'   `distinct_right`, `distinct_combinations`, `max_right_per_left`,
#'   `max_left_per_right`, `left_values_with_multiple_right`,
#'   `right_values_with_multiple_left` and `relationship_class`. `mappings` adds
#'   character `left_value` and `right_value`, and aggregate `n`. `conflicts`
#'   adds `exception_type`, character `left_value` and `right_value`, and
#'   aggregate `n`. Empty selections return typed zero-row components.
#'
#' @details Only active dictionary columns whose recorded `source_data_type` is
#'   an explicitly supported PostgreSQL scalar type can be profiled. Every pair
#'   is preflighted before any detail query; if its distinct both-present
#'   combination count exceeds `max_levels`, the complete call fails without a
#'   partial result. SQL NULL is partitioned separately and never becomes a
#'   mapping value. Literal observed values such as empty text, `"NA"` and
#'   `"NULL"` remain ordinary values.
#'
#'   `relationship_class` is one of `insufficient_data`, `one_to_one`,
#'   `constant_left`, `constant_right`, `one_to_many`, `many_to_one` or
#'   `many_to_many`. Directional conflict types are
#'   `left_maps_multiple_right` and `right_maps_multiple_left`; missingness types
#'   are `left_missing`, `right_missing` and `both_missing`. Missingness
#'   conflict values are `NA_character_`. These technical classes do not
#'   declare equivalence, redundancy, a canonical field or a transformation.
#'   Queries are aggregate-only, quote identifiers through DBI, cast returned
#'   mapping values to PostgreSQL `text`, and do not manage transactions or
#'   modify the database.
#'
#' @seealso [epi_db_catalogue_profile()], [epi_eda_dictionary_scaffold()],
#'   [epi_eda_dictionary_validate()]
#' @export
epi_db_relationship_profile <- function(con,
                                        dictionary,
                                        pairs,
                                        max_levels = 500) {
  validate_dictionary_shape(dictionary)
  validate_dictionary_values(dictionary)
  pairs <- validate_relationship_pairs(dictionary, pairs)
  validate_relationship_limit(max_levels)

  if (nrow(pairs) == 0) {
    return(empty_relationship_profile())
  }

  validate_postgres_connection(con)
  pairs <- order_relationship_data(pairs, relationship_pair_columns())

  preflights <- lapply(seq_len(nrow(pairs)), function(index) {
    pair <- pairs[index, , drop = FALSE]
    identifiers <- quote_relationship_identifiers(con, pair)
    aggregate <- DBI::dbGetQuery(
      con,
      relationship_preflight_query(identifiers)
    )
    relationship_summary_row(pair, aggregate)
  })
  summary <- do.call(rbind, preflights)
  row.names(summary) <- NULL

  over_limit <- which(summary$distinct_combinations > max_levels)
  if (length(over_limit) > 0) {
    row <- summary[over_limit[[1]], , drop = FALSE]
    stop(
      "Relationship profiling refused ", qualified_relationship_pair(row),
      ": ", row$distinct_combinations[[1]],
      " distinct non-NULL combinations exceed max_levels = ", max_levels, ".",
      call. = FALSE
    )
  }

  detail <- lapply(seq_len(nrow(pairs)), function(index) {
    pair <- pairs[index, , drop = FALSE]
    identifiers <- quote_relationship_identifiers(con, pair)
    mappings <- DBI::dbGetQuery(
      con,
      relationship_mappings_query(identifiers, max_levels)
    )
    if (nrow(mappings) > 0 &&
          as.numeric(mappings$observed_combinations[[1]]) > max_levels) {
      stop(
        "Relationship profiling refused ", qualified_relationship_pair(pair),
        ": ", as.numeric(mappings$observed_combinations[[1]]),
        " distinct non-NULL combinations exceed max_levels = ", max_levels,
        " after preflight.",
        call. = FALSE
      )
    }
    mappings$observed_combinations <- NULL
    list(
      mappings = relationship_mapping_rows(pair, mappings),
      conflicts = relationship_build_conflicts(
        pair,
        mappings,
        summary[index, , drop = FALSE]
      )
    )
  })

  mappings <- do.call(rbind, lapply(detail, `[[`, "mappings"))
  conflicts <- do.call(rbind, lapply(detail, `[[`, "conflicts"))
  mappings <- order_relationship_data(
    mappings,
    c(relationship_pair_columns(), "left_value", "right_value")
  )
  conflicts <- order_relationship_data(
    conflicts,
    c(
      relationship_pair_columns(), "left_value", "right_value",
      "exception_type"
    )
  )

  new_relationship_profile(summary, mappings, conflicts)
}

validate_relationship_pairs <- function(dictionary, pairs) {
  if (!is.data.frame(pairs)) {
    stop("pairs must be a data frame with the six relationship key fields.", call. = FALSE)
  }
  required <- relationship_pair_columns()
  if (!identical(names(pairs), required)) {
    stop(
      paste0(
        "pairs must contain exactly left_schema, left_table, left_column, ",
        "right_schema, right_table and right_column in that order."
      ),
      call. = FALSE
    )
  }
  pairs <- as.data.frame(pairs, stringsAsFactors = FALSE)
  for (column in required) {
    values <- as.character(pairs[[column]])
    if (anyNA(values) || any(trimws(values) == "")) {
      stop("pairs relationship keys must be non-empty.", call. = FALSE)
    }
    pairs[[column]] <- values
  }
  if (anyDuplicated(pairs[required])) {
    stop("pairs directional relationship keys must be unique.", call. = FALSE)
  }
  if (any(
    pairs$left_schema != pairs$right_schema |
      pairs$left_table != pairs$right_table
  )) {
    stop("Each relationship pair must select columns from the same table.", call. = FALSE)
  }
  if (any(pairs$left_column == pairs$right_column)) {
    stop("Relationship self-pairs are not supported.", call. = FALSE)
  }

  active <- dictionary[dictionary$drift_status != "removed", , drop = FALSE]
  left_rows <- match_relationship_rows(pairs, active, "left")
  right_rows <- match_relationship_rows(pairs, active, "right")
  if (anyNA(left_rows) || anyNA(right_rows)) {
    stop("Every relationship pair column must identify an active dictionary row.", call. = FALSE)
  }

  allowed <- relationship_supported_types()
  supported <- active$source_data_type[left_rows] %in% allowed &
    active$source_data_type[right_rows] %in% allowed
  if (any(!supported)) {
    invalid <- pairs[which(!supported)[[1]], , drop = FALSE]
    stop(
      "Relationship profiling does not support a recorded source_data_type for ",
      qualified_relationship_pair(invalid), ".",
      call. = FALSE
    )
  }
  pairs
}

validate_relationship_limit <- function(max_levels) {
  valid <- is.numeric(max_levels) && length(max_levels) == 1L &&
    !is.na(max_levels) && is.finite(max_levels) && max_levels >= 1 &&
    max_levels == floor(max_levels)
  if (!valid) {
    stop("max_levels must be a positive whole number.", call. = FALSE)
  }
  invisible(TRUE)
}

match_relationship_rows <- function(pairs, dictionary, side) {
  vapply(seq_len(nrow(pairs)), function(index) {
    matched <- which(
      dictionary$source_schema == pairs[[paste0(side, "_schema")]][[index]] &
        dictionary$source_table == pairs[[paste0(side, "_table")]][[index]] &
        dictionary$source_column == pairs[[paste0(side, "_column")]][[index]]
    )
    if (length(matched) == 1L) matched else NA_integer_
  }, integer(1))
}

quote_relationship_identifiers <- function(con, pair) {
  table <- DBI::Id(
    schema = pair$left_schema[[1]],
    table = pair$left_table[[1]]
  )
  list(
    table = as.character(DBI::dbQuoteIdentifier(con, table)),
    left = as.character(DBI::dbQuoteIdentifier(con, pair$left_column[[1]])),
    right = as.character(DBI::dbQuoteIdentifier(con, pair$right_column[[1]]))
  )
}

relationship_preflight_query <- function(identifiers) {
  paste0(
    "/* episcout_relationship_preflight */ WITH pair_mappings AS (SELECT ",
    identifiers$left, " AS left_value, ", identifiers$right,
    " AS right_value, COUNT(*)::bigint AS n FROM ", identifiers$table,
    " WHERE ", identifiers$left, " IS NOT NULL AND ", identifiers$right,
    " IS NOT NULL GROUP BY ", identifiers$left, ", ", identifiers$right,
    "), left_counts AS (SELECT left_value, COUNT(*)::bigint AS n_right ",
    "FROM pair_mappings GROUP BY left_value), right_counts AS (SELECT ",
    "right_value, COUNT(*)::bigint AS n_left FROM pair_mappings GROUP BY ",
    "right_value), row_partitions AS (SELECT ",
    "COUNT(*) FILTER (WHERE ", identifiers$left, " IS NOT NULL AND ",
    identifiers$right, " IS NOT NULL)::bigint AS both_present, ",
    "COUNT(*) FILTER (WHERE ", identifiers$left, " IS NULL AND ",
    identifiers$right, " IS NOT NULL)::bigint AS left_missing, ",
    "COUNT(*) FILTER (WHERE ", identifiers$left, " IS NOT NULL AND ",
    identifiers$right, " IS NULL)::bigint AS right_missing, ",
    "COUNT(*) FILTER (WHERE ", identifiers$left, " IS NULL AND ",
    identifiers$right, " IS NULL)::bigint AS both_missing FROM ",
    identifiers$table, ") SELECT (both_present + left_missing + ",
    "right_missing + both_missing)::bigint AS total_rows, row_partitions.*, ",
    "(SELECT COUNT(*)::bigint FROM left_counts) AS distinct_left, ",
    "(SELECT COUNT(*)::bigint FROM right_counts) AS distinct_right, ",
    "(SELECT COUNT(*)::bigint FROM pair_mappings) AS distinct_combinations, ",
    "COALESCE((SELECT MAX(n_right) FROM left_counts), 0)::bigint AS ",
    "max_right_per_left, COALESCE((SELECT MAX(n_left) FROM right_counts), ",
    "0)::bigint AS max_left_per_right, (SELECT COUNT(*)::bigint FROM ",
    "left_counts WHERE n_right > 1) AS left_values_with_multiple_right, ",
    "(SELECT COUNT(*)::bigint FROM right_counts WHERE n_left > 1) AS ",
    "right_values_with_multiple_left FROM row_partitions"
  )
}

relationship_mappings_query <- function(identifiers, max_levels) {
  detail_limit <- format(max_levels + 1, scientific = FALSE, trim = TRUE)
  paste0(
    "/* episcout_relationship_mappings */ WITH mappings AS (SELECT CAST(",
    identifiers$left,
    " AS text) AS left_value, CAST(", identifiers$right,
    " AS text) AS right_value, COUNT(*)::bigint AS n FROM ",
    identifiers$table, " WHERE ", identifiers$left, " IS NOT NULL AND ",
    identifiers$right, " IS NOT NULL GROUP BY ", identifiers$left, ", ",
    identifiers$right, ") SELECT mappings.*, ",
    "COUNT(*) OVER ()::bigint AS observed_combinations FROM mappings LIMIT ",
    detail_limit
  )
}

relationship_build_conflicts <- function(pair, mappings, summary) {
  conflicts <- list()
  if (nrow(mappings) > 0) {
    left_multiple <- duplicated(mappings$left_value) |
      duplicated(mappings$left_value, fromLast = TRUE)
    right_multiple <- duplicated(mappings$right_value) |
      duplicated(mappings$right_value, fromLast = TRUE)
    if (any(left_multiple)) {
      rows <- mappings[left_multiple, c("left_value", "right_value", "n")]
      rows$exception_type <- "left_maps_multiple_right"
      conflicts[[length(conflicts) + 1L]] <- rows[
        c("exception_type", "left_value", "right_value", "n")
      ]
    }
    if (any(right_multiple)) {
      rows <- mappings[right_multiple, c("left_value", "right_value", "n")]
      rows$exception_type <- "right_maps_multiple_left"
      conflicts[[length(conflicts) + 1L]] <- rows[
        c("exception_type", "left_value", "right_value", "n")
      ]
    }
  }

  for (exception_type in c("left_missing", "right_missing", "both_missing")) {
    if (summary[[exception_type]][[1]] > 0) {
      conflicts[[length(conflicts) + 1L]] <- data.frame(
        exception_type = exception_type,
        left_value = NA_character_,
        right_value = NA_character_,
        n = summary[[exception_type]][[1]],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(conflicts) == 0) {
    return(empty_relationship_conflicts())
  }
  relationship_conflict_rows(pair, do.call(rbind, conflicts))
}

relationship_summary_row <- function(pair, aggregate) {
  count_columns <- c(
    "total_rows", "both_present", "left_missing", "right_missing", "both_missing",
    "distinct_left", "distinct_right", "distinct_combinations",
    "max_right_per_left", "max_left_per_right",
    "left_values_with_multiple_right", "right_values_with_multiple_left"
  )
  counts <- as.list(vapply(count_columns, function(column) {
    as.numeric(aggregate[[column]][[1]])
  }, numeric(1)))
  names(counts) <- count_columns
  row <- cbind(pair, as.data.frame(counts, stringsAsFactors = FALSE))
  row$relationship_class <- classify_relationship(row)
  row
}

classify_relationship <- function(summary) {
  if (summary$both_present[[1]] == 0) {
    return("insufficient_data")
  }
  if (summary$distinct_left[[1]] == 1 &&
        summary$distinct_right[[1]] == 1) {
    return("one_to_one")
  }
  if (summary$distinct_left[[1]] == 1) {
    return("constant_left")
  }
  if (summary$distinct_right[[1]] == 1) {
    return("constant_right")
  }
  if (summary$max_right_per_left[[1]] == 1 &&
        summary$max_left_per_right[[1]] == 1) {
    return("one_to_one")
  }
  if (summary$max_right_per_left[[1]] > 1 &&
        summary$max_left_per_right[[1]] == 1) {
    return("one_to_many")
  }
  if (summary$max_right_per_left[[1]] == 1 &&
        summary$max_left_per_right[[1]] > 1) {
    return("many_to_one")
  }
  "many_to_many"
}

relationship_mapping_rows <- function(pair, mappings) {
  mappings <- as.data.frame(mappings, stringsAsFactors = FALSE)
  if (nrow(mappings) == 0) {
    return(empty_relationship_mappings())
  }
  mappings$left_value <- as.character(mappings$left_value)
  mappings$right_value <- as.character(mappings$right_value)
  mappings$n <- as.numeric(mappings$n)
  cbind(
    pair[rep(1L, nrow(mappings)), , drop = FALSE],
    mappings[c("left_value", "right_value", "n")],
    row.names = NULL
  )
}

relationship_conflict_rows <- function(pair, conflicts) {
  conflicts <- as.data.frame(conflicts, stringsAsFactors = FALSE)
  if (nrow(conflicts) == 0) {
    return(empty_relationship_conflicts())
  }
  conflicts$exception_type <- as.character(conflicts$exception_type)
  conflicts$left_value <- as.character(conflicts$left_value)
  conflicts$right_value <- as.character(conflicts$right_value)
  conflicts$n <- as.numeric(conflicts$n)
  cbind(
    pair[rep(1L, nrow(conflicts)), , drop = FALSE],
    conflicts[c("exception_type", "left_value", "right_value", "n")],
    row.names = NULL
  )
}

relationship_summary_columns <- function() {
  c(
    relationship_pair_columns(), "total_rows", "both_present", "left_missing",
    "right_missing", "both_missing", "distinct_left",
    "distinct_right", "distinct_combinations", "max_right_per_left",
    "max_left_per_right", "left_values_with_multiple_right",
    "right_values_with_multiple_left", "relationship_class"
  )
}

empty_relationship_summary <- function() {
  data <- as.data.frame(
    stats::setNames(rep(list(character()), 6L), relationship_pair_columns()),
    stringsAsFactors = FALSE
  )
  count_columns <- setdiff(
    relationship_summary_columns(),
    c(relationship_pair_columns(), "relationship_class")
  )
  for (column in count_columns) data[[column]] <- numeric()
  data$relationship_class <- character()
  data[relationship_summary_columns()]
}

empty_relationship_mappings <- function() {
  data <- as.data.frame(
    stats::setNames(rep(list(character()), 6L), relationship_pair_columns()),
    stringsAsFactors = FALSE
  )
  data$left_value <- character()
  data$right_value <- character()
  data$n <- numeric()
  data
}

empty_relationship_conflicts <- function() {
  data <- as.data.frame(
    stats::setNames(rep(list(character()), 6L), relationship_pair_columns()),
    stringsAsFactors = FALSE
  )
  data$exception_type <- character()
  data$left_value <- character()
  data$right_value <- character()
  data$n <- numeric()
  data
}

empty_relationship_profile <- function() {
  new_relationship_profile(
    empty_relationship_summary(),
    empty_relationship_mappings(),
    empty_relationship_conflicts()
  )
}

new_relationship_profile <- function(summary, mappings, conflicts) {
  structure(
    list(summary = summary, mappings = mappings, conflicts = conflicts),
    class = c("epi_db_relationship_profile", "list")
  )
}

relationship_utf8_key <- function(values) {
  vapply(enc2utf8(as.character(values)), function(value) {
    if (is.na(value)) return(NA_character_)
    paste(sprintf("%02x", as.integer(charToRaw(value))), collapse = "")
  }, character(1))
}

order_relationship_data <- function(data, columns) {
  if (nrow(data) == 0) {
    row.names(data) <- NULL
    return(data)
  }
  keys <- lapply(data[columns], relationship_utf8_key)
  ordering <- do.call(order, c(keys, list(na.last = TRUE, method = "radix")))
  data <- data[ordering, , drop = FALSE]
  row.names(data) <- NULL
  data
}

qualified_relationship_pair <- function(pair) {
  paste0(
    pair$left_schema[[1]], ".", pair$left_table[[1]], ".",
    pair$left_column[[1]], " -> ", pair$right_schema[[1]], ".",
    pair$right_table[[1]], ".", pair$right_column[[1]]
  )
}
