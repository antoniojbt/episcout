#' Render a traceable Table 1 data frame
#'
#' Format an [epi_eda_profile_stratified()] calculation as a long, review-ready
#' plain data frame. Formatting never replaces the machine-readable source
#' fields and adds no p-values or automatic small-cell suppression.
#'
#' @param result An `epi_eda_stratified` result.
#'
#' @return An ordinary long-form data frame. `denominator` records the numeric
#'   denominator behind every count/percentage display. Output is not
#'   disclosure-controlled and must be reviewed before sharing.
#'
#' @export
epi_eda_table1 <- function(result) {
  stratified_validate_table(result)
  variable_info <- result$variables[!duplicated(result$variables$name), c(
    "name", "label", "type"
  ), drop = FALSE]
  variable_info$label <- vapply(
    seq_len(nrow(variable_info)),
    function(index) stratified_label(variable_info$label[[index]], variable_info$name[[index]]),
    character(1)
  )
  variable_info <- variable_info[variable_info$name != result$metadata$strata, , drop = FALSE]
  rows <- list()
  for (variable_order in seq_len(nrow(variable_info))) {
    info <- variable_info[variable_order, , drop = FALSE]
    name <- info$name[[1]]
    type <- info$type[[1]]
    if (type %in% c("numeric", "integer")) {
      source <- result$numeric[result$numeric$name == name, , drop = FALSE]
      for (index in seq_len(nrow(source))) {
        row <- source[index, , drop = FALSE]
        rows <- c(rows, stratified_table_numeric(row, info, variable_order))
      }
    } else if (type %in% c("categorical", "binary")) {
      source <- result$categorical[result$categorical$name == name, , drop = FALSE]
      for (index in seq_len(nrow(source))) {
        row <- source[index, , drop = FALSE]
        level_order <- sum(source$group_id[[index]] == source$group_id[seq_len(index)])
        rows[[length(rows) + 1L]] <- stratified_table_categorical(
          row, info, variable_order, level_order
        )
      }
    } else if (type %in% c("date", "datetime")) {
      source <- result$temporal[result$temporal$name == name, , drop = FALSE]
      for (index in seq_len(nrow(source))) {
        rows <- c(rows, stratified_table_temporal(source[index, , drop = FALSE], info, variable_order))
      }
    } else if (type == "text") {
      source <- result$text[result$text$name == name, , drop = FALSE]
      for (index in seq_len(nrow(source))) {
        rows <- c(rows, stratified_table_text(source[index, , drop = FALSE], info, variable_order))
      }
    }
  }
  if (length(rows) == 0L) {
    return(empty_eda_table1())
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$variable_order, out$row_order, out$.group_order), , drop = FALSE]
  out$.group_order <- NULL
  row.names(out) <- NULL
  out
}

stratified_table_numeric <- function(row, info, variable_order) {
  note <- stratified_table_note(row)
  if (!is.na(row$n_infinite[[1]]) && row$n_infinite[[1]] > 0L) {
    note <- paste(note, "Non-finite observations are excluded from finite statistics.")
  }
  list(
    stratified_table_row(
      variable_order, 1L, info, NA_character_, "", "mean_sd", row,
      row$n_finite, paste0(stratified_format(row$mean), " (", stratified_format(row$sd), ")"), note
    ),
    stratified_table_row(
      variable_order, 2L, info, NA_character_, "", "median_iqr", row,
      row$n_finite,
      paste0(stratified_format(row$median), " [", stratified_format(row$q1), ", ", stratified_format(row$q3), "]"),
      note
    ),
    stratified_table_row(
      variable_order, 3L, info, NA_character_, "Missing", "missing", row,
      row$n, stratified_count_percent(row$n_missing, row$n),
      paste(note, "Missing percentages use all rows in the group.")
    )
  )
}

stratified_table_categorical <- function(row, info, variable_order, row_order) {
  missing <- row$is_missing_level[[1]]
  percentage <- if (missing) row$p_total[[1]] else row$p_observed[[1]]
  denominator <- if (missing) row$n_total[[1]] else row$n_observed[[1]]
  level_label <- if (missing) "Missing" else as.character(row$level[[1]])
  note <- stratified_table_note(row)
  if (isTRUE(row$is_unexpected[[1]])) {
    note <- paste(note, "Unexpected level.")
  }
  note <- paste(
    note,
    if (missing) "Missing percentages use all rows in the group." else "Level percentages use observed non-missing values."
  )
  stratified_table_row(
    variable_order, row_order, info, row$level[[1]], level_label,
    if (missing) "missing" else "level", row, denominator,
    stratified_count_proportion(row$n[[1]], percentage), note
  )
}

stratified_table_temporal <- function(row, info, variable_order) {
  timezone <- if (is.na(row$timezone[[1]]) || !nzchar(row$timezone[[1]])) {
    ""
  } else {
    paste0(" Timezone: ", row$timezone[[1]], ".")
  }
  note <- paste0(
    stratified_table_note(row), timezone,
    " Range unit: ", row$range_unit[[1]], "."
  )
  list(
    stratified_table_row(
      variable_order, 1L, info, NA_character_, "", "median_iqr", row,
      row$n_observed,
      paste0(stratified_text_value(row$median), " [", stratified_text_value(row$q1), ", ", stratified_text_value(row$q3), "]"),
      note
    ),
    stratified_table_row(
      variable_order, 2L, info, NA_character_, "", "range", row,
      row$n_observed,
      paste0(stratified_text_value(row$min), " to ", stratified_text_value(row$max)),
      note
    ),
    stratified_table_row(
      variable_order, 3L, info, NA_character_, "Missing", "missing", row,
      row$n, stratified_count_percent(row$n_missing, row$n),
      paste(note, "Missing percentages use all rows in the group.")
    )
  )
}

stratified_table_text <- function(row, info, variable_order) {
  note <- paste(stratified_table_note(row), "Text diagnostics contain no observed examples.")
  length_display <- if (is.na(row$min_length[[1]]) || is.na(row$max_length[[1]])) {
    "\u2014"
  } else {
    paste0(row$min_length[[1]], " to ", row$max_length[[1]])
  }
  list(
    stratified_table_row(
      variable_order, 1L, info, NA_character_, "", "observed_unique", row,
      row$n_observed,
      paste0(row$n_observed[[1]], " observed; ", row$n_unique[[1]], " unique"), note
    ),
    stratified_table_row(
      variable_order, 2L, info, NA_character_, "", "length_range", row,
      row$n_observed, length_display, note
    ),
    stratified_table_row(
      variable_order, 3L, info, NA_character_, "", "blank_whitespace", row,
      row$n_observed,
      paste0(row$n_empty[[1]], " empty; ", row$n_whitespace[[1]], " whitespace-only"),
      note
    ),
    stratified_table_row(
      variable_order, 4L, info, NA_character_, "Missing", "missing", row,
      row$n, stratified_count_percent(row$n_missing, row$n),
      paste(note, "Missing percentages use all rows in the group.")
    )
  )
}

stratified_table_row <- function(variable_order,
                                 row_order,
                                 info,
                                 level,
                                 level_label,
                                 statistic,
                                 group,
                                 denominator,
                                 display,
                                 note) {
  group_n <- if ("n_total" %in% names(group)) group$n_total[[1]] else group$n[[1]]
  data.frame(
    variable_order = as.integer(variable_order),
    row_order = as.integer(row_order),
    name = info$name[[1]],
    label = info$label[[1]],
    type = info$type[[1]],
    level = as.character(level),
    level_label = as.character(level_label),
    statistic = statistic,
    group_id = group$group_id[[1]],
    group_label = group$group_label[[1]],
    group_n = as.integer(group_n),
    denominator = as.integer(denominator[[1]]),
    display = as.character(display),
    note = trimws(note),
    .group_order = group$group_order[[1]],
    stringsAsFactors = FALSE
  )
}

stratified_table_note <- function(row) {
  notes <- "Counts are not disclosure-controlled."
  if (isTRUE(row$is_missing_stratum[[1]])) {
    notes <- paste(notes, "Missing stratum.")
  }
  if (isTRUE(row$is_unexpected_stratum[[1]])) {
    notes <- paste(notes, "Unexpected stratum.")
  }
  notes
}

stratified_format <- function(value) {
  if (length(value) == 0L || is.na(value[[1]])) {
    return("\u2014")
  }
  formatC(as.numeric(value[[1]]), format = "f", digits = 1)
}

stratified_text_value <- function(value) {
  if (length(value) == 0L || is.na(value[[1]]) || !nzchar(value[[1]])) "\u2014" else as.character(value[[1]])
}

stratified_count_percent <- function(count, denominator) {
  proportion <- summary_safe_proportion(count[[1]], denominator[[1]])
  stratified_count_proportion(count[[1]], proportion)
}

stratified_count_proportion <- function(count, proportion) {
  percent <- if (is.na(proportion)) "\u2014" else paste0(formatC(100 * proportion, format = "f", digits = 1), "%")
  paste0(as.integer(count), " (", percent, ")")
}

empty_eda_table1 <- function() {
  data.frame(
    variable_order = integer(), row_order = integer(), name = character(),
    label = character(), type = character(), level = character(),
    level_label = character(), statistic = character(), group_id = character(),
    group_label = character(), group_n = integer(), denominator = integer(),
    display = character(), note = character(), stringsAsFactors = FALSE
  )
}

stratified_validate_table <- function(result) {
  component_names <- c(
    "groups", "variables", "numeric", "categorical", "text",
    "temporal", "skipped", "metadata"
  )
  if (!inherits(result, "epi_eda_stratified") ||
        !identical(names(result), component_names)) {
    stop("result must be an epi_eda_stratified object.", call. = FALSE)
  }
  prefix <- c(
    "group_id", "group_order", "group_label", "is_missing_stratum",
    "is_unexpected_stratum"
  )
  required <- list(
    groups = c("group_id", "group_order", "group_label", "n"),
    variables = c("name", "label", "type"),
    numeric = c(prefix, "name", "n", "n_missing", "n_infinite", "n_finite", "mean", "sd", "median", "q1", "q3"),
    categorical = c(prefix, "name", "level", "n", "n_total", "n_observed", "p_total", "p_observed", "is_unexpected", "is_missing_level"),
    text = c(prefix, "name", "n", "n_missing", "n_observed", "n_unique", "n_empty", "n_whitespace", "min_length", "max_length"),
    temporal = c(prefix, "name", "timezone", "n", "n_missing", "n_observed", "min", "q1", "median", "q3", "max", "range_unit"),
    metadata = "strata"
  )
  valid <- vapply(names(required), function(name) {
    is.data.frame(result[[name]]) && all(required[[name]] %in% names(result[[name]]))
  }, logical(1))
  metadata_valid <- isTRUE(valid[["metadata"]]) && nrow(result$metadata) == 1L
  valid_strata <- FALSE
  if (metadata_valid) {
    strata <- result$metadata$strata
    valid_strata <- is.character(strata) && length(strata) == 1L &&
      !is.na(strata) && nzchar(strata)
  }
  if (any(!valid) || !is.data.frame(result$skipped) ||
        !valid_strata) {
    stop("result does not satisfy the epi_eda_stratified component contract.", call. = FALSE)
  }
  invisible(TRUE)
}
