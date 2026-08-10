#' Calculate categorical display denominators
#'
#' Derive one traceable categorical numerator, denominator, and proportion
#' table from existing canonical or stratified EDA aggregate components. The
#' function never reads source observations or opens a database connection.
#'
#' @param result A canonical summary list returned by
#'   [epi_eda_profile_summaries()] or an `epi_eda_stratified` result returned
#'   by [epi_eda_profile_stratified()].
#' @param basis Percentage basis. `"compatibility"` uses observed values for
#'   ordinary levels and all group rows for the missing level. `"column"`
#'   uses all rows in each group. `"row"` distributes each level across
#'   non-Overall strata and is available only for stratified results.
#'   `"overall"` uses the included analysis population.
#'
#' @return A data frame with fixed columns `variable_order`, `level_order`,
#'   `name`, `label`, `type`, `level`, `group_id`, `group_order`,
#'   `group_label`, `is_overall`, `group_n`, `population_n`, `numerator`,
#'   `denominator`, `proportion`, `percentage_basis`, `denominator_scope`,
#'   `missing_treatment`, and `is_missing_level`. A zero denominator is
#'   retained as zero and its proportion is `NA_real_`.
#'
#' @details Standard missing values and declared missing codes are combined in
#'   one explicit missing-level row. The function does not infer a
#'   not-applicable category or preferred percentage basis. Row denominators
#'   exclude Overall from the cross-stratum sum to avoid double-counting.
#'
#' @export
epi_eda_categorical_display <- function(result,
                                        basis = c(
                                          "compatibility", "column", "row",
                                          "overall"
                                        )) {
  basis <- match.arg(basis)
  stratified <- inherits(result, "epi_eda_stratified")
  if (!stratified && basis == "row") {
    stop("basis = \"row\" requires an epi_eda_stratified result.", call. = FALSE)
  }
  source <- if (stratified) {
    eda_cat_display_stratified(result)
  } else {
    eda_cat_display_canonical(result)
  }
  eda_cat_display_calculate(source, basis)
}

eda_categorical_display_names <- function() {
  c(
    "variable_order", "level_order", "name", "label", "type", "level",
    "group_id", "group_order", "group_label", "is_overall", "group_n",
    "population_n", "numerator", "denominator", "proportion",
    "percentage_basis", "denominator_scope", "missing_treatment",
    "is_missing_level"
  )
}

eda_empty_categorical_display <- function() {
  data.frame(
    variable_order = integer(), level_order = integer(), name = character(),
    label = character(), type = character(), level = character(),
    group_id = character(), group_order = integer(), group_label = character(),
    is_overall = logical(), group_n = integer(), population_n = integer(),
    numerator = integer(), denominator = integer(), proportion = numeric(),
    percentage_basis = character(), denominator_scope = character(),
    missing_treatment = character(), is_missing_level = logical(),
    stringsAsFactors = FALSE
  )
}

eda_empty_cat_display_source <- function() {
  out <- eda_empty_categorical_display()[0, c(
    "variable_order", "level_order", "name", "label", "type", "level",
    "group_id", "group_order", "group_label", "is_overall", "group_n",
    "population_n", "numerator", "is_missing_level"
  )]
  out$.n_observed <- integer()
  out
}

eda_cat_display_canonical <- function(result) {
  expected <- c(
    "variables", "numeric", "categorical", "text", "temporal", "skipped"
  )
  required_variables <- c(
    "name", "label", "type", "n", "n_missing", "n_observed", "status"
  )
  required_categorical <- c("name", "level", "n", "p_total", "p_observed")
  valid <- is.list(result) && identical(names(result), expected) &&
    is.data.frame(result$variables) &&
    all(required_variables %in% names(result$variables)) &&
    is.data.frame(result$categorical) &&
    all(required_categorical %in% names(result$categorical))
  if (!valid) {
    stop("result must satisfy the canonical EDA summary component contract.", call. = FALSE)
  }

  variables <- result$variables[
    result$variables$type %in% c("categorical", "binary") &
      result$variables$status == "summarised",
    required_variables,
    drop = FALSE
  ]
  if (nrow(variables) == 0L) {
    if (nrow(result$categorical) > 0L) {
      stop("Canonical categorical aggregate counts did not reconcile.", call. = FALSE)
    }
    return(eda_empty_cat_display_source())
  }
  if (anyDuplicated(variables$name) ||
        !eda_cat_counts_valid(variables$n) ||
        !eda_cat_counts_valid(variables$n_missing) ||
        !eda_cat_counts_valid(variables$n_observed) ||
        any(as.numeric(variables$n_missing) + variables$n_observed != variables$n)) {
    stop("Canonical categorical aggregate counts did not reconcile.", call. = FALSE)
  }
  population <- unique(as.integer(variables$n))
  if (length(population) != 1L) {
    stop("Canonical categorical populations did not reconcile.", call. = FALSE)
  }
  categorical_reconciled <- all(result$categorical$name %in% variables$name) &&
    !anyNA(result$categorical$level) &&
    eda_cat_counts_valid(result$categorical$n) &&
    !anyDuplicated(paste(result$categorical$name, result$categorical$level, sep = "\r"))
  categorical_valid <- nrow(result$categorical) == 0L || categorical_reconciled
  if (!categorical_valid) {
    stop("Canonical categorical aggregate counts did not reconcile.", call. = FALSE)
  }

  rows <- list()
  for (variable_order in seq_len(nrow(variables))) {
    variable <- variables[variable_order, , drop = FALSE]
    levels <- result$categorical[
      result$categorical$name == variable$name[[1]], ,
      drop = FALSE
    ]
    if (sum(as.numeric(levels$n)) != variable$n_observed[[1]]) {
      stop("Canonical categorical aggregate counts did not reconcile.", call. = FALSE)
    }
    label <- eda_categorical_display_label(
      variable$label[[1]], variable$name[[1]]
    )
    if (nrow(levels) > 0L) {
      rows[[length(rows) + 1L]] <- eda_cat_display_source_rows(
        variable_order = variable_order,
        level_order = seq_len(nrow(levels)),
        name = variable$name[[1]],
        label = label,
        type = variable$type[[1]],
        level = as.character(levels$level),
        group_id = ".overall",
        group_order = 1L,
        group_label = "Overall",
        is_overall = TRUE,
        group_n = variable$n[[1]],
        population_n = population,
        numerator = levels$n,
        n_observed = variable$n_observed[[1]],
        is_missing_level = FALSE
      )
    }
    rows[[length(rows) + 1L]] <- eda_cat_display_source_rows(
      variable_order = variable_order,
      level_order = nrow(levels) + 1L,
      name = variable$name[[1]],
      label = label,
      type = variable$type[[1]],
      level = NA_character_,
      group_id = ".overall",
      group_order = 1L,
      group_label = "Overall",
      is_overall = TRUE,
      group_n = variable$n[[1]],
      population_n = population,
      numerator = variable$n_missing[[1]],
      n_observed = variable$n_observed[[1]],
      is_missing_level = TRUE
    )
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

eda_cat_display_stratified <- function(result) {
  stratified_validate_table(result)
  required_groups <- c(
    "group_id", "group_order", "group_label", "is_overall", "n"
  )
  required_variables <- c(
    "group_id", "group_order", "group_label", "is_overall", "name", "label",
    "type", "n", "n_missing", "n_observed", "status"
  )
  required_categorical <- c(
    "group_id", "group_order", "group_label", "is_overall", "name", "type",
    "level", "n", "n_total", "n_observed", "is_missing_level"
  )
  if (!all(required_groups %in% names(result$groups)) ||
        !all(required_variables %in% names(result$variables)) ||
        !all(required_categorical %in% names(result$categorical)) ||
        !is.data.frame(result$metadata) || nrow(result$metadata) != 1L ||
        !"n_included" %in% names(result$metadata)) {
    stop("result does not satisfy the categorical stratified component contract.", call. = FALSE)
  }
  groups <- result$groups[, required_groups, drop = FALSE]
  population <- result$metadata$n_included[[1]]
  if (anyDuplicated(groups$group_id) || anyDuplicated(groups$group_order) ||
        !is.logical(groups$is_overall) || anyNA(groups$is_overall) ||
        !eda_cat_counts_valid(groups$n) ||
        !eda_cat_counts_valid(population)) {
    stop("Stratified categorical populations did not reconcile.", call. = FALSE)
  }
  non_overall_n <- sum(as.numeric(groups$n[!groups$is_overall]))
  overall <- groups[groups$is_overall, , drop = FALSE]
  if (non_overall_n != population || nrow(overall) > 1L ||
        (nrow(overall) == 1L && overall$n[[1]] != population)) {
    stop("Stratified categorical populations did not reconcile.", call. = FALSE)
  }

  source_variables <- result$variables[, required_variables, drop = FALSE]
  variable_group <- match(source_variables$group_id, groups$group_id)
  variable_groups_valid <- !anyNA(variable_group) &&
    identical(source_variables$group_order, groups$group_order[variable_group]) &&
    identical(source_variables$group_label, groups$group_label[variable_group]) &&
    identical(source_variables$is_overall, groups$is_overall[variable_group])
  if (!variable_groups_valid) {
    stop("Stratified categorical populations did not reconcile.", call. = FALSE)
  }
  variables <- source_variables[
    source_variables$type %in% c("categorical", "binary") &
      source_variables$status == "summarised",
    c("group_id", "name", "label", "type", "n", "n_missing", "n_observed"),
    drop = FALSE
  ]
  variable_keys <- paste(variables$group_id, variables$name, sep = "\r")
  variable_names <- unique(variables$name)
  expected_variable_keys <- as.vector(outer(
    groups$group_id, variable_names, paste,
    sep = "\r"
  ))
  if (anyDuplicated(variable_keys) ||
        !setequal(variable_keys, expected_variable_keys) ||
        !all(variables$group_id %in% groups$group_id) ||
        !eda_cat_counts_valid(variables$n) ||
        !eda_cat_counts_valid(variables$n_missing) ||
        !eda_cat_counts_valid(variables$n_observed) ||
        any(as.numeric(variables$n_missing) + variables$n_observed != variables$n)) {
    stop("Stratified categorical aggregate counts did not reconcile.", call. = FALSE)
  }
  group_n <- groups$n[match(variables$group_id, groups$group_id)]
  if (any(variables$n != group_n)) {
    stop("Stratified categorical aggregate counts did not reconcile.", call. = FALSE)
  }
  categorical <- result$categorical[, required_categorical, drop = FALSE]
  if (nrow(variables) == 0L && nrow(categorical) == 0L) {
    return(eda_empty_cat_display_source())
  }
  if (!eda_cat_counts_valid(categorical$n) ||
        !eda_cat_counts_valid(categorical$n_total) ||
        !eda_cat_counts_valid(categorical$n_observed) ||
        !is.logical(categorical$is_missing_level) ||
        anyNA(categorical$is_missing_level)) {
    stop("Stratified categorical aggregate counts did not reconcile.", call. = FALSE)
  }
  categorical_group <- match(categorical$group_id, groups$group_id)
  categorical_groups_valid <- !anyNA(categorical_group) &&
    identical(categorical$group_order, groups$group_order[categorical_group]) &&
    identical(categorical$group_label, groups$group_label[categorical_group]) &&
    identical(categorical$is_overall, groups$is_overall[categorical_group])
  if (!categorical_groups_valid) {
    stop("Stratified categorical aggregate counts did not reconcile.", call. = FALSE)
  }
  categorical_keys <- paste(categorical$group_id, categorical$name, sep = "\r")
  if (!setequal(unique(categorical_keys), variable_keys)) {
    stop("Stratified categorical aggregate counts did not reconcile.", call. = FALSE)
  }
  for (key in variable_keys) {
    source <- categorical[categorical_keys == key, , drop = FALSE]
    variable <- variables[variable_keys == key, , drop = FALSE]
    ordinary <- source[!source$is_missing_level, , drop = FALSE]
    missing <- source[source$is_missing_level, , drop = FALSE]
    duplicate_levels <- anyDuplicated(as.character(ordinary$level))
    valid <- nrow(missing) == 1L && !duplicate_levels &&
      all(source$n_total == variable$n[[1]]) &&
      all(source$n_observed == variable$n_observed[[1]]) &&
      all(source$type == variable$type[[1]]) &&
      sum(as.numeric(ordinary$n)) == variable$n_observed[[1]] &&
      missing$n[[1]] == variable$n_missing[[1]]
    if (!valid) {
      stop("Stratified categorical aggregate counts did not reconcile.", call. = FALSE)
    }
  }

  for (name in variable_names) {
    variable_rows <- variables[variables$name == name, , drop = FALSE]
    if (length(unique(as.character(variable_rows$label))) != 1L ||
          length(unique(variable_rows$type)) != 1L) {
      stop("Stratified categorical aggregate counts did not reconcile.", call. = FALSE)
    }
    signatures <- lapply(groups$group_id, function(group_id) {
      rows <- categorical[
        categorical$name == name & categorical$group_id == group_id, ,
        drop = FALSE
      ]
      paste(
        rows$type,
        ifelse(rows$is_missing_level, "<missing>", paste0("<level>", rows$level)),
        rows$is_missing_level,
        sep = "\r"
      )
    })
    if (!all(vapply(signatures[-1L], identical, logical(1), signatures[[1L]]))) {
      stop("Stratified categorical aggregate counts did not reconcile.", call. = FALSE)
    }
  }

  variable_names <- unique(result$variables$name)
  variable_names <- variable_names[variable_names %in% variables$name]
  variable_info <- result$variables[
    match(variable_names, result$variables$name),
    c("name", "label", "type"),
    drop = FALSE
  ]
  match_variable <- match(categorical$name, variable_info$name)
  match_group <- match(categorical$group_id, groups$group_id)
  match_summary <- match(categorical_keys, variable_keys)
  level_order <- stats::ave(
    seq_len(nrow(categorical)), categorical_keys,
    FUN = function(index) seq_along(index)
  )
  out <- eda_cat_display_source_rows(
    variable_order = match_variable,
    level_order = level_order,
    name = categorical$name,
    label = vapply(seq_len(nrow(categorical)), function(index) {
      eda_categorical_display_label(
        variable_info$label[[match_variable[[index]]]],
        categorical$name[[index]]
      )
    }, character(1)),
    type = categorical$type,
    level = categorical$level,
    group_id = categorical$group_id,
    group_order = groups$group_order[match_group],
    group_label = groups$group_label[match_group],
    is_overall = groups$is_overall[match_group],
    group_n = groups$n[match_group],
    population_n = population,
    numerator = categorical$n,
    n_observed = variables$n_observed[match_summary],
    is_missing_level = categorical$is_missing_level
  )
  out <- out[order(out$variable_order, out$group_order, out$level_order), , drop = FALSE]
  row.names(out) <- NULL
  out
}

eda_cat_display_source_rows <- function(variable_order,
                                        level_order,
                                        name,
                                        label,
                                        type,
                                        level,
                                        group_id,
                                        group_order,
                                        group_label,
                                        is_overall,
                                        group_n,
                                        population_n,
                                        numerator,
                                        n_observed,
                                        is_missing_level) {
  lengths <- c(
    length(variable_order), length(level_order), length(name), length(label),
    length(type), length(level), length(group_id), length(group_order),
    length(group_label), length(is_overall), length(group_n),
    length(population_n), length(numerator), length(n_observed),
    length(is_missing_level)
  )
  size <- max(lengths)
  if (!is.finite(size) || size == 0L) {
    return(eda_empty_cat_display_source())
  }
  data.frame(
    variable_order = rep(as.integer(variable_order), length.out = size),
    level_order = rep(as.integer(level_order), length.out = size),
    name = rep(as.character(name), length.out = size),
    label = rep(as.character(label), length.out = size),
    type = rep(as.character(type), length.out = size),
    level = rep(as.character(level), length.out = size),
    group_id = rep(as.character(group_id), length.out = size),
    group_order = rep(as.integer(group_order), length.out = size),
    group_label = rep(as.character(group_label), length.out = size),
    is_overall = rep(as.logical(is_overall), length.out = size),
    group_n = rep(as.integer(group_n), length.out = size),
    population_n = rep(as.integer(population_n), length.out = size),
    numerator = rep(as.integer(numerator), length.out = size),
    is_missing_level = rep(as.logical(is_missing_level), length.out = size),
    .n_observed = rep(as.integer(n_observed), length.out = size),
    stringsAsFactors = FALSE
  )
}

eda_cat_display_calculate <- function(source, basis) {
  if (nrow(source) == 0L) {
    return(eda_empty_categorical_display())
  }
  denominator <- switch(
    basis,
    compatibility = ifelse(
      source$is_missing_level, source$group_n, source$.n_observed
    ),
    column = source$group_n,
    overall = source$population_n,
    row = eda_cat_row_denominator(source)
  )
  denominator <- as.integer(denominator)
  proportion <- vapply(seq_len(nrow(source)), function(index) {
    summary_safe_proportion(
      source$numerator[[index]], denominator[[index]]
    )
  }, numeric(1))
  scope <- switch(
    basis,
    compatibility = ifelse(
      source$is_missing_level, "all_within_group", "observed_within_group"
    ),
    column = rep("all_within_group", nrow(source)),
    row = rep("level_across_groups", nrow(source)),
    overall = rep("analysis_population", nrow(source))
  )
  missing_treatment <- switch(
    basis,
    compatibility = ifelse(
      source$is_missing_level, "separate_level", "excluded"
    ),
    column = rep("included", nrow(source)),
    row = rep("separate_level", nrow(source)),
    overall = rep("included", nrow(source))
  )
  out <- source[, setdiff(names(source), ".n_observed"), drop = FALSE]
  out$denominator <- denominator
  out$proportion <- proportion
  out$percentage_basis <- basis
  out$denominator_scope <- scope
  out$missing_treatment <- missing_treatment
  out <- out[, eda_categorical_display_names(), drop = FALSE]
  row.names(out) <- NULL
  out
}

eda_cat_row_denominator <- function(source) {
  keys <- eda_cat_level_key(source)
  included <- !source$is_overall
  if (!any(included)) {
    return(integer(nrow(source)))
  }
  totals <- tapply(
    as.numeric(source$numerator[included]), keys[included], sum,
    simplify = TRUE
  )
  denominator <- unname(totals[keys])
  if (anyNA(denominator) || any(denominator > .Machine$integer.max)) {
    stop("Row percentage denominators did not reconcile.", call. = FALSE)
  }
  as.integer(denominator)
}

eda_cat_level_key <- function(source) {
  level <- ifelse(
    source$is_missing_level, "<missing>", paste0("<level>", source$level)
  )
  paste(source$name, level, sep = "\r")
}

eda_cat_counts_valid <- function(value) {
  is.numeric(value) && !anyNA(value) && all(is.finite(value)) &&
    all(value >= 0) && all(value == floor(value)) &&
    all(value <= .Machine$integer.max)
}

eda_categorical_display_label <- function(label, name) {
  if (length(label) == 0L || is.na(label[[1]]) ||
        !nzchar(trimws(as.character(label[[1]])))) {
    return(as.character(name[[1]]))
  }
  as.character(label[[1]])
}

eda_cat_display_frequency <- function(frequencies,
                                      name,
                                      label,
                                      type,
                                      n_total,
                                      n_missing) {
  required <- c("level", "n")
  n_observed <- as.integer(n_total - n_missing)
  valid <- is.data.frame(frequencies) && all(required %in% names(frequencies)) &&
    !anyNA(frequencies$level) &&
    !anyDuplicated(as.character(frequencies$level)) &&
    eda_cat_counts_valid(frequencies$n) &&
    eda_cat_counts_valid(n_total) &&
    eda_cat_counts_valid(n_missing) &&
    n_missing <= n_total && sum(as.numeric(frequencies$n)) == n_observed
  if (!valid) {
    stop("Categorical frequency aggregates did not reconcile.", call. = FALSE)
  }
  if (nrow(frequencies) == 0L) {
    return(eda_empty_categorical_display())
  }
  source <- eda_cat_display_source_rows(
    variable_order = 1L,
    level_order = seq_len(nrow(frequencies)),
    name = name,
    label = eda_categorical_display_label(label, name),
    type = type,
    level = frequencies$level,
    group_id = ".overall",
    group_order = 1L,
    group_label = "Overall",
    is_overall = TRUE,
    group_n = n_total,
    population_n = n_total,
    numerator = frequencies$n,
    n_observed = n_observed,
    is_missing_level = FALSE
  )
  eda_cat_display_calculate(source, "compatibility")
}
