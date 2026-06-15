#' Profile summaries using an EDA specification
#'
#' Produce machine-readable descriptive summaries for variables listed in a
#' specification-first EDA data dictionary. Standard `NA` values and configured
#' `missing_codes` are excluded from observed summaries.
#'
#' @param data A data frame containing observed data.
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A named list with `numeric` and `categorical` data frames. In
#'   categorical summaries, `p` uses total rows as the denominator and
#'   `p_observed` uses observed non-missing rows.
#'
#' @export
epi_eda_profile_summaries <- function(data, spec) {
  if (!is.data.frame(data)) {
    stop("EDA data must be a data frame.", call. = FALSE)
  }

  spec <- epi_eda_spec(spec)
  missing_vars <- setdiff(spec$name, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "EDA data is missing specified variables: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    numeric = profile_summaries_numeric(data, spec),
    categorical = profile_summaries_categorical(data, spec)
  )
}

profile_summaries_numeric <- function(data, spec) {
  numeric_spec <- spec[spec$type %in% c("numeric", "integer"), , drop = FALSE]

  if (nrow(numeric_spec) == 0) {
    return(data.frame(
      name = character(),
      n = integer(),
      n_missing = integer(),
      mean = numeric(),
      sd = numeric(),
      median = numeric(),
      min = numeric(),
      max = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(numeric_spec$name, function(name) {
    values <- data[[name]]
    missing <- eda_missing_mask(values, eda_missing_codes(spec, name))
    observed <- values[!missing]
    data.frame(
      name = name,
      n = length(values),
      n_missing = sum(missing),
      mean = eda_safe_numeric_summary(observed, mean),
      sd = eda_safe_numeric_summary(observed, stats::sd),
      median = eda_safe_numeric_summary(observed, stats::median),
      min = eda_safe_numeric_summary(observed, min),
      max = eda_safe_numeric_summary(observed, max),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

profile_summaries_categorical <- function(data, spec) {
  categorical_spec <- spec[spec$type %in% c("categorical", "binary"), , drop = FALSE]

  if (nrow(categorical_spec) == 0) {
    return(data.frame(
      name = character(),
      level = character(),
      n = integer(),
      p = numeric(),
      p_observed = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(categorical_spec$name, function(name) {
    values <- data[[name]]
    missing <- eda_missing_mask(values, eda_missing_codes(spec, name))
    observed <- as.character(values[!missing])
    levels_value <- categorical_spec$levels[categorical_spec$name == name]
    levels <- eda_spec_levels(levels_value)
    if (length(levels) == 0) {
      levels <- sort(unique(observed))
    }
    counts <- vapply(levels, function(level) sum(observed == level), integer(1))
    total_n <- length(values)
    observed_n <- length(observed)

    data.frame(
      name = name,
      level = levels,
      n = as.integer(counts),
      p = eda_safe_proportion(counts, total_n),
      p_observed = eda_safe_proportion(counts, observed_n),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

eda_safe_numeric_summary <- function(values, fun) {
  if (length(values) == 0) {
    return(NA_real_)
  }

  value <- tryCatch(
    suppressWarnings(fun(values)),
    error = function(e) NA_real_
  )
  if (length(value) == 0 || is.nan(value)) {
    return(NA_real_)
  }

  as.numeric(value)
}

eda_safe_proportion <- function(counts, denominator) {
  if (denominator == 0) {
    return(rep(NA_real_, length(counts)))
  }

  as.numeric(counts) / denominator
}

eda_spec_levels <- function(levels_value) {
  if (length(levels_value) == 0 || is.na(levels_value[[1]])) {
    return(character())
  }

  levels <- trimws(strsplit(as.character(levels_value[[1]]), ";", fixed = TRUE)[[1]])
  levels[nzchar(levels)]
}
