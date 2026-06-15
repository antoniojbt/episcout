#' Profile summaries using an EDA specification
#'
#' Produce machine-readable descriptive summaries for variables listed in a
#' specification-first EDA data dictionary.
#'
#' @param data A data frame containing observed data.
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A named list with `numeric` and `categorical` data frames.
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
    data.frame(
      name = name,
      n = length(values),
      n_missing = sum(is.na(values)),
      mean = mean(values, na.rm = TRUE),
      sd = stats::sd(values, na.rm = TRUE),
      median = stats::median(values, na.rm = TRUE),
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
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
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(categorical_spec$name, function(name) {
    values <- data[[name]]
    observed <- as.character(values[!is.na(values)])
    levels_value <- categorical_spec$levels[categorical_spec$name == name]
    levels <- strsplit(levels_value, ";", fixed = TRUE)[[1]]
    counts <- vapply(levels, function(level) sum(observed == level), integer(1))

    data.frame(
      name = name,
      level = levels,
      n = as.integer(counts),
      p = as.numeric(counts) / length(values),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
