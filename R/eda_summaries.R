#' Profile summaries using an EDA specification
#'
#' Produce machine-readable descriptive summaries for variables listed in a specification-first EDA data dictionary. Standard `NA` values and configured `missing_codes` are excluded from observed summaries.
#'
#' @param data A data frame containing observed data.
#' @param spec An EDA specification data frame or CSV path.
#' @param summary_version Summary contract to return. `"v1"` preserves the original numeric and categorical tables and remains the default for one compatibility release. `"v2"` returns complete typed summary components.
#'
#' @return With `summary_version = "v1"`, a named list containing `numeric` and `categorical` data frames. With `summary_version = "v2"`, a named list containing `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped` data frames.
#'
#' @export
epi_eda_profile_summaries <- function(data, spec, summary_version = c("v1", "v2")) {
  if (!is.data.frame(data)) {
    stop("EDA data must be a data frame.", call. = FALSE)
  }
  summary_version <- match.arg(summary_version)
  spec <- epi_eda_spec(spec)

  if (summary_version == "v2") {
    return(profile_summaries_v2(data, spec))
  }

  missing_vars <- setdiff(spec$name, names(data))
  if (length(missing_vars) > 0L) {
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
  if (nrow(numeric_spec) == 0L) {
    return(empty_eda_numeric_v1())
  }

  rows <- lapply(numeric_spec$name, function(name) {
    core <- summary_numeric_core(data[[name]], eda_missing_codes(spec, name))
    data.frame(
      name = name,
      n = core$n,
      n_missing = core$n_missing,
      mean = core$mean,
      sd = core$sd,
      median = core$median,
      min = core$min,
      max = core$max,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

profile_summaries_categorical <- function(data, spec) {
  categorical_spec <- spec[spec$type %in% c("categorical", "binary"), , drop = FALSE]
  if (nrow(categorical_spec) == 0L) {
    return(empty_eda_categorical_v1())
  }

  rows <- lapply(categorical_spec$name, function(name) {
    levels <- eda_spec_levels(categorical_spec$levels[categorical_spec$name == name])
    declared <- if (length(levels) > 0L) levels else NULL
    core <- summary_categorical_core(
      data[[name]],
      eda_missing_codes(spec, name),
      declared_levels = declared
    )
    if (!is.null(declared)) {
      core <- core[core$is_declared, , drop = FALSE]
    }
    data.frame(
      name = rep(name, nrow(core)),
      level = core$level,
      n = core$n,
      p = core$p_total,
      p_observed = core$p_observed,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

empty_eda_numeric_v1 <- function() {
  data.frame(
    name = character(),
    n = integer(),
    n_missing = integer(),
    mean = numeric(),
    sd = numeric(),
    median = numeric(),
    min = numeric(),
    max = numeric(),
    stringsAsFactors = FALSE
  )
}

empty_eda_categorical_v1 <- function() {
  data.frame(
    name = character(),
    level = character(),
    n = integer(),
    p = numeric(),
    p_observed = numeric(),
    stringsAsFactors = FALSE
  )
}

profile_summaries_v2 <- function(data, spec, global_missing_codes = NULL) {
  outputs <- list(
    variables = list(),
    numeric = list(),
    categorical = list(),
    text = list(),
    temporal = list(),
    skipped = list()
  )

  for (row_index in seq_len(nrow(spec))) {
    row <- spec[row_index, , drop = FALSE]
    name <- as.character(row$name[[1]])
    label <- if ("label" %in% names(row)) as.character(row$label[[1]]) else name
    role <- if ("role" %in% names(row)) as.character(row$role[[1]]) else NA_character_
    type <- as.character(row$type[[1]])

    if (!name %in% names(data)) {
      reason <- "Specified variable was not found in data."
      outputs$variables[[length(outputs$variables) + 1L]] <- v2_variable_row(
        name, label, type, role, nrow(data), NA_integer_, NA_integer_, NA_integer_, NA_integer_, "skipped", reason
      )
      outputs$skipped[[length(outputs$skipped) + 1L]] <- v2_skipped_row(name, type, NA_character_, reason)
      next
    }

    values <- data[[name]]
    codes <- if (is.null(global_missing_codes)) eda_missing_codes(spec, name) else global_missing_codes
    missing <- summary_missing_mask(values, codes)
    observed <- values[!missing]
    n_infinite <- if (is.numeric(observed) && !inherits(observed, c("Date", "POSIXt"))) sum(!is.finite(observed)) else 0L
    result <- tryCatch(
      v2_dispatch_summary(values, type, codes, row),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      reason <- conditionMessage(result)
      outputs$variables[[length(outputs$variables) + 1L]] <- v2_variable_row(
        name,
        label,
        type,
        role,
        length(values),
        sum(missing),
        length(observed),
        length(unique(as.character(observed))),
        n_infinite,
        "skipped",
        reason
      )
      outputs$skipped[[length(outputs$skipped) + 1L]] <- v2_skipped_row(
        name,
        type,
        paste(class(values), collapse = "/"),
        reason
      )
      next
    }

    outputs$variables[[length(outputs$variables) + 1L]] <- v2_variable_row(
      name,
      label,
      type,
      role,
      length(values),
      sum(missing),
      length(observed),
      length(unique(as.character(observed))),
      n_infinite,
      "summarised",
      NA_character_
    )
    component <- result$component
    outputs[[component]][[length(outputs[[component]]) + 1L]] <- cbind(
      data.frame(name = rep(name, nrow(result$data)), stringsAsFactors = FALSE),
      result$data
    )
  }

  list(
    variables = bind_or_empty(outputs$variables, empty_eda_variables_v2()),
    numeric = bind_or_empty(outputs$numeric, empty_eda_numeric_v2()),
    categorical = bind_or_empty(outputs$categorical, empty_eda_categorical_v2()),
    text = bind_or_empty(outputs$text, empty_eda_text_v2()),
    temporal = bind_or_empty(outputs$temporal, empty_eda_temporal_v2()),
    skipped = bind_or_empty(outputs$skipped, empty_eda_skipped_v2())
  )
}

v2_dispatch_summary <- function(values, type, missing_codes, spec_row) {
  if (type %in% c("numeric", "integer")) {
    if (!is.numeric(values) && all(summary_missing_mask(values, missing_codes))) {
      values <- rep(NA_real_, length(values))
    }
    core <- summary_numeric_core(values, missing_codes)
    return(list(component = "numeric", data = core[, c(
      "n_finite", "sum", "min", "q1", "mean", "median", "q3", "max", "iqr", "sd", "variance", "sem", "cv", "skewness", "kurtosis", "shapiro_p", "lower_fence", "upper_fence", "n_below_lower", "n_above_upper", "outlier_count", "outlier_percentage"
    ), drop = FALSE]))
  }
  if (type %in% c("categorical", "binary")) {
    if (!is.atomic(values) || inherits(values, c("Date", "POSIXt"))) {
      stop("Observed class is incompatible with declared categorical type.", call. = FALSE)
    }
    levels <- if ("levels" %in% names(spec_row)) eda_spec_levels(spec_row$levels) else character()
    return(list(
      component = "categorical",
      data = summary_categorical_core(values, missing_codes, if (length(levels) > 0L) levels else NULL)
    ))
  }
  if (type == "text") {
    if (!is.character(values) && !is.factor(values) && all(summary_missing_mask(values, missing_codes))) {
      values <- rep(NA_character_, length(values))
    }
    return(list(component = "text", data = summary_text_core(values, missing_codes)))
  }
  if (type %in% c("date", "datetime")) {
    if (!inherits(values, c("Date", "IDate", "POSIXct", "POSIXlt")) && !is.character(values) && all(summary_missing_mask(values, missing_codes))) {
      values <- if (type == "date") as.Date(rep(NA_real_, length(values)), origin = "1970-01-01") else as.POSIXct(rep(NA_real_, length(values)), origin = "1970-01-01", tz = "UTC")
    }
    core <- summary_temporal_core(values, type, missing_codes)
    return(list(component = "temporal", data = core[, c(
      "source_class", "timezone", "n", "n_missing", "n_observed", "n_unique", "min", "q1", "median", "q3", "max", "range_value", "range_unit"
    ), drop = FALSE]))
  }
  stop("Observed class is unsupported for typed summaries.", call. = FALSE)
}

v2_variable_row <- function(name, label, type, role, n, n_missing, n_observed, n_unique, n_infinite, status, reason) {
  data.frame(
    name = name,
    label = label,
    type = type,
    role = role,
    n = as.integer(n),
    n_missing = as.integer(n_missing),
    n_observed = as.integer(n_observed),
    n_unique = as.integer(n_unique),
    n_infinite = as.integer(n_infinite),
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

v2_skipped_row <- function(name, type, observed_class, reason) {
  data.frame(
    name = name,
    type = type,
    observed_class = observed_class,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

bind_or_empty <- function(rows, empty) {
  if (length(rows) == 0L) {
    return(empty)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

empty_eda_variables_v2 <- function() {
  v2_variable_row(character(), character(), character(), character(), integer(), integer(), integer(), integer(), integer(), character(), character())
}

empty_eda_numeric_v2 <- function() {
  core <- summary_numeric_core(numeric())[0, c(
    "n_finite", "sum", "min", "q1", "mean", "median", "q3", "max", "iqr", "sd", "variance", "sem", "cv", "skewness", "kurtosis", "shapiro_p", "lower_fence", "upper_fence", "n_below_lower", "n_above_upper", "outlier_count", "outlier_percentage"
  )]
  cbind(data.frame(name = character()), core)
}

empty_eda_categorical_v2 <- function() {
  core <- summary_categorical_core(character())[0, ]
  cbind(data.frame(name = character()), core)
}

empty_eda_text_v2 <- function() {
  core <- summary_text_core(character())[0, ]
  cbind(data.frame(name = character()), core)
}

empty_eda_temporal_v2 <- function() {
  core <- summary_temporal_core(as.Date(character()), "date")[0, c(
    "source_class", "timezone", "n", "n_missing", "n_observed", "n_unique", "min", "q1", "median", "q3", "max", "range_value", "range_unit"
  )]
  cbind(data.frame(name = character()), core)
}

empty_eda_skipped_v2 <- function() {
  v2_skipped_row(character(), character(), character(), character())
}

eda_safe_numeric_summary <- function(values, fun) {
  if (length(values) == 0L) {
    return(NA_real_)
  }
  summary_safe_scalar(fun(values))
}

eda_safe_proportion <- function(counts, denominator) {
  summary_safe_proportion(counts, denominator)
}

eda_spec_levels <- function(levels_value) {
  if (length(levels_value) == 0L || is.na(levels_value[[1]])) {
    return(character())
  }
  levels <- trimws(strsplit(as.character(levels_value[[1]]), ";", fixed = TRUE)[[1]])
  levels[nzchar(levels)]
}
