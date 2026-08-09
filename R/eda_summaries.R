#' Profile summaries using an EDA specification
#'
#' Produce one canonical, machine-readable descriptive summary for every variable listed in a specification-first EDA data dictionary. Standard `NA` values and configured `missing_codes` are excluded from observed summaries.
#'
#' @param data A data frame or an [epi_eda_postgres_source()] containing
#'   observed data.
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A named list containing `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped` data frames. The `variables` table records every specification row and its summary status; the remaining tables contain type-specific results or explicit skipped reasons.
#'
#' @export
epi_eda_profile_summaries <- function(data, spec) {
  if (inherits(data, "epi_eda_postgres_source")) {
    spec <- epi_eda_spec(spec)
    return(eda_postgres_transaction(
      data,
      eda_postgres_summaries_inside(data, spec)
    ))
  }
  if (!is.data.frame(data)) {
    stop("EDA data must be a data frame or an epi_eda_postgres_source.", call. = FALSE)
  }
  spec <- epi_eda_spec(spec)
  build_typed_summaries(data, spec)
}

build_typed_summaries <- function(data, spec, global_missing_codes = NULL) {
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
    required <- if ("required" %in% names(row)) as.logical(row$required[[1]]) else NA
    type <- as.character(row$type[[1]])

    if (!name %in% names(data)) {
      reason <- missing_variable_reason(required)
      outputs$variables[[length(outputs$variables) + 1L]] <- canonical_variable_row(
        name,
        label,
        type,
        role,
        required,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        "skipped",
        reason
      )
      outputs$skipped[[length(outputs$skipped) + 1L]] <- canonical_skipped_row(
        name,
        type,
        NA_character_,
        reason
      )
      next
    }

    values <- data[[name]]
    codes <- if (is.null(global_missing_codes)) eda_missing_codes(spec, name) else global_missing_codes
    missing <- summary_missing_mask(values, codes)
    observed <- values[!missing]
    n_infinite <- if (is.numeric(observed) && !inherits(observed, c("Date", "POSIXt"))) {
      sum(!is.finite(observed))
    } else {
      0L
    }
    result <- tryCatch(
      dispatch_typed_summary(values, type, codes, row),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      reason <- conditionMessage(result)
      outputs$variables[[length(outputs$variables) + 1L]] <- canonical_variable_row(
        name,
        label,
        type,
        role,
        required,
        length(values),
        sum(missing),
        length(observed),
        length(unique(as.character(observed))),
        n_infinite,
        "skipped",
        reason
      )
      outputs$skipped[[length(outputs$skipped) + 1L]] <- canonical_skipped_row(
        name,
        type,
        paste(class(values), collapse = "/"),
        reason
      )
      next
    }

    outputs$variables[[length(outputs$variables) + 1L]] <- canonical_variable_row(
      name,
      label,
      type,
      role,
      required,
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
    variables = bind_or_empty(outputs$variables, empty_eda_variables()),
    numeric = bind_or_empty(outputs$numeric, empty_eda_numeric()),
    categorical = bind_or_empty(outputs$categorical, empty_eda_categorical()),
    text = bind_or_empty(outputs$text, empty_eda_text()),
    temporal = bind_or_empty(outputs$temporal, empty_eda_temporal()),
    skipped = bind_or_empty(outputs$skipped, empty_eda_skipped())
  )
}

missing_variable_reason <- function(required) {
  if (isTRUE(required)) {
    return("Required specified variable was not found in data.")
  }
  if (identical(required, FALSE)) {
    return("Optional specified variable was not found in data.")
  }
  "Specified variable was not found in data; required status was not supplied."
}

dispatch_typed_summary <- function(values, type, missing_codes, spec_row) {
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
      data = summary_categorical_core(
        values,
        missing_codes,
        if (length(levels) > 0L) levels else NULL
      )
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
      values <- if (type == "date") {
        as.Date(rep(NA_real_, length(values)), origin = "1970-01-01")
      } else {
        as.POSIXct(rep(NA_real_, length(values)), origin = "1970-01-01", tz = "UTC")
      }
    }
    core <- summary_temporal_core(values, type, missing_codes)
    return(list(component = "temporal", data = core[, c(
      "source_class", "timezone", "n", "n_missing", "n_observed", "n_unique", "min", "q1", "median", "q3", "max", "range_value", "range_unit"
    ), drop = FALSE]))
  }
  stop("Observed class is unsupported for typed summaries.", call. = FALSE)
}

canonical_variable_row <- function(name,
                                   label,
                                   type,
                                   role,
                                   required,
                                   n,
                                   n_missing,
                                   n_observed,
                                   n_unique,
                                   n_infinite,
                                   status,
                                   reason) {
  data.frame(
    name = name,
    label = label,
    type = type,
    role = role,
    required = as.logical(required),
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

canonical_skipped_row <- function(name, type, observed_class, reason) {
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

empty_eda_variables <- function() {
  canonical_variable_row(
    character(), character(), character(), character(), logical(), integer(),
    integer(), integer(), integer(), integer(), character(), character()
  )
}

empty_eda_numeric <- function() {
  core <- summary_numeric_core(numeric())[0, c(
    "n_finite", "sum", "min", "q1", "mean", "median", "q3", "max", "iqr", "sd", "variance", "sem", "cv", "skewness", "kurtosis", "shapiro_p", "lower_fence", "upper_fence", "n_below_lower", "n_above_upper", "outlier_count", "outlier_percentage"
  )]
  cbind(data.frame(name = character()), core)
}

empty_eda_categorical <- function() {
  core <- summary_categorical_core(character())[0, ]
  cbind(data.frame(name = character()), core)
}

empty_eda_text <- function() {
  core <- summary_text_core(character())[0, ]
  cbind(data.frame(name = character()), core)
}

empty_eda_temporal <- function() {
  core <- summary_temporal_core(as.Date(character()), "date")[0, c(
    "source_class", "timezone", "n", "n_missing", "n_observed", "n_unique", "min", "q1", "median", "q3", "max", "range_value", "range_unit"
  )]
  cbind(data.frame(name = character()), core)
}

empty_eda_skipped <- function() {
  canonical_skipped_row(character(), character(), character(), character())
}

eda_summary_exclusions <- function(data, spec) {
  stats::setNames(character(), character())
}

eda_apply_summary_exclusions <- function(canonical, data, spec, exclusions) {
  excluded <- intersect(names(exclusions), spec$name)
  if (length(excluded) == 0L) {
    return(canonical)
  }
  hit <- canonical$variables$name %in% excluded
  canonical$variables$status[hit] <- "skipped"
  canonical$variables$reason[hit] <- unname(exclusions[canonical$variables$name[hit]])
  for (component in c("numeric", "categorical", "text", "temporal")) {
    canonical[[component]] <- canonical[[component]][
      !canonical[[component]]$name %in% excluded, ,
      drop = FALSE
    ]
  }
  canonical$skipped <- canonical$skipped[
    !canonical$skipped$name %in% excluded, ,
    drop = FALSE
  ]
  for (name in excluded) {
    observed_class <- if (name %in% names(data)) {
      paste(class(data[[name]]), collapse = "/")
    } else {
      NA_character_
    }
    canonical$skipped <- rbind(
      canonical$skipped,
      canonical_skipped_row(
        name, spec$type[match(name, spec$name)], observed_class,
        unname(exclusions[[name]])
      )
    )
  }
  row.names(canonical$skipped) <- NULL
  canonical
}

eda_spec_levels <- function(levels_value) {
  if (length(levels_value) == 0L || is.na(levels_value[[1]])) {
    return(character())
  }
  levels <- trimws(strsplit(as.character(levels_value[[1]]), ";", fixed = TRUE)[[1]])
  levels[nzchar(levels)]
}
