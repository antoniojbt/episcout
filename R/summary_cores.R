summary_missing_mask <- function(values, missing_codes = character()) {
  missing <- is.na(values)
  if (length(missing_codes) == 0L) {
    return(missing)
  }
  values_chr <- as.character(values)
  codes_chr <- as.character(missing_codes)
  missing | (!is.na(values_chr) & values_chr %in% codes_chr)
}

summary_safe_proportion <- function(numerator, denominator) {
  if (length(denominator) != 1L || is.na(denominator) || denominator == 0L) {
    return(rep(NA_real_, length(numerator)))
  }
  as.numeric(numerator) / denominator
}

summary_safe_scalar <- function(value) {
  value <- tryCatch(
    suppressWarnings(value),
    error = function(e) NA_real_
  )
  if (length(value) == 0L || is.nan(value[[1]])) {
    return(NA_real_)
  }
  as.numeric(value[[1]])
}

summary_numeric_core <- function(values, missing_codes = character(), coef = 1.5, ...) {
  if (!is.numeric(values) || inherits(values, c("Date", "POSIXt"))) {
    stop("values must be a numeric vector.", call. = FALSE)
  }
  if (!is.numeric(coef) || length(coef) != 1L || is.na(coef) || coef < 0) {
    stop("coef must be a single non-negative numeric value.", call. = FALSE)
  }

  missing <- summary_missing_mask(values, missing_codes)
  observed <- values[!missing]
  infinite <- !is.finite(observed)
  finite <- as.numeric(observed[!infinite])
  n_finite <- length(finite)

  if (n_finite > 0L) {
    q1 <- summary_safe_scalar(stats::quantile(finite, 0.25, names = FALSE, type = 7))
    q3 <- summary_safe_scalar(stats::quantile(finite, 0.75, names = FALSE, type = 7))
    mean_value <- mean(finite)
    median_value <- stats::median(finite)
    iqr_value <- q3 - q1
    lower_fence <- q1 - coef * iqr_value
    upper_fence <- q3 + coef * iqr_value
  } else {
    q1 <- q3 <- mean_value <- median_value <- iqr_value <- NA_real_
    lower_fence <- upper_fence <- NA_real_
  }

  sd_value <- if (n_finite >= 2L) stats::sd(finite) else NA_real_
  variance_value <- if (n_finite >= 2L) stats::var(finite) else NA_real_
  sem_value <- if (n_finite >= 2L) sd_value / sqrt(n_finite) else NA_real_
  cv_value <- if (!is.na(mean_value) && mean_value != 0) sd_value / mean_value else NA_real_
  has_variation <- n_finite >= 2L && !is.na(sd_value) && sd_value > 0
  skewness_value <- if (n_finite >= 3L && has_variation && requireNamespace("e1071", quietly = TRUE)) {
    summary_safe_scalar(e1071::skewness(finite, na.rm = TRUE, ...))
  } else {
    NA_real_
  }
  kurtosis_value <- if (n_finite >= 3L && has_variation && requireNamespace("e1071", quietly = TRUE)) {
    summary_safe_scalar(e1071::kurtosis(finite, na.rm = TRUE, ...))
  } else {
    NA_real_
  }
  shapiro_value <- if (n_finite > 3L && n_finite < 5000L && has_variation) {
    summary_safe_scalar(stats::shapiro.test(finite)$p.value)
  } else {
    NA_real_
  }
  n_below <- if (n_finite > 0L && coef > 0) sum(finite < lower_fence) else 0L
  n_above <- if (n_finite > 0L && coef > 0) sum(finite > upper_fence) else 0L
  outlier_count <- as.integer(n_below + n_above)

  data.frame(
    n = as.integer(length(values)),
    n_missing = as.integer(sum(missing)),
    n_observed = as.integer(sum(!missing)),
    n_infinite = as.integer(sum(infinite)),
    n_finite = as.integer(n_finite),
    sum = if (n_finite > 0L) sum(finite) else 0,
    min = if (n_finite > 0L) min(finite) else NA_real_,
    q1 = q1,
    mean = mean_value,
    median = median_value,
    q3 = q3,
    max = if (n_finite > 0L) max(finite) else NA_real_,
    iqr = iqr_value,
    sd = sd_value,
    variance = variance_value,
    sem = sem_value,
    cv = cv_value,
    skewness = skewness_value,
    kurtosis = kurtosis_value,
    shapiro_p = shapiro_value,
    lower_fence = lower_fence,
    upper_fence = upper_fence,
    n_below_lower = as.integer(n_below),
    n_above_upper = as.integer(n_above),
    outlier_count = outlier_count,
    outlier_percentage = summary_safe_proportion(outlier_count * 100, n_finite),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

summary_categorical_core <- function(values, missing_codes = character(), declared_levels = NULL) {
  missing <- summary_missing_mask(values, missing_codes)
  observed <- as.character(values[!missing])
  factor_levels <- if (is.factor(values)) levels(values) else character()
  has_declared <- !is.null(declared_levels) || length(factor_levels) > 0L
  declared <- if (!is.null(declared_levels)) as.character(declared_levels) else factor_levels
  declared <- unique(declared[!is.na(declared) & nzchar(declared)])
  unexpected <- sort(setdiff(unique(observed), declared))
  levels_out <- if (has_declared) c(declared, unexpected) else sort(unique(observed))

  if (length(levels_out) == 0L) {
    return(data.frame(
      level = character(),
      n = integer(),
      p_total = numeric(),
      p_observed = numeric(),
      is_declared = logical(),
      is_unexpected = logical(),
      stringsAsFactors = FALSE
    ))
  }

  counts <- vapply(levels_out, function(level) sum(observed == level), integer(1))
  is_declared <- if (has_declared) levels_out %in% declared else rep(NA, length(levels_out))
  data.frame(
    level = levels_out,
    n = as.integer(counts),
    p_total = summary_safe_proportion(counts, length(values)),
    p_observed = summary_safe_proportion(counts, length(observed)),
    is_declared = is_declared,
    is_unexpected = if (has_declared) !is_declared else rep(FALSE, length(levels_out)),
    stringsAsFactors = FALSE
  )
}

summary_text_core <- function(values, missing_codes = character()) {
  if (!is.character(values) && !is.factor(values)) {
    stop("values must be a character or factor vector.", call. = FALSE)
  }
  missing <- summary_missing_mask(values, missing_codes)
  observed <- as.character(values[!missing])
  lengths <- nchar(observed, type = "chars")
  data.frame(
    n = as.integer(length(values)),
    n_missing = as.integer(sum(missing)),
    n_observed = as.integer(length(observed)),
    n_unique = as.integer(length(unique(observed))),
    n_empty = as.integer(sum(observed == "")),
    n_whitespace = as.integer(sum(observed != "" & trimws(observed) == "")),
    min_length = if (length(lengths) > 0L) as.integer(min(lengths)) else NA_integer_,
    max_length = if (length(lengths) > 0L) as.integer(max(lengths)) else NA_integer_,
    stringsAsFactors = FALSE
  )
}

summary_temporal_timezone <- function(values) {
  timezone <- attr(values, "tzone")
  if (length(timezone) == 0L || is.na(timezone[[1]]) || !nzchar(timezone[[1]])) {
    return("UTC")
  }
  as.character(timezone[[1]])
}

summary_as_date_vector <- function(values) {
  if (!inherits(values, c("Date", "IDate"))) {
    stop("Input must be a vector of type Date or IDate.", call. = FALSE)
  }
  as.Date(values)
}

summary_parse_datetime_chr <- function(values) {
  formats <- c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%dT%H:%M:%OS"
  )
  parsed <- as.POSIXct(rep(NA_real_, length(values)), origin = "1970-01-01", tz = "UTC")
  remaining <- seq_along(values)
  for (format_value in formats) {
    if (length(remaining) == 0L) {
      break
    }
    candidate <- suppressWarnings(as.POSIXct(values[remaining], format = format_value, tz = "UTC"))
    accepted <- !is.na(candidate)
    parsed[remaining[accepted]] <- candidate[accepted]
    remaining <- remaining[!accepted]
  }
  parsed
}

summary_format_datetime <- function(values) {
  ifelse(
    is.na(values),
    NA_character_,
    format(as.POSIXct(values, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

summary_temporal_core <- function(values, type, missing_codes = character()) {
  if (!type %in% c("date", "datetime")) {
    stop("type must be date or datetime.", call. = FALSE)
  }
  source_class <- paste(class(values), collapse = "/")
  missing <- summary_missing_mask(values, missing_codes)
  observed <- values[!missing]

  if (type == "date") {
    if (inherits(observed, "IDate")) {
      parsed <- as.Date(observed)
    } else if (inherits(observed, "Date")) {
      parsed <- observed
    } else if (is.character(observed)) {
      parsed <- suppressWarnings(as.Date(observed, format = "%Y-%m-%d"))
    } else {
      stop("Observed class is incompatible with declared date type.", call. = FALSE)
    }
    timezone <- NA_character_
    unit <- "days"
    numeric_values <- as.numeric(parsed)
    formatter <- function(x) ifelse(is.na(x), NA_character_, format(as.Date(x, origin = "1970-01-01"), "%Y-%m-%d"))
  } else {
    if (inherits(observed, "POSIXt")) {
      timezone <- summary_temporal_timezone(values)
      parsed <- as.POSIXct(observed, tz = timezone)
    } else if (is.character(observed)) {
      timezone <- "UTC"
      parsed <- summary_parse_datetime_chr(observed)
    } else {
      stop("Observed class is incompatible with declared datetime type.", call. = FALSE)
    }
    unit <- "seconds"
    numeric_values <- as.numeric(parsed)
    formatter <- summary_format_datetime
  }

  if (any(is.na(parsed)) && length(observed) > 0L) {
    stop("Temporal variable contains invalid non-missing values.", call. = FALSE)
  }

  n_observed <- length(parsed)
  if (n_observed > 0L) {
    quantiles <- as.numeric(stats::quantile(numeric_values, c(0, 0.25, 0.5, 0.75, 1), names = FALSE, type = 7))
    mode_table <- table(numeric_values)
    most_common <- as.numeric(names(mode_table)[which.max(mode_table)])
  } else {
    quantiles <- rep(NA_real_, 5L)
    most_common <- NA_real_
  }

  data.frame(
    source_class = source_class,
    timezone = timezone,
    n = as.integer(length(values)),
    n_missing = as.integer(sum(missing)),
    n_observed = as.integer(n_observed),
    n_unique = as.integer(length(unique(numeric_values))),
    min = formatter(quantiles[[1]]),
    q1 = formatter(quantiles[[2]]),
    median = formatter(quantiles[[3]]),
    q3 = formatter(quantiles[[4]]),
    max = formatter(quantiles[[5]]),
    iqr_value = if (n_observed > 0L) quantiles[[4]] - quantiles[[2]] else NA_real_,
    most_common = formatter(most_common),
    range_value = if (n_observed > 0L) quantiles[[5]] - quantiles[[1]] else NA_real_,
    range_unit = unit,
    stringsAsFactors = FALSE
  )
}

summary_infer_type <- function(values) {
  if (inherits(values, c("Date", "IDate"))) {
    return("date")
  }
  if (inherits(values, c("POSIXct", "POSIXlt"))) {
    return("datetime")
  }
  if (is.integer(values)) {
    return("integer")
  }
  if (is.numeric(values)) {
    return("numeric")
  }
  if (is.logical(values)) {
    return("binary")
  }
  if (is.factor(values)) {
    return("categorical")
  }
  if (is.character(values)) {
    return("text")
  }
  "unsupported"
}
