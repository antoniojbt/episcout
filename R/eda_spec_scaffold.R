#' Scaffold an EDA specification from existing data
#'
#' Create a conservative, editable EDA specification from the storage classes and aggregate structure of an existing data frame. The result is a draft for human review: it does not infer scientific roles, privacy status, sentinel missing values, units or validation ranges, and it never includes observed values as examples or candidate levels.
#'
#' @param data A data frame or data-frame subclass. File reading is deliberately outside this function.
#' @param max_candidate_levels A positive whole number used to flag non-empty low-cardinality integer and character columns for review. It never changes the initial type or enumerates observed values.
#'
#' @return An ordinary data frame with one row per source column in source order. It contains the core EDA specification fields plus aggregate review evidence. Every row has `review_status = "review_required"`; the result is not approved for analysis or sharing until it has been reviewed.
#'
#' @details Factor levels and fixed logical levels are storage metadata and are copied to the core `levels` field. Factor metadata that cannot be represented safely by the package's semicolon-delimited specification format is refused. `candidate_levels` is reserved and remains blank so low-cardinality values are not disclosed.
#'
#' @export
epi_eda_spec_scaffold <- function(data, max_candidate_levels = 20L) {
  validate_eda_scaffold_data(data)
  max_candidate_levels <- validate_scaffold_max_levels(max_candidate_levels)
  validate_eda_scaffold_names(names(data))
  validate_eda_scaffold_columns(data)

  if (ncol(data) == 0L) {
    return(epi_eda_validate_spec(empty_eda_spec_scaffold()))
  }

  rows <- lapply(seq_along(data), function(index) {
    scaffold_eda_column(
      name = names(data)[[index]],
      values = data[[index]],
      max_candidate_levels = max_candidate_levels
    )
  })
  scaffold <- do.call(rbind, rows)
  rownames(scaffold) <- NULL
  epi_eda_validate_spec(scaffold)
}

validate_eda_scaffold_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_scaffold_max_levels <- function(max_candidate_levels) {
  valid <- is.numeric(max_candidate_levels) &&
    length(max_candidate_levels) == 1L &&
    !is.na(max_candidate_levels) &&
    is.finite(max_candidate_levels) &&
    max_candidate_levels > 0 &&
    max_candidate_levels == floor(max_candidate_levels)
  if (!valid) {
    stop("max_candidate_levels must be a positive whole number.", call. = FALSE)
  }
  max_candidate_levels
}

validate_eda_scaffold_names <- function(data_names) {
  empty <- is.na(data_names) | trimws(data_names) == ""
  if (any(empty)) {
    stop("EDA scaffold source column names must be non-empty.", call. = FALSE)
  }
  if (anyDuplicated(data_names)) {
    stop("Duplicate source column names are not supported by the EDA scaffold.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_eda_scaffold_columns <- function(data) {
  blockers <- character()
  for (index in seq_along(data)) {
    values <- data[[index]]
    name <- names(data)[[index]]
    observed_class <- scaffold_observed_class(values)
    type <- scaffold_storage_type(values)
    if (type == "unsupported") {
      blockers <- c(blockers, paste0(name, " (", observed_class, ": unsupported storage)"))
      next
    }
    if (type == "categorical" && !scaffold_factor_levels_safe(values)) {
      blockers <- c(blockers, paste0(
        name,
        " (",
        observed_class,
        ": factor level metadata cannot be encoded for a safe CSV round-trip)"
      ))
    }
  }
  if (length(blockers) > 0L) {
    stop(
      "EDA scaffold cannot represent these columns: ",
      paste(blockers, collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

scaffold_storage_type <- function(values) {
  classes <- class(values)
  if (inherits(values, c("Date", "IDate")) && all(classes %in% c("Date", "IDate"))) {
    return("date")
  }
  if (inherits(values, c("POSIXct", "POSIXlt")) && all(classes %in% c("POSIXct", "POSIXlt", "POSIXt"))) {
    return("datetime")
  }
  if (is.factor(values) && all(classes %in% c("factor", "ordered"))) {
    return("categorical")
  }
  if (is.integer(values) && identical(classes, "integer")) {
    return("integer")
  }
  if (is.double(values) && identical(classes, "numeric")) {
    return("numeric")
  }
  if (is.logical(values) && identical(classes, "logical")) {
    return("binary")
  }
  if (is.character(values) && identical(classes, "character")) {
    return("text")
  }
  "unsupported"
}

scaffold_factor_levels_safe <- function(values) {
  declared <- levels(values)
  if (length(declared) == 0L) {
    return(TRUE)
  }
  !anyNA(declared) &&
    all(nzchar(declared)) &&
    all(!grepl(";", declared, fixed = TRUE)) &&
    all(declared == trimws(declared))
}

scaffold_eda_column <- function(name, values, max_candidate_levels) {
  type <- scaffold_storage_type(values)
  missing <- summary_missing_mask(values)
  observed <- values[!missing]
  n_unique <- length(unique(as.character(observed)))
  candidate_type <- scaffold_candidate_type(
    values,
    observed,
    type,
    n_unique,
    max_candidate_levels
  )

  data.frame(
    name = name,
    label = name,
    type = type,
    role = "",
    units = "",
    levels = scaffold_declared_levels(values, type),
    min = "",
    max = "",
    missing_codes = "",
    required = NA,
    group = "",
    description = "",
    geo_role = "",
    geo_pair = "",
    geo_crs = "",
    observed_class = scaffold_observed_class(values),
    n = as.integer(length(values)),
    n_missing = as.integer(sum(missing)),
    n_observed = as.integer(length(observed)),
    n_unique = as.integer(n_unique),
    candidate_type = candidate_type,
    candidate_levels = "",
    review_status = "review_required",
    review_reason = scaffold_review_reason(
      type,
      candidate_type,
      n_unique,
      if (type == "datetime") summary_temporal_timezone(values) else ""
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

scaffold_candidate_type <- function(values, observed, type, n_unique, max_candidate_levels) {
  if (length(observed) == 0L || type %in% c("binary", "categorical", "date", "datetime")) {
    return("")
  }
  if (type == "numeric") {
    in_integer_range <- all(
      observed >= -.Machine$integer.max &
        observed <= .Machine$integer.max
    )
    if (all(is.finite(observed)) && all(observed == trunc(observed)) && in_integer_range) {
      return("integer")
    }
    return("")
  }
  if (type == "text") {
    if (eda_all_iso_dates(observed)) {
      return("date")
    }
    if (eda_all_iso_datetimes(observed)) {
      return("datetime")
    }
  }
  if (type %in% c("integer", "text")) {
    if (n_unique <= max_candidate_levels) {
      if (n_unique == 2L) {
        return("binary")
      }
      return("categorical")
    }
  }
  ""
}

scaffold_declared_levels <- function(values, type) {
  if (type == "binary") {
    return("FALSE;TRUE")
  }
  if (type != "categorical") {
    return("")
  }
  declared <- levels(values)
  if (length(declared) == 0L) {
    return("")
  }
  if (length(declared) == 1L && identical(declared, "NA")) {
    return("NA;")
  }
  paste(declared, collapse = ";")
}

scaffold_observed_class <- function(values) {
  paste(class(values), collapse = "/")
}

scaffold_review_reason <- function(type, candidate_type, n_unique, timezone) {
  storage_reason <- paste0("Initial type follows ", type, " storage.")
  candidate_reason <- if (candidate_type == "") {
    " No value-derived type candidate was recorded."
  } else if (candidate_type %in% c("binary", "categorical")) {
    paste0(" Aggregate cardinality (", n_unique, ") suggests ", candidate_type, " review only.")
  } else if (candidate_type == "integer") {
    " All observed numeric values are finite whole numbers, suggesting integer review only."
  } else {
    paste0(" Strict character shape suggests ", candidate_type, " review only.")
  }
  timezone_reason <- if (type == "datetime") {
    paste0(" Datetime timezone metadata is ", timezone, ".")
  } else if (candidate_type == "datetime") {
    " Character datetime timezone semantics require review."
  } else {
    ""
  }
  paste0(
    storage_reason,
    candidate_reason,
    timezone_reason,
    " Review semantic fields, levels, missing codes, privacy and validation limits."
  )
}

empty_eda_spec_scaffold <- function() {
  data.frame(
    name = character(),
    label = character(),
    type = character(),
    role = character(),
    units = character(),
    levels = character(),
    min = character(),
    max = character(),
    missing_codes = character(),
    required = logical(),
    group = character(),
    description = character(),
    geo_role = character(),
    geo_pair = character(),
    geo_crs = character(),
    observed_class = character(),
    n = integer(),
    n_missing = integer(),
    n_observed = integer(),
    n_unique = integer(),
    candidate_type = character(),
    candidate_levels = character(),
    review_status = character(),
    review_reason = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
