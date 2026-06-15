#' Read an EDA specification
#'
#' Read and validate a specification-first EDA data dictionary.
#'
#' @param path_or_data A path to a CSV specification file, or a data frame
#'   containing the specification.
#'
#' @return A validated data frame specification.
#'
#' @export
epi_eda_spec <- function(path_or_data) {
  if (is.character(path_or_data) && length(path_or_data) == 1) {
    spec <- utils::read.csv(
      path_or_data,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(path_or_data)) {
    spec <- as.data.frame(path_or_data, stringsAsFactors = FALSE)
  } else {
    stop("EDA specification must be a CSV path or a data frame.", call. = FALSE)
  }

  epi_eda_validate_spec(spec)
}

#' Validate an EDA specification
#'
#' Validate the columns and basic values required by a specification-first EDA
#' data dictionary.
#'
#' @param spec A data frame containing an EDA specification.
#'
#' @return The validated specification as a data frame.
#'
#' @export
epi_eda_validate_spec <- function(spec) {
  if (!is.data.frame(spec)) {
    stop("EDA specification must be a data frame.", call. = FALSE)
  }

  spec <- as.data.frame(spec, stringsAsFactors = FALSE)
  required_cols <- c("name", "label", "type", "role")
  missing_cols <- setdiff(required_cols, names(spec))

  if (length(missing_cols) > 0) {
    stop(
      "EDA specification is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  spec$name <- as.character(spec$name)
  empty_names <- is.na(spec$name) | trimws(spec$name) == ""

  if (any(empty_names)) {
    stop("EDA specification variable names must be non-empty.", call. = FALSE)
  }

  if (anyDuplicated(spec$name)) {
    stop("Duplicate variable name found in EDA specification.", call. = FALSE)
  }

  allowed_types <- c(
    "numeric",
    "integer",
    "categorical",
    "binary",
    "date",
    "datetime",
    "text"
  )
  spec$type <- tolower(as.character(spec$type))
  invalid_type <- is.na(spec$type) | !(spec$type %in% allowed_types)

  if (any(invalid_type)) {
    stop(
      "Invalid type in EDA specification: ",
      paste(unique(spec$type[invalid_type]), collapse = ", "),
      call. = FALSE
    )
  }

  if ("required" %in% names(spec)) {
    spec$required <- parse_eda_spec_logical(spec$required)
  }

  validate_eda_spec_ranges(spec)
  spec
}

parse_eda_spec_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  values <- trimws(tolower(as.character(x)))
  missing_values <- is.na(x) | values == ""
  out <- rep(NA, length(values))
  out[values %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[values %in% c("false", "f", "0", "no", "n")] <- FALSE
  invalid <- is.na(out) & !missing_values

  if (any(invalid)) {
    stop("Invalid required value in EDA specification.", call. = FALSE)
  }

  out
}

validate_eda_spec_ranges <- function(spec) {
  if (!all(c("min", "max") %in% names(spec))) {
    return(invisible(TRUE))
  }

  min_value <- suppressWarnings(as.numeric(spec$min))
  max_value <- suppressWarnings(as.numeric(spec$max))
  comparable <- !is.na(min_value) & !is.na(max_value)

  if (any(comparable & min_value > max_value)) {
    stop("EDA specification min must not be greater than max.", call. = FALSE)
  }

  invisible(TRUE)
}
