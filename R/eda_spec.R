#' Read an EDA specification
#'
#' Read and validate a specification-first EDA data dictionary.
#'
#' @param path_or_data A path to a CSV specification file, or a data frame containing the specification.
#'
#' @return A validated data frame specification.
#'
#' @export
epi_eda_spec <- function(path_or_data) {
  if (is.character(path_or_data) && length(path_or_data) == 1) {
    header <- utils::read.csv(
      path_or_data,
      nrows = 0L,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (is_eda_scaffold_spec(header)) {
      spec <- utils::read.csv(
        path_or_data,
        check.names = FALSE,
        stringsAsFactors = FALSE,
        colClasses = "character",
        na.strings = character()
      )
      if ("required" %in% names(spec)) {
        serialized_missing <- trimws(tolower(spec$required)) == "na"
        spec$required[serialized_missing] <- NA_character_
      }
    } else {
      spec <- utils::read.csv(
        path_or_data,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  } else if (is.data.frame(path_or_data)) {
    spec <- as.data.frame(path_or_data, stringsAsFactors = FALSE)
  } else {
    stop("EDA specification must be a CSV path or a data frame.", call. = FALSE)
  }

  epi_eda_validate_spec(spec)
}

#' Validate an EDA specification
#'
#' Validate the columns and basic values required by a specification-first EDA data dictionary.
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

  if (is_eda_scaffold_spec(spec)) {
    for (name in eda_scaffold_count_cols()) {
      spec[[name]] <- parse_eda_spec_integer(spec[[name]], name)
    }
    validate_eda_scaffold_counts(spec)
  }

  spec <- validate_eda_geo_spec(spec)
  validate_eda_spec_ranges(spec)
  spec
}

eda_geo_spec_fields <- function() {
  c("geo_role", "geo_pair", "geo_crs")
}

validate_eda_geo_spec <- function(spec) {
  fields <- eda_geo_spec_fields()
  present <- fields %in% names(spec)
  if (!any(present)) {
    return(spec)
  }
  if (!all(present)) {
    stop(
      "EDA coordinate metadata requires geo_role, geo_pair and geo_crs together.",
      call. = FALSE
    )
  }
  for (field in fields) {
    value <- as.character(spec[[field]])
    value[is.na(value)] <- ""
    spec[[field]] <- trimws(value)
  }
  spec$geo_role <- tolower(spec$geo_role)
  invalid_role <- !(spec$geo_role %in% c("", "x", "y"))
  if (any(invalid_role)) {
    stop("EDA geo_role must be blank, x or y.", call. = FALSE)
  }
  blank_role <- spec$geo_role == ""
  orphan <- blank_role & (spec$geo_pair != "" | spec$geo_crs != "")
  incomplete <- !blank_role & (spec$geo_pair == "" | spec$geo_crs == "")
  if (any(orphan | incomplete)) {
    stop("EDA coordinate metadata must be blank or complete on each row.", call. = FALSE)
  }
  reviewed <- which(!blank_role)
  if (length(reviewed) == 0L) {
    return(spec)
  }
  if (any(!(spec$type[reviewed] %in% c("numeric", "integer")))) {
    stop("EDA coordinate roles require numeric or integer declared types.", call. = FALSE)
  }
  epi_geo_require("sf")
  pairs <- unique(spec$geo_pair[reviewed])
  for (pair in pairs) {
    rows <- which(spec$geo_pair == pair & !blank_role)
    roles <- spec$geo_role[rows]
    if (length(rows) != 2L || !identical(sort(roles), c("x", "y"))) {
      stop("Each EDA geo_pair must contain exactly one x row and one y row.", call. = FALSE)
    }
    crs <- unique(spec$geo_crs[rows])
    if (length(crs) != 1L) {
      stop("Rows in an EDA geo_pair must use the same explicit geo_crs.", call. = FALSE)
    }
    eda_geo_resolve_crs(crs)
  }
  spec
}

eda_geo_resolve_crs <- function(crs) {
  value <- if (grepl("^[0-9]+$", crs)) suppressWarnings(as.numeric(crs)) else crs
  epi_geo_crs(value)
}

parse_eda_spec_integer <- function(x, name) {
  values <- suppressWarnings(as.numeric(x))
  invalid <- !is.na(x) & trimws(as.character(x)) != "" &
    (
      is.na(values) | !is.finite(values) | values != floor(values) |
        values < 0 | values > .Machine$integer.max
    )
  if (any(invalid)) {
    stop("Invalid ", name, " value in EDA specification.", call. = FALSE)
  }
  as.integer(values)
}

eda_scaffold_count_cols <- function() {
  c("n", "n_missing", "n_observed", "n_unique")
}

eda_scaffold_evidence_cols <- function() {
  c(
    "observed_class",
    eda_scaffold_count_cols(),
    "candidate_type",
    "candidate_levels",
    "review_status",
    "review_reason"
  )
}

is_eda_scaffold_spec <- function(spec) {
  all(eda_scaffold_evidence_cols() %in% names(spec))
}

validate_eda_scaffold_counts <- function(spec) {
  counts <- spec[eda_scaffold_count_cols()]
  if (anyNA(counts)) {
    stop("EDA scaffold count fields must be complete.", call. = FALSE)
  }
  if (any(spec$n_missing + spec$n_observed != spec$n)) {
    stop("EDA scaffold missing and observed counts must reconcile with n.", call. = FALSE)
  }
  if (any(spec$n_unique > spec$n_observed)) {
    stop("EDA scaffold unique counts must not exceed observed counts.", call. = FALSE)
  }
  invisible(TRUE)
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
