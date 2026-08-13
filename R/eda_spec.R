#' Read an EDA specification
#'
#' Read and validate a specification-first EDA data dictionary. Specifications declare `database_type` for the storage family and `analysis_type` for R/EDA handling; the former `type` field is not accepted.
#'
#' @param path_or_data A path to a CSV specification file, or a data frame containing the specification.
#'
#' @return A validated data frame specification.
#'
#' @export
epi_eda_spec <- function(path_or_data) {
  if (is.character(path_or_data) && length(path_or_data) == 1) {
    spec <- utils::read.csv(
      path_or_data,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = character()
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
  deprecated <- intersect(names(spec), eda_removed_scaffold_fields())
  if (length(deprecated) > 0L) {
    stop(
      "This EDA specification uses the removed evidence/review scaffold schema (",
      paste(deprecated, collapse = ", "),
      "). Regenerate it with epi_eda_spec_scaffold() and copy only semantic fields.",
      call. = FALSE
    )
  }
  if ("type" %in% names(spec)) {
    stop("EDA specification field 'type' was removed; use database_type and analysis_type.", call. = FALSE)
  }
  required_cols <- c("name", "label", "database_type", "analysis_type", "role")
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

  allowed_database_types <- c("numeric", "integer", "boolean", "date", "datetime", "text")
  allowed_analysis_types <- c(
    "numeric",
    "integer",
    "categorical",
    "binary",
    "date",
    "datetime",
    "text"
  )
  spec$database_type <- tolower(as.character(spec$database_type))
  invalid_database_type <- is.na(spec$database_type) | !(spec$database_type %in% allowed_database_types)

  if (any(invalid_database_type)) {
    stop(
      "Invalid database_type in EDA specification: ",
      paste(unique(spec$database_type[invalid_database_type]), collapse = ", "),
      call. = FALSE
    )
  }
  spec$analysis_type <- tolower(as.character(spec$analysis_type))
  invalid_analysis_type <- is.na(spec$analysis_type) | !(spec$analysis_type %in% allowed_analysis_types)
  if (any(invalid_analysis_type)) {
    stop("Invalid analysis_type in EDA specification: ", paste(unique(spec$analysis_type[invalid_analysis_type]), collapse = ", "), call. = FALSE)
  }

  character_fields <- intersect(
    c(
      "label", "role", "units", "levels", "min", "max",
      "missing_codes", "group", "description"
    ),
    names(spec)
  )
  for (field in character_fields) {
    value <- as.character(spec[[field]])
    value[is.na(value)] <- ""
    spec[[field]] <- value
  }

  if ("required" %in% names(spec)) {
    spec$required <- parse_eda_spec_logical(spec$required)
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
  if (any(!(spec$analysis_type[reviewed] %in% c("numeric", "integer")))) {
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
  epi_geo_crs(eda_geo_crs_value(crs))
}

eda_geo_crs_value <- function(crs) {
  if (grepl("^[0-9]+$", crs)) suppressWarnings(as.numeric(crs)) else crs
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

eda_removed_scaffold_fields <- function() {
  c(
    "observed_class",
    "n", "n_missing", "n_observed", "n_unique",
    "candidate_type",
    "candidate_levels",
    "review_status",
    "review_reason"
  )
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
