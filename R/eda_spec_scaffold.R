#' Scaffold an EDA specification from existing data
#'
#' Create an editable semantic EDA specification from the storage classes of an existing data frame. The result does not infer scientific roles, sentinel missing values, units, validation ranges or geographic meaning, and it never includes observed values or aggregate evidence.
#'
#' @param data A data frame or data-frame subclass. File reading is deliberately outside this function.
#'
#' @return An ordinary data frame with one row per source column in source order and the lean semantic EDA fields.
#'
#' @details Factor levels and fixed logical levels are storage metadata and are copied to the core `levels` field. Factor metadata that cannot be represented safely by the package's semicolon-delimited specification format is refused. episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared.
#'
#' @export
epi_eda_spec_scaffold <- function(data) {
  validate_eda_scaffold_data(data)
  validate_eda_scaffold_names(names(data))
  validate_eda_scaffold_columns(data)

  if (ncol(data) == 0L) {
    return(epi_eda_validate_spec(empty_eda_spec_scaffold()))
  }

  rows <- lapply(seq_along(data), function(index) {
    scaffold_eda_column(names(data)[[index]], data[[index]])
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

scaffold_observed_class <- function(values) {
  paste(class(values), collapse = "/")
}

scaffold_eda_column <- function(name, values) {
  type <- scaffold_storage_type(values)

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
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
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
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
