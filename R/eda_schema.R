#' Check observed data against an EDA specification
#'
#' Compare expected variables in an EDA specification with variables observed in
#' a data frame.
#'
#' @param data A data frame to check.
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A data frame with one row per expected or unexpected variable.
#'
#' @export
epi_eda_check_schema <- function(data, spec) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.", call. = FALSE)
  }

  spec <- epi_eda_spec(spec)
  data_names <- names(data)
  expected_names <- spec$name

  expected <- data.frame(
    name = expected_names,
    expected_type = spec$type,
    observed_type = unname(vapply(
      expected_names,
      function(name) {
        if (name %in% data_names) {
          eda_observed_type(data[[name]])
        } else {
          NA_character_
        }
      },
      character(1)
    )),
    expected_present = TRUE,
    observed_present = expected_names %in% data_names,
    stringsAsFactors = FALSE
  )
  expected$status <- ifelse(expected$observed_present, "present", "missing")

  unexpected_names <- setdiff(data_names, expected_names)
  if (length(unexpected_names) == 0) {
    return(expected[, schema_columns()])
  }

  unexpected <- data.frame(
    name = unexpected_names,
    expected_type = NA_character_,
    observed_type = unname(vapply(
      unexpected_names,
      function(name) eda_observed_type(data[[name]]),
      character(1)
    )),
    expected_present = FALSE,
    observed_present = TRUE,
    status = "unexpected",
    stringsAsFactors = FALSE
  )

  rbind(expected[, schema_columns()], unexpected[, schema_columns()])
}

schema_columns <- function() {
  c(
    "name",
    "expected_type",
    "observed_type",
    "expected_present",
    "observed_present",
    "status"
  )
}

eda_observed_type <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return("datetime")
  }

  if (inherits(x, "Date")) {
    return("date")
  }

  if (is.factor(x)) {
    return("categorical")
  }

  if (is.numeric(x) || is.integer(x)) {
    return("numeric")
  }

  if (is.character(x)) {
    return("text")
  }

  if (is.logical(x)) {
    return("binary")
  }

  class(x)[1]
}
