#' Profile missingness using an EDA specification
#'
#' Count standard missing values and configured missing codes for variables
#' listed in an EDA specification.
#'
#' @param data A data frame to profile.
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A data frame with variable names, row counts, missing counts and
#'   missing proportions.
#'
#' @export
epi_eda_profile_missing <- function(data, spec) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.", call. = FALSE)
  }

  spec <- epi_eda_spec(spec)
  n <- nrow(data)
  n_missing <- unname(vapply(
    spec$name,
    function(name) {
      if (!name %in% names(data)) {
        return(NA_integer_)
      }
      sum(eda_missing_mask(data[[name]], eda_missing_codes(spec, name)))
    },
    integer(1)
  ))

  data.frame(
    name = spec$name,
    n = n,
    n_missing = n_missing,
    p_missing = if (n > 0) n_missing / n else rep(NA_real_, length(n_missing)),
    stringsAsFactors = FALSE
  )
}

eda_missing_codes <- function(spec, name) {
  if (!("missing_codes" %in% names(spec))) {
    return(character())
  }

  row_index <- match(name, spec$name)
  if (is.na(row_index)) {
    return(character())
  }

  value <- spec$missing_codes[[row_index]]
  if (is.na(value) || !nzchar(trimws(as.character(value)))) {
    return(character())
  }

  codes <- trimws(strsplit(as.character(value), ";", fixed = TRUE)[[1]])
  codes[nzchar(codes)]
}

eda_missing_mask <- function(values, missing_codes = character()) {
  missing <- is.na(values)

  if (length(missing_codes) == 0) {
    return(missing)
  }

  values_chr <- as.character(values)
  missing | (!is.na(values_chr) & values_chr %in% missing_codes)
}
