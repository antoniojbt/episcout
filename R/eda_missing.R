#' Profile missingness using an EDA specification
#'
#' Count standard missing values for variables listed in an EDA specification.
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
      sum(is.na(data[[name]]))
    },
    integer(1)
  ))

  data.frame(
    name = spec$name,
    n = n,
    n_missing = n_missing,
    p_missing = n_missing / n,
    stringsAsFactors = FALSE
  )
}
