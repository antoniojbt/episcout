#' Wrapper for `epi_stats_dates`
#'
#' Provided for backwards compatibility with older code that used
#' `calculate_date_stats()`.
#' @inheritParams epi_stats_dates
#' @return See `epi_stats_dates`.
#' @examples
#' calculate_date_stats(as.Date(c("2020-01-01", "2020-05-15")))
#' @export
calculate_date_stats <- function(date_vector) {
  epi_stats_dates(date_vector)
}
