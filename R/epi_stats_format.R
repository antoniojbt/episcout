#' @title Format numeric columns for display
#'
#' @description Round and format numeric or integer columns with a fixed minimum number of decimal places. Formatting converts changed columns to character, so use the returned data frame for display rather than further calculation.
#'
#' @param df Data frame containing columns to format.
#' @param skip Optional numeric column positions to leave unchanged.
#' @param digits Number of decimal places to display.
#' @param ... Additional arguments passed to [format()].
#'
#' @return A data frame in which formatted numeric columns are character vectors.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_summary}}, \code{\link{epi_stats_tidy}}, \code{\link{epi_clean_cond_numeric}}, \code{\link[base]{format}}, \code{\link[base]{round}}.
#'
#' @example inst/examples/summary-functions.R
#'
#' @export
#'

epi_stats_format <- function(df = NULL,
                             skip = NULL,
                             digits = 2,
                             ...) {
  df <- as.data.frame(df)
  if (!is.null(skip)) {
    col_names <- names(df)[-skip]
  } else {
    col_names <- names(df)
  }
  for (i in col_names) {
    if (epi_clean_cond_numeric(df[[i]])) {
      df[[i]] <- format(round(df[[i]], digits), nsmall = digits, ...)
    }
  }
  df
}
