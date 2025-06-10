#' @title Format a dataframe with numerical or integer columns
#'
#' @description epi_stats_format() formats columns so that digits appear
#' even if they are x.00. Useful for saving a table with descriptive statistics.
#' A data frame with an id column is expected. Check if column
#' is numeric or integer, other types are not formatted. Pass a vector
#' of column number to skip if needed. Values are rounded to the option
#' passed to 'digits' digits = 2 by default
# format(x, nsmall = digits) is used to ensure xx.00 are printed
# This may not produce the right results for very large or small numbers
# Also note that format() will change the class type to factor or character
#'
#' @param df Data.frame with summary to clean up.
#' @param skip Columns to skip, pass as a string. Default is NULL.
#' @param digits Number of digits to print. Default is 2.
#' @param ... Any other parameter that can be passed to round().
#'
#' @return A data.frame with formatted and rounded values.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_summary}},
#' \code{\link{epi_stats_tidy}},
#' \code{\link{epi_clean_cond_numeric}},
#' \code{\link[base]{format}},
#' \code{\link[base]{round}}.
#'
#' @example vignettes/summary_funcs_examples.R
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
  return(df)
}
