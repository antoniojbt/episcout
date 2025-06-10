#' Drop Unused Levels from a Factor Vector
#'
#' Removes any factor levels that have zero occurrences, which can arise
#' after introducing `NA` values or subsetting. Handy for cleaning up
#' factor vectors before analysis or modeling.
#'
#' @param factor_var A factor vector in which you want to drop unused levels.
#'
#' @return A factor vector with all levels that have zero observations removed.
#'
#' @examples
#' \dontrun{}
#' # Single vector example
#' factor_var <- factor(c("a", "b", "a", "c", "b", "a", "b", "c"))
#' factor_var[c(2, 5)] <- NA  # simulate missing values that remove counts
#' cleaned_factor_var <- epi_clean_drop_zero_levels_vector(factor_var)
#' print(cleaned_factor_var)
#'
#' # Data frame example: apply to each factor column
#' df <- data.frame(
#'   col1 = factor(c("a", "b", "a", "c", "b")),
#'   col2 = factor(c("d", "d", "e", "f", "d")),
#'   col3 = factor(c("g", "h", "g", "g", "h"))
#' )
#' df$col1[c(2, 5)] <- NA
#' df$col2[4]      <- NA
#' df$col3[5]      <- NA
#' df[] <- lapply(df, function(column) {
#'   if (is.factor(column)) {
#'     epi_clean_drop_zero_levels_vector(column)
#'   } else {
#'     column
#'   }
#' })
#' print(df)

#' @export
epi_clean_drop_zero_levels_vector <- function(factor_var) {
  # Ensure the input is a factor
  if (!is.factor(factor_var)) {
    stop("The input variable is not a factor.")
  }

  # Get the levels that are present in the factor
  present_levels <- levels(factor_var)[table(factor_var) > 0]

  # Drop levels that are zero
  cleaned_factor <- factor(factor_var, levels = present_levels)

  return(cleaned_factor)
}
