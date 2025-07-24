#' Summarize Factor Variables
#'
#' Compute summary statistics for all factor columns in a data frame.
#' For each factor variable, this function returns:
#' \itemize{
#'   \item \code{Variable}: the name of the factor column.
#'   \item \code{n_missing}: the number of \code{NA} values.
#'   \item \code{complete_rate}: the proportion of non-\code{NA} values.
#'   \item \code{ordered}: \code{TRUE} if the factor is ordered, \code{FALSE} otherwise.
#'   \item \code{n_unique}: the number of distinct non-\code{NA} levels.
#'   \item \code{top_counts}: a character string listing the top up to three levels
#'         and their counts, formatted as \code{"level1 (count1), level2 (count2), ..."}.
#' }
#'
#' @param df A \code{data.frame} or tibble containing one or more factor columns.
#'
#' @return A tibble with one row per factor variable and the statistics described above.
#'
#' @details
#' This function uses \pkg{purrr} to iterate over each factor column,
#' \pkg{dplyr} to compute basic summaries, and base \code{table()} to rank levels
#' by frequency. Missing values are counted separately. The top three levels are
#' collapsed into a single string for compact reporting.
#'
#' @examples
#' library(dplyr)
#' df <- tibble::tibble(
#'   grp = factor(c("A", "B", "A", NA, "C", "A")),
#'   ord = ordered(c("low", "medium", "high", "low", NA, "medium"))
#' )
#' epi_stats_factors(df)
#'
#' @importFrom purrr imap_dfr
#' @importFrom dplyr select where n_distinct
#' @importFrom tibble tibble
#' @export
epi_stats_factors <- function(df) {
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package purrr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  factor_df <- df %>%
    dplyr::select(dplyr::where(is.factor))

  purrr::imap_dfr(factor_df, function(col, nm) {
    counts <- sort(table(col), decreasing = TRUE)
    tibble::tibble(
      Variable = nm,
      n_missing = sum(is.na(col)),
      complete_rate = mean(!is.na(col)),
      ordered = is.ordered(col),
      n_unique = dplyr::n_distinct(col, na.rm = TRUE),
      top_counts = paste(
        paste0(
          names(counts)[seq_len(min(3, length(counts)))],
          " (", counts[seq_len(min(3, length(counts)))], ")"
        ),
        collapse = ", "
      )
    )
  })
}
