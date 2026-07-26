

#' Summarise factor variables
#'
#' Produce summary metrics for all factor columns in a data frame. The output includes counts of missing values, proportion of complete cases, whether the factor is ordered, the number of unique levels and the three most frequent levels with their counts.
#'
#' @param df A data frame containing factor variables.
#'
#' @return A tibble in wide format with one row per factor column.
#' @seealso \code{\link{epi_stats_chars}}, \code{\link{epi_stats_numeric}}, \code{\link{epi_stats_summary}}
#'
#' @examples
#' df <- data.frame(
#'   f1 = factor(c("a", "b", "a", NA, "a"), ordered = TRUE),
#'   f2 = factor(c("x", "x", "y", "y", "y"))
#' )
#' epi_stats_factors(df)
#'
#' @export
epi_stats_factors <- function(df) {
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop(
      "Package purrr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  factor_df <- df %>% dplyr::select(dplyr::where(is.factor))

  purrr::imap_dfr(factor_df, function(col, nm) {
    core <- summary_categorical_core(col)
    counts <- core[core$n > 0L, c("level", "n"), drop = FALSE]
    counts <- counts[order(-counts$n, seq_len(nrow(counts))), , drop = FALSE]
    top <- seq_len(min(3L, nrow(counts)))
    tibble::tibble(
      Variable = nm,
      n_missing = sum(is.na(col)),
      complete_rate = summary_safe_proportion(sum(!is.na(col)), length(col)),
      ordered = is.ordered(col),
      n_unique = dplyr::n_distinct(col, na.rm = TRUE),
      top_counts = if (length(top) > 0L) paste0(counts$level[top], " (", counts$n[top], ")", collapse = ", ") else ""
    )
  })
}
