#' @title Tidy a count summary
#'
#' @description Convert the historical count output from [epi_stats_summary()] to a wide table, add row totals and percentages, and order rows by a selected numeric column.
#'
#' @param sum_df Historical count summary returned by [epi_stats_summary()].
#' @param order_by Column used to order results. Default is `"percent"`.
#' @param perc_n Explicit denominator used to calculate percentages.
#' @param digits Retained historical argument; the current implementation does not format or round values. Use [epi_stats_format()] for display formatting.
#' @param decreasing Whether to sort `order_by` in decreasing order.
#'
#' @return A wide tibble with `row_sums` and `percent` columns, ordered by `order_by`.
#'
#' @note The first output column is treated as the row identifier when calculating row totals. `perc_n` must match the intended denominator; the function does not infer the analysis population.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_summary}}, \code{\link{epi_stats_format}}, \code{\link{epi_stats_numeric}}.
#'
#' @example inst/examples/summary-functions.R
#'
#' @export
#'

epi_stats_tidy <- function(sum_df = NULL,
                           order_by = "percent",
                           perc_n = NULL,
                           digits = 2,
                           decreasing = TRUE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package dplyr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop(
      "Package tidyr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop(
      "Package tibble needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (is.null(perc_n)) {
    stop("perc_n must be passed in order to calculate percentage. It will be the
			sample size (number of rows) from the original data frame.")
  }

  df <- tibble::as_tibble(sum_df)
  # standard eval version with spread_ to avoid NSE and R CMD check NOTEs:
  df <- df %>% tidyr::spread(., key = "x", value = "n")
  # Reorder columns as:
  df <- df %>%
    dplyr::select( # rlang::.data[['id']], # assumes there is a column called 'id'
      dplyr::everything()
    )
  # Add row sum:
  df$row_sums <- rowSums(df[, -1], na.rm = TRUE) # assumes the first column is 'id'
  # Add percentage from total provided:
  df$percent <- (df$row_sums / perc_n) * 100
  # Re-order rows by column decreasing number:
  set_order <- order(
    as.numeric(as.character(df[[order_by]])),
    decreasing = decreasing
  )
  df <- df[set_order, ]
  df
}
