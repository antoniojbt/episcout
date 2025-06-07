#' @title Tidy up a data.frame with summary values
#'
#' @description epi_stats_tidy() cleans up the output from epi_stats_summary() and
#' epi_stats_numeric(). Values are rounded to digits (default is 2).
#' format(x, nsmall = digits) is used to ensure xx.00 are printed.
#' Ordering uses as.numeric(as.character(x)) as 'percent' or other numeric
#' column is assumed to be the preferred option. 'decreasing' is passed to order.
#'
#' @param sum_df Data.frame with summary to clean up.
#' @param order_by Column to order results by. Default is 'percent'.
#' @param perc_n Number of rows from original dataframe to calculate percentage. Must
#' be passed manually.
#' @param digits = 2,
#' @param decreasing Sort values by decreasing order. Default is TRUE.
#'
#' @return Returns a dataframe as a tibble with values ordered and spread.
#' Adds row sums and percentage.
#'
#' @note Note that format() will likely change the class type.
# Assumes that the first column is 'id'
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_summary}},
#' \code{\link{epi_stats_format}},
#' \code{\link{epi_stats_numeric}}.
#'
#' @example vignettes/summary_funcs_examples.R
#'
#' @export
#'

epi_stats_tidy <- function(sum_df  = NULL,
                           order_by = 'percent',
                           perc_n = NULL,
                           digits = 2,
                           decreasing = TRUE
                           ) {
  if (!requireNamespace('dplyr', quietly = TRUE)) {
    stop("Package dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('tidyr', quietly = TRUE)) {
    stop("Package tidyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('tibble', quietly = TRUE)) {
    stop("Package tibble needed for this function to work. Please install it.",
         call. = FALSE)
  }
	if (is.null(perc_n)) {
		stop("perc_n must be passed in order to calculate percentage. It will be the
			sample size (number of rows) from the original data frame.")
	}

  df <- tibble::as_tibble(sum_df)
  # standard eval version with spread_ to avoid NSE and R CMD check NOTEs:
  df <- df %>% tidyr::spread(., key = 'x', value = 'n')
  # Reorder columns as:
  df <- df %>%
    dplyr::select(#rlang::.data[['id']], # assumes there is a column called 'id'
                  dplyr::everything()
           )
  # Add row sum:
  df$row_sums <- rowSums(df[, -1], na.rm = TRUE) # assumes the first column is 'id'
  # Add percentage from total provided:
  df$percent <- (df$row_sums / perc_n) * 100
  # Re-order rows by column decreasing number:
  set_order <- order(as.numeric(as.character(df[[order_by]])),
                     decreasing = decreasing)
  df <- df[set_order, ]
  return(df)
  }
