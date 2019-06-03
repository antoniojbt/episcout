#' @title Tidy up a data.frame with summary values
#'
#' @description epi_stats_tidy() cleans up the ouput from epi_stats_summary and
#' epi_stats_numeric functions. Values are rounded to digits (default is 2).
#' format(x, nsmall = digits) is used to ensure xx.00 are printed.
#' Ordering uses as.numeric(as.character(x)) as 'percent' or other numeric
#' column is assumed to be the preferred option. 'decreasing' is passed to order.
#'
#' @param sum_df  Data.frame with summary to clean up
#' @param order_by Column to order results by. Default is empty  = '',
#' @param perc_n = NULL,
#' @param digits = 2,
#' @param decreasing = TRUE
#' @param
#'
#' @return
#'
#' @note Note that format() will likely change the class type.
#' Assumes that the first column is 'id'
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{functioname}},
#' \code{\link[packagename]{functioname}}.
#'
#' @examples
#'
#' \dontrun{
#'
#'
#'
#' }
#'
#' @export
#'

epi_stats_tidy <- function(sum_df  = NULL,
                           order_by = '',
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
  df <- tibble::as.tibble(sum_df)
  df <- df %>% tidyr::spread(., key = x, value = n)
  # Reorder columns as:
  df <- df %>%
    dplyr::select(id, # assumes there is a column called 'id'
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
