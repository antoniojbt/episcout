#' @title Get counts and percentage of NAs
#'
#' @description epi_stats_na_perc() gets the total counts and percentage for missing values
#' across rows or columns
#'
#' @param df A dataframe with missing values
#'
#' @param margin 2 for columns, 1 for rows. Default is columns
#'
#' @return A dataframe object with counts and percentage of NAs for each column or row.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[stats]{complete.cases}}
#'
#' @examples
#'
#' \dontrun{
#'library(mice)
#'dim(nhanes)
#'epi_head_and_tail(nhanes, cols = 4)
#' # Get summary of counts and percentages for missing values across columns:
#'na_cols <- epi_stats_na_perc(nhanes, margin = 2)
#'na_cols
#' # Get summary of counts and percentages for missing values across rows:
#'na_rows <- epi_stats_na_perc(nhanes, margin = 1)
#'epi_head_and_tail(na_rows, cols = 2)
#'summary(na_rows)
#'length(which(na_rows$na_counts == 0))
#' # which should be equal to:
#'length(which(complete.cases(nhanes)))
#' }
#'
#' @export
#'

epi_stats_na_perc <- function(df = NULL,
                              margin = 2 # 2 for columns, 1 for rows
                              ) {
  # For columns:
  if (margin == 2) {
    na_perc_all <- as.list(apply(X = df, MARGIN = margin, function(x) sum(is.na(x))))
    na_perc_all <- as.data.frame(na_perc_all)
    na_perc_all <- as.data.frame(t(na_perc_all))
    names(na_perc_all)[1] <- 'na_counts'
    na_perc_all$na_perc <- (na_perc_all$na_counts / dim(df)[1]) * 100
  }
  # For rows:
  else if (margin == 1) {
    na_perc_all <- apply(X = df, MARGIN = margin, function(x) sum(is.na(x)))
    na_perc_all <- as.data.frame(na_perc_all)
    names(na_perc_all)[1] <- 'na_counts'
    na_perc_all$na_perc <- (na_perc_all$na_counts / dim(df)[2]) * 100
    }
  return(na_perc_all)
}
