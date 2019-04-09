#' @title Get counts and percentage of NAs per row or column for a dataframe
#'
#' @description imp_desc_missingness() gets the total counts and percentage for missing values
#'
#' @param df A dataframe
#'
#' @param margin 2 for columns, 1 for rows. Default is columns
#'
#' @return A dataframe object with counts and percentage of NAs for each column or row.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{functioname}},
#' \code{\link[base]{is.na}}.
#'
#' @examples
#'
#' \dontrun{
#' data('employee')
#' head(employee)
#' na_cols <- epi_stats_na_perc(employee, margin = 2)
#' na_cols
#' na_rows <- epi_stats_na_perc(employee, margin = 1)
#' na_rows
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
