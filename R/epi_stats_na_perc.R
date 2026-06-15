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
#' Invalid margins raise a clear error.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[stats]{complete.cases}}
#'
#' @examples
#' \dontrun{
#' library(mice)
#' dim(nhanes)
#' epi_head_and_tail(nhanes, cols = 4)
#' # Get summary of counts and percentages for missing values across columns:
#' na_cols <- epi_stats_na_perc(nhanes, margin = 2)
#' na_cols
#' # Get summary of counts and percentages for missing values across rows:
#' na_rows <- epi_stats_na_perc(nhanes, margin = 1)
#' epi_head_and_tail(na_rows, cols = 2)
#' summary(na_rows)
#' length(which(na_rows$na_counts == 0))
#' # which should be equal to:
#' length(which(complete.cases(nhanes)))
#' }
#'
#' @export
#'

epi_stats_na_perc <- function(df = NULL,
                              margin = 2 # 2 for columns, 1 for rows
) {
  if (!is.numeric(margin) || length(margin) != 1 || !margin %in% c(1, 2)) {
    stop("margin must be either 1 (rows) or 2 (columns)", call. = FALSE)
  }

  df <- as.data.frame(df)

  # For columns:
  if (margin == 2) {
    na_counts <- vapply(df, function(x) sum(is.na(x)), integer(1))
    na_perc <- if (nrow(df) > 0) {
      (na_counts / nrow(df)) * 100
    } else {
      rep(NA_real_, length(na_counts))
    }
    na_perc_all <- data.frame(
      na_counts = as.integer(na_counts),
      na_perc = as.numeric(na_perc),
      row.names = names(df),
      check.names = FALSE
    )
  }
  # For rows:
  else if (margin == 1) {
    na_counts <- rowSums(is.na(df))
    na_perc <- if (ncol(df) > 0) {
      (na_counts / ncol(df)) * 100
    } else {
      rep(NA_real_, length(na_counts))
    }
    na_perc_all <- data.frame(
      na_counts = as.integer(na_counts),
      na_perc = as.numeric(na_perc),
      row.names = rownames(df),
      check.names = FALSE
    )
  }
  na_perc_all
}
