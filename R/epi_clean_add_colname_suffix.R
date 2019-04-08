#' @title Add a suffix to column names
#'
#' @description Add a suffix to column names except for an ID header.
#' Useful if changing labels to include eg baseline, time_1, time_2, etc.
#'
#' @param df A dataframe object with headers
#'
#' @param id_col_num Column number to exclude, usually the ID variable. Default is 1.
#'
#' @param suffix String to add as a suffix to each column except id_col_num
#'
#' @return String of column names with suffix added.
#' Change headers manually with these.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_add_rep_num}}, \code{\link{epi_clean_get_dups}},
#' \code{\link{epi_clean_spread_repeated}}
#'
#' @examples
#'
#' \dontrun{
#' n <- 20
#' df2 <- data.frame(
#' var_id = rep(1:(n / 2), each = 2),
#' var_to_rep = rep(c('Pre', 'Post'), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' # df2
#' names(df2)
#' # Add .0 as suffix to all column names starting from column 2 (skip id col):
#' id_col <- 1
#' new_colnames <- epi_clean_add_colname_suffix(df2, id_col, '.0')
#' new_colnames
#' # Rename them in my data frame:
#' names(df2)[-id_col] <- new_colnames
#' names(df2)
#' }
#'
#' @export
#'

epi_clean_add_colname_suffix <- function(df = NULL,
                                         id_col_num = 1,
                                         suffix = ''
                                         ) {
  col_names <- names(df)[-id_col_num]
  col_names <- paste(col_names, suffix, sep = '')
  # names(df)[start_at:ncol(df)] <- col_names
  return(col_names)
  }
