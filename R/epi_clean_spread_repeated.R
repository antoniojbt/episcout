#' @title Spread repeated observations
#'
#' @description Create a single data frame with unique rows (individuals) and
#' repeated observations across columns. A column with the replicate/repeated
#' observation/time-point number for each row must be provided.
#'
#' @param df A dataframe in long format
#'
#' @param rep_col Column index or string to use to base spread on.
#'
#' @param id_col_num Column index with ID values. Default is 1.
#'
#' @return a nested list with as many sub-lists as unique values contained in the
#' column passed as rep_col. Column headers are renamed using
#' \code{\link{epi_clean_add_colname_suffix}} and will contain suffixes taken from
#' the unique values in rep_col.
#' The ID column passed as id_col_num is included in each sub-list.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_add_colname_suffix}}, \code{\link{epi_clean_merge_nested_dfs}},
#' \code{\link{epi_clean_transpose}}
#'
#' @examples
#' \dontrun{
#' n <- 20
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c("Pre", "Post"), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' df
#' df_spread <- epi_clean_spread_repeated(df, "var_to_rep", 1)
#' # Returns a nested list:
#' df_spread
#' }
#'
#' @export
#'

epi_clean_spread_repeated <- function(df = NULL,
                                      rep_col = "",
                                      id_col_num = 1) {
  reps <- unique(df[[rep_col]])
  output <- vector(mode = "list") # , length = length(reps))
  for (i in reps) {
    # Create sets with distinct observations, use rep_num_col to filter rows:
    rep_df <- df[which(df[[rep_col]] == i), ]
    # Sanity check, should return an empty tibble:
    # print(get_all_dups(rep_df, id_col, 1))
    # Change col names to baseline, time_1, time_2, etc.:
    suffix <- sprintf(".%s", i)
    new_colnames <- epi_clean_add_colname_suffix(rep_df, id_col_num, suffix)
    names(rep_df)[-id_col_num] <- new_colnames
    output[[i]] <- rep_df
  }
  output
}
