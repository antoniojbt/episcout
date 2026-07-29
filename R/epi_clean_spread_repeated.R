#' @title Spread repeated observations
#'
#' @description Create a single data frame with unique rows (individuals) and repeated observations across columns. A column with the replicate/repeated observation/time-point number for each row must be provided. Visit codes may be zero or nonconsecutive, but they must not be missing and each identifier/visit pair must be unique.
#'
#' @param df A dataframe in long format
#'
#' @param rep_col Column index or string to use to base spread on.
#'
#' @param id_col_num Column index with ID values. Default is 1.
#'
#' @return a nested list with as many sub-lists as unique values contained in the column passed as rep_col. Column headers are renamed using
#' \code{\link{epi_clean_add_colname_suffix}} and will contain suffixes taken from
#' the unique values in rep_col. The ID column passed as id_col_num is included in each sub-list.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_add_colname_suffix}}, \code{\link{epi_clean_merge_nested_dfs}}, \code{\link{epi_clean_transpose}}
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
  if (!is.data.frame(df)) {
    stop("df must be a data frame", call. = FALSE)
  }
  if (length(rep_col) != 1L || is.na(rep_col) ||
        !(is.character(rep_col) || is.numeric(rep_col)) ||
        (is.character(rep_col) && !rep_col %in% names(df)) ||
        (is.numeric(rep_col) &&
           (!is.finite(rep_col) || rep_col != as.integer(rep_col) ||
              rep_col < 1L || rep_col > ncol(df)))) {
    stop("rep_col must identify a single existing column", call. = FALSE)
  }
  if (!is.numeric(id_col_num) || length(id_col_num) != 1L ||
        is.na(id_col_num) || !is.finite(id_col_num) ||
        id_col_num != as.integer(id_col_num) ||
        id_col_num < 1L || id_col_num > ncol(df)) {
    stop(
      "id_col_num must be a single valid column index",
      call. = FALSE
    )
  }
  id_col_num <- as.integer(id_col_num)
  rep_values <- df[[rep_col]]
  if (any(is.na(rep_values))) {
    stop("rep_col must not contain missing visit codes", call. = FALSE)
  }
  id_values <- df[[id_col_num]]
  duplicate_pairs <- duplicated(data.frame(id_values, rep_values))
  if (any(duplicate_pairs)) {
    stop(
      "Each identifier and visit code pair must be unique",
      call. = FALSE
    )
  }
  reps <- unique(rep_values)
  output <- vector(mode = "list", length = length(reps))
  names(output) <- as.character(reps)
  for (i in seq_along(reps)) {
    rep_value <- reps[[i]]
    rep_label <- as.character(rep_value)
    # Create sets with distinct observations, use rep_num_col to filter rows:
    rep_df <- df[which(rep_values == rep_value), ]
    # Sanity check, should return an empty tibble:
    # print(get_all_dups(rep_df, id_col, 1))
    # Change col names to baseline, time_1, time_2, etc.:
    suffix <- sprintf(".%s", rep_label)
    new_colnames <- epi_clean_add_colname_suffix(rep_df, id_col_num, suffix)
    names(rep_df)[-id_col_num] <- new_colnames
    output[[i]] <- rep_df
  }
  output
}
