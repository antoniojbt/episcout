#' @title Transposes a dataframe preserving row and column names
#'
#' @description Transposes a dataframe preserving row and column names. Based on data.table transpose. Assumes there is an id column with unique IDs
#'
#' @param df A dataframe object to transpose. Coerced to data.table and returned as data.frame.
#'
#' @param id_col_num Index to identify the column with IDs
#'
#' @return A transposed data frame whose first column contains the original non-ID column names and whose remaining column names are the values from the column selected by `id_col_num`.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_add_colname_suffix}}, \code{\link{epi_clean_spread_repeated}}, \code{\link{epi_clean_merge_nested_dfs}}, \code{\link[data.table]{transpose}}, \code{\link[data.table]{as.data.table}}.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Generate some data:
#' n <- 20
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c("Pre", "Post"), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' df$id_col <- rownames(df)
#' df
#' id_col <- 6
#' df_t <- epi_clean_transpose(df = df, id_col)
#' class(df_t)
#' dim(df)
#' dim(df_t)
#' df_t
#' names(df_t)
#' }
#'
#' @export
#'

epi_clean_transpose <- function(df = NULL,
                                id_col_num = "") {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package data.table needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (!is.data.frame(df)) {
    stop("df must be a data frame", call. = FALSE)
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
  # Save original IDs from the selected column:
  rows <- as.character(unlist(df[[id_col_num]]))
  # Save original column names other than the selected ID column:
  cols <- as.character(colnames(df)[-id_col_num])
  # Transpose file without the selected ID column:
  df_t <- data.table::as.data.table(data.table::transpose(df[, -id_col_num]))
  # Insert original IDs as new colnames in transposed:
  colnames(df_t) <- rows
  # Insert original column names as first column into transposed:
  df_t <- cbind(V1 = cols, df_t)
  as.data.frame(df_t)
}
