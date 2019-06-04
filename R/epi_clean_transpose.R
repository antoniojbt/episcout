#' @title Transposes a dataframe preserving row and column names
#'
#' @description Transposes a dataframe preserving row and column names. Based on
#' data.table transpose. Assumes there is an id column with unique IDs
#'
#' @param df A dataframe object to transpose. Coerced to data.table and returned
#' as data.frame.
#'
#' @param id_col_num Index to identify the column with IDs
#'
#' @return A tranposed dataframe with the first column containing the tranposed
#' values of the column passed as id_col_num.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_add_colname_suffix}},
#' \code{\link{epi_clean_spread_repeated}},
#' \code{\link{epi_clean_merge_nested_dfs}},
#' \code{\link[data.table]{transpose}},
#' \code{\link[data.table]{as.data.table}}.
#'
#' @examples
#'
#' \dontrun{
#' library(data.table)
#' # Generate some data:
#' n <- 20
#' df <- data.frame(
#' var_id = rep(1:(n / 2), each = 2),
#' var_to_rep = rep(c('Pre', 'Post'), n / 2),
#' x = rnorm(n),
#' y = rbinom(n, 1, 0.50),
#' z = rpois(n, 2)
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
                                id_col_num = ''
                                ) {
  if (!requireNamespace('data.table', quietly = TRUE)) {
    stop("Package data.table needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Save original IDs from first column:
  rows <- as.character(unlist(df[[id_col_num]]))
  # Save original IDs from first row:
  cols <- as.character(colnames(df))
  # Transpose file without first column (containing IDs):
  df_t <- data.table::as.data.table(data.table::transpose(df[, -id_col_num]))
  # Insert original IDs as new colnames in transposed:
  colnames(df_t) <- rows
  # Insert original IDs as first column into transposed:
  df_t <- cbind(as.character(cols[-1]), df_t) # Exclude first label
  return(as.data.frame(df_t))
  }
