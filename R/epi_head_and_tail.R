#' @title Print the first few rows and last few rows of a data frame
#'
#' @description epi_head_and_tail() prints the first and last few rows of a data.frame
#'
#' @param df data.frame as input
#'
#' @param rows Number of rows to print, default is 5
#'
#' @param cols Number of columns to print, default is 5
#'
#' @param last_cols Print the last columns instead of the first few,
#' default is FALSE
#'
#' Row and column counts outside the available range are clamped to the
#' data dimensions.
#'
#' @return A data.frame containing the selected head and tail rows and
#'   columns. The value is returned invisibly after printing.
#'
#' @note Similar to data.table printing of a data.table object but works for
#' a tibble or data.frame, or any object that can be coerced to a data.frame
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
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
#' dim(df)
#' epi_head_and_tail(df)
#' epi_head_and_tail(df, rows = 2, cols = 2)
#' epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE)
#' }
#'
#' @importFrom utils head tail
#'
#' @export

epi_head_and_tail <- function(df = NULL,
                              rows = 5,
                              cols = 5,
                              last_cols = FALSE) {
  df <- as.data.frame(df)

  n_rows <- nrow(df)
  n_cols <- ncol(df)

  # Clamp the requested rows/cols to the available range
  rows <- min(max(1, rows), n_rows)
  cols <- min(max(1, cols), n_cols)

  print(sprintf("Total number of rows: %s", n_rows))
  print(sprintf("Total number of columns: %s", n_cols))

  if (last_cols == TRUE) {
    # Get the last columns
    cols <- ((n_cols - cols + 1):n_cols)
  } else {
    # Otherwise get the first columns
    cols <- 1:cols
  }

  heads <- df[1:rows, cols, drop = FALSE]
  last_rows <- ((n_rows - (rows - 1)):n_rows)
  tails <- df[last_rows, cols, drop = FALSE]

  out <- rbind(heads, tails)
  print(out)
  invisible(out)
}
