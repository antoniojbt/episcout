#' Convert integer columns to factors
#'
#' Converts columns of class integer to factors in a `data.table`, excluding
#' those of class `IDate`.
#'
#' @param dt A `data.table` containing integer columns to convert.
#'
#' @return A `data.table` with integer columns converted to factors.
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(a = 1:3, b = as.IDate("2020-01-01") + 0:2)
#' epi_clean_int_to_factor(dt)
epi_clean_int_to_factor <- function(dt) {
  for (col in names(dt)) {
    if (is.integer(dt[[col]]) && !inherits(dt[[col]], "IDate")) {
      dt[[col]] <- as.factor(dt[[col]])
    }
  }
  dt
}
