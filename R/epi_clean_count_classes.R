#' @title Get a count of column types from a data frame
#'
#' @description Count by column type in a data frame. Uses dplyr and purrr, useful
#' when handling large data frames (hundreds of columns and thousands of observations
#' for example).
#'
#' @param df data frame object with columns to count
#'
#' @return Returns a table with the counts of column types contained in the
#' data frame.
#'
#' @note Note that columns with more than one class will be counted each time
#' (such as POSIX dates), hence ncol() and epi_clean_count_classes() may not match
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_cond_numeric}},
#' \code{\link{epi_clean_class_to_factor}},
#' \code{\link{epi_clean_cond_date}}.
#'
#' @examples
#'
#' \dontrun{
#' set.seed(12345)
#' n <- 20
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#' var_to_rep = rep(c('Pre', 'Post'), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' df$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
#' epi_clean_count_classes(df)
#' }
#'
#' @export
#'
# @importFrom magrittr %>% # This errors. check() complains as NOTE though
#'

epi_clean_count_classes <- function(df = NULL) {
  if (!requireNamespace('dplyr', quietly = TRUE)) {
    stop("Package dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('purrr', quietly = TRUE)) {
    stop("Package purrr needed for this function to work. Please install it.",
         call. = FALSE)
  # if (!requireNamespace('magrittr', quietly = TRUE)) {
  #   stop("Package magrittr needed for this function to work. Please install it.",
  #        call. = FALSE)
  }
  df %>%
    purrr::map(., class) %>%
    purrr::flatten() %>% # this may double count if eg Date is POSIX as will have
    # more than one class
    as.data.frame() %>%
    t() %>%
    table()
}
