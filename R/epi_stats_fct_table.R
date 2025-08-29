#' Tabulate counts of factor levels
#'
#' Creates a tidy table of counts for each level of factor or character
#' columns in a data frame. Missing values are reported with the level
#' `NA`.
#'
#' @param df A data frame.
#' @param vars_list Optional character vector of column names to include.
#'   When `NULL` (default) all factor and character columns are used.
#'
#' @return A tibble with columns `variable`, `level` and `count`.
#'
#' @examples
#' df <- data.frame(
#'   sex = factor(c("male", "female", "female")),
#'   group = c("A", "B", "A"),
#'   age = c(10, 20, 30)
#' )
#' epi_stats_fct_table(df)
#' epi_stats_fct_table(df, vars_list = "group")
#'
#' @seealso \code{\link{epi_stats_factors}},
#'   \code{\link{epi_stats_summary}}
#'
#' @export
epi_stats_fct_table <- function(df, vars_list = NULL) {
  df <- tibble::as_tibble(df)

  if (is.null(vars_list)) {
    fct_df <- dplyr::select(df, dplyr::where(~ is.factor(.) || is.character(.)))
  } else {
    fct_df <- dplyr::select(df, dplyr::all_of(vars_list))
  }

  fct_df <- purrr::modify(fct_df, as.factor)

  purrr::imap_dfr(fct_df, function(col, nm) {
    tmp <- tibble::tibble(level = addNA(col))
    dplyr::count(tmp, level, name = "count") %>%
      dplyr::mutate(
        variable = nm,
        level = as.character(level)
      ) %>%
      tidyr::replace_na(list(level = "NA")) %>%
      dplyr::select(variable, level, count)
  })
}
