#' Tabulate counts of factor levels
#'
#' Creates a tidy table of counts for each level of factor or character columns in a data frame. Declared factor levels are retained with zero counts. Missing values use an actual `NA` level so they remain distinct from the literal string `"NA"`.
#'
#' @param df A data frame.
#' @param vars_list Optional character vector of column names to include. When `NULL` (default) all factor and character columns are used.
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
#' @seealso \code{\link{epi_stats_factors}}, \code{\link{epi_stats_summary}}
#'
#' @export
epi_stats_fct_table <- function(df, vars_list = NULL) {
  df <- tibble::as_tibble(df)

  if (is.null(vars_list)) {
    fct_df <- dplyr::select(df, dplyr::where(~ is.factor(.) || is.character(.)))
  } else {
    fct_df <- dplyr::select(df, dplyr::all_of(vars_list))
  }

  purrr::imap_dfr(fct_df, function(col, nm) {
    declared_levels <- if (is.factor(col)) levels(col) else NULL
    core <- summary_categorical_core(col, declared_levels = declared_levels)
    observed_missing <- sum(is.na(col))
    result <- tibble::tibble(
      variable = rep(nm, nrow(core)),
      level = core$level,
      count = core$n
    )
    if (observed_missing > 0L) {
      result <- dplyr::bind_rows(
        result,
        tibble::tibble(variable = nm, level = NA_character_, count = as.integer(observed_missing))
      )
    }
    result
  })
}
