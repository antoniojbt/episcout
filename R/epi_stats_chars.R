#' Summarise character variables
#'
#' Generate summary metrics for character columns in a data frame, returning
#' counts of missing values, complete rate, minimum and maximum string length,
#' count of empty strings, number of unique values and how many values contain
#' only white-space.
#'
#' @param df A data frame containing character variables.
#'
#' @return A tibble with one row per character column summarising its contents.
#' @seealso \code{\link{epi_stats_factors}}, \code{\link{epi_stats_numeric}},
#'   \code{\link{epi_stats_summary}}
#' @examples
#' df <- data.frame(
#'   c1 = c("a", "", "c", "  ", NA),
#'   c2 = c("aa", "bb", "bb", "bb", "")
#' )
#' epi_stats_chars(df)
#' @export
epi_stats_chars <- function(df) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop(
      "Package stringr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  df %>%
    dplyr::select(dplyr::where(is.character)) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(
      n_missing = sum(is.na(Value)),
      complete_rate = mean(!is.na(Value)),
      min_length = dplyr::if_else(
        n_missing < dplyr::n(),
        min(nchar(Value), na.rm = TRUE),
        NA_integer_
      ),
      max_length = dplyr::if_else(
        n_missing < dplyr::n(),
        max(nchar(Value), na.rm = TRUE),
        NA_integer_
      ),
      empty = sum(Value == "", na.rm = TRUE),
      n_unique = dplyr::n_distinct(Value, na.rm = TRUE),
      whitespace = sum(stringr::str_trim(Value) == "", na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}
