#' Summarize Character Variables
#'
#' Compute summary statistics for all character columns in a data frame.
#' For each character variable, this function returns the number of missing
#' values (\code{NA}), the proportion of non-missing values, the minimum
#' and maximum string lengths, the count of empty strings, the number of
#' unique values, and the count of values consisting only of whitespace.
#'
#' @param df A \code{data.frame} (or tibble) containing one or more character columns.
#'
#' @return A tibble with one row per character variable and the following columns:
#' \describe{
#'   \item{\code{Variable}}{Name of the character variable.}
#'   \item{\code{n_missing}}{Number of \code{NA} values.}
#'   \item{\code{complete_rate}}{Proportion of non-\code{NA} values.}
#'   \item{\code{min_length}}{Minimum length of the non-\code{NA} strings.}
#'   \item{\code{max_length}}{Maximum length of the non-\code{NA} strings.}
#'   \item{\code{empty}}{Count of empty strings (\code{""}).}
#'   \item{\code{n_unique}}{Number of unique non-\code{NA} values.}
#'   \item{\code{whitespace}}{Count of strings containing only whitespace.}
#' }
#'
#' @details
#' This function relies on \pkg{dplyr} for data manipulation,
#' \pkg{tidyr} for reshaping, and \pkg{stringr} for trimming whitespace.
#' It first pivots all character columns into long form, groups by variable,
#' and then computes each statistic in turn.
#'
#' @examples
#' library(dplyr)
#' df <- tibble(
#'   name = c("Alice", "Bob ", "", NA),
#'   city = c("NY", " LA", "  ", "Chicago")
#' )
#' epi_stats_chars(df)
#'
#' @importFrom dplyr select where group_by summarise if_else n_distinct ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_trim
#' @export
epi_stats_chars <- function(df) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package stringr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  char_cols <- dplyr::select(df, dplyr::where(~ is.character(.x) || all(is.na(.x))))
  if (ncol(char_cols) == 0) {
    return(dplyr::tibble())
  }
  char_cols %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(
      n_missing = sum(is.na(Value)),
      complete_rate = mean(!is.na(Value)),
      min_length = dplyr::if_else(n_missing < dplyr::n(), min(nchar(Value), na.rm = TRUE), NA_integer_),
      max_length = dplyr::if_else(n_missing < dplyr::n(), max(nchar(Value), na.rm = TRUE), NA_integer_),
      empty = sum(Value == "", na.rm = TRUE),
      n_unique = dplyr::n_distinct(Value, na.rm = TRUE),
      whitespace = sum(stringr::str_trim(Value) == "" & Value != "", na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}
