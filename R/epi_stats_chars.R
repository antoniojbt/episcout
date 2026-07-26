#' Summarise Character Variables
#'
#' Compute summary statistics for all character columns in a data frame. For each character variable, this function returns the number of missing values (`NA`), the proportion of non-missing values, the minimum and maximum string lengths, the count of empty strings, the number of unique values, and the count of values consisting only of whitespace.
#'
#' @param df A \code{data.frame} (or tibble) containing one or more character columns.
#'
#' @return A tibble with one row per character variable containing:
#' \describe{
#'   \item{\code{Variable}}{Name of the character variable.}
#'   \item{\code{n_missing}}{Number of \code{NA} values.}
#'   \item{\code{complete_rate}}{Proportion of non-\code{NA} values.}
#'   \item{\code{min_length}}{Minimum length of the non-\code{NA} strings.}
#'   \item{\code{max_length}}{Maximum length of the non-\code{NA} strings.}
#'   \item{\code{empty}}{Count of empty strings (\code{""}).}
#'   \item{\code{n_unique}}{Number of unique non-\code{NA} values.}
#'   \item{\code{whitespace}}{Count of strings consisting only of whitespace.}
#' }
#' @seealso \code{\link{epi_stats_factors}}, \code{\link{epi_stats_numeric}}, \code{\link{epi_stats_summary}}
#'
#' @details Each selected column is summarised independently through the same internal text contract used by specification-first EDA. Zero-row and all-missing columns retain one stable output row.
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
  char_cols <- dplyr::select(df, dplyr::where(~ is.character(.x) || all(is.na(.x))))
  if (ncol(char_cols) == 0) {
    return(dplyr::tibble())
  }
  purrr::imap_dfr(char_cols, function(values, name) {
    if (!is.character(values)) {
      values <- rep(NA_character_, length(values))
    }
    core <- summary_text_core(values)
    tibble::tibble(
      Variable = name,
      n_missing = core$n_missing,
      complete_rate = summary_safe_proportion(core$n_observed, core$n),
      min_length = core$min_length,
      max_length = core$max_length,
      empty = core$n_empty,
      n_unique = core$n_unique,
      whitespace = core$n_whitespace
    )
  })
}
