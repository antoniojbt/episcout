#' @title Get all duplicated rows including the originals
#'
#' @description base R duplicated() does not return the originals (duplicated - 1).
#' See for example \href{https://stackoverflow.com/questions/16905425/find-duplicate-values-in-r}{find duplicates in R}.
#'
#' @param df A data.frame to extract duplicates from
#'
#' @param var Variable/column name as a string to use to detect duplicate values
#'
#' @param freq Frequency count, an integer. Defaults to 1 (so will return original
#' plus any repeating values)
#'
#' @note   Creates a table with frequencies, checks those which have more than 1
#' (or value passed to freq and considers these as variables with duplicates)
#' and extracts them.
#'
#' @return a data.frame with original and duplicated rows
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[base]{duplicated}}
#'
#' @examples
#' \dontrun{
#' dim(df)
#' epi_head_and_tail(df, rows = 2, cols = 2)
#' # Get all duplicates:
#' check_dups <- epi_clean_get_dups(df, "var_id", 1)
#' dim(check_dups)
#' check_dups
#' }
#'
#' @export
#'

epi_clean_get_dups <- function(df = NULL,
                               var = "",
                               freq = 1) {
  # Create a table with frequencies:
  n_occur <- data.frame(table(df[[var]]))
  # Check those which have mor than 1, these are duplicated:
  dups <- n_occur$Freq > freq
  # Extract them:
  df_dups <- df[df[[var]] %in% n_occur$Var1[dups], ]
  df_dups
}
