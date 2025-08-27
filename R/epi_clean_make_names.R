#' @title Check and correct strings which are not syntactically valid or reserved
#'
#' @description epi_clean_make_names() Pass a string vector such as column
#' headers, check they are valid names and if not correct.
#' Saves some headaches downstream when trying to plot non-syntactically
#' valid column headers for example.
#'
#' @param string String with names to test
#' @param str_replacement string to use instead of the '.' from make.names()
#' Default is '_'.
#'
#' @param unique make.names() unique parameter. Default is TRUE.
#'
#' @return A list of corrected and valid strings.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[base]{make.names}},
#' \href{https://stackoverflow.com/questions/8396577/check-if-character-value-is-a-valid-r-object-name/8396658#8396658}{stackoverflow test if valid},
#' \href{https://www.r-bloggers.com/testing-for-valid-variable-names/}{r-bloggers test valid names}.
#'
#'
#' @examples
#' \dontrun{
#' library(stringr)
#' string <- c(
#'   "mean", ".j_j", "...", "if",
#'   "while", "TRUE", "NULL", "_jj",
#'   "  j", ".2way"
#' )
#' valid_names <- epi_clean_make_names(string)
#' valid_names
#' epi_clean_make_names(valid_names) # There shouldn't be anything to correct
#' }
#'
#' @export
#'

epi_clean_make_names <- function(string = "",
                                 str_replacement = "_",
                                 unique = TRUE) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop(
      "Package stringr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  # Check any duplicates in headers:
  if (length(string) != length(unique(string))) {
    print("There are duplicate strings.")
    print("These will be changed to make unique names.")
  }

  # Check how many are not valid:
  check_valids <- make.names(names = string) == string
  print("Number of invalid names: ")
  print(length(string) - length(which(check_valids)))

  # Check which are not valid:
  invalid <- which(check_valids == FALSE)
  invalid <- string[invalid]
  print("Invalid names (reserved or syntactically invalid): ")
  print(invalid)

  # Make them valid:
  make_valid <- make.names(names = string, unique = unique) # here changes '-' for '.'

  # Replace dots from make.names():
  str_replacement <- str_replacement
  for (i in seq_along(make_valid)) {
    make_valid[i] <- stringr::str_replace_all(
      string = make_valid[i],
      pattern = "[.]",
      replacement = str_replacement
    )
  }

  # Make sure the substitution didn't introduce problems:
  cond <- all(make.names(names = make_valid) == make_valid)
  if (cond == FALSE) {
    make_valid <- make.names(names = make_valid)
  }

  # Stop if valid names couldn't be generated:
  cond <- all(make.names(names = make_valid) == make_valid)
  if (cond == FALSE) {
    stop("Could not generate valid names. Check your str_replacement value
         or generate them manually with make.names() for example.")
  }

  make_valid
}
