#' @title Compare two strings
#'
#' epi_clean_compare_str() uses stringi::stri_detect(). Developed when comparing
#' a database of observed and matched values for drug treatments from the BNF.
#' Can be used generically as a light wrapper for stri_detect().
#' stri_detect() if there is at least one match to a corresponding pattern.
#'
#' @description Compare two strings and determine if one is substring of the other
#'
#' @param df a data frame object containing rows with strings to compare
#'
#' @param row_n row number within the data frame with two columns to compare,
#' default is 1
#'
#' @param string_col column number with value which will be passed to
#' stri_detect as stri_detect(str = string, fixed = fixed_chr)
#'
#' @param fixed_chr_col column number value which will be passed to
#' stri_detect as stri_detect(str = string, fixed = fixed_chr)
#'
#' @param ... other options passed to stri_detect()
#'
#' @return returns the output of stri_detect(), a boolean TRUE/FALSE testing
#' whether the value of fixed_chr_col is a sub-string of that in string_col
#'
#' @note 	fixed_chr_col and string_col are extracted as characters and compared
#' as fixed characters, not with regex. The value in string_col will be the
#' character vector with strings to search in.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[stringi]{stri_detect}}.
#'
#' @examples
#'
#' \dontrun{
#' test data:
#' letts <- paste(letters, collapse = ' ')
#' other_letts <- toupper(paste(letters, collapse = ' '))
#' df_comp <- data.frame ('sub' = rep(x = substr(letts, 1, 5), 10),
#' 											 'str' = rep(x = substr(letts, 1, 5), 10),
#'											 stringsAsFactors = FALSE)
#' df2_comp <- data.frame ('sub' = rep(x = substr(letts, 1, 5), 10),
#' 												'str' = rep(x = substr(other_letts, 6, 10), 10),
#'												stringsAsFactors = FALSE)
#' # Create a new data frame and rename columns:
#' df3 <- rbind(df_comp, df2_comp)
#' col_1 <- 'sub'
#' col_2 <- 'str'
#' val_id <- 1
#' # df3[val_id, c(col_1, col_2)]
#' # Should evaluate to TRUE:
#' epi_clean_compare_str(df3, val_id, col_1, col_2)
#' }
#'

epi_clean_compare_str <- function(df = NULL,
																	row_n = 1,
																	string_col = '',
																	fixed_chr_col = '',
																	...
																		) {
	if (!requireNamespace('stringi', quietly = TRUE)) {
		stop("Package stringi needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	string <- as.character(df[row_n, string_col])
	fixed_chr <- as.character(df[row_n, fixed_chr_col])
	# Match using fixed characters not regex:
	match_observed <- stringi::stri_detect(str = string, fixed = fixed_chr, ...)
	return(match_observed)
}
