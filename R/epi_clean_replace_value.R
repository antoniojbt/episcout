#' @title Replace values with string in a column
#'
#' @description Uses stringr to match string pattern provided and replace value
#' in column with string provided. Useful for partial matching for dates
#' and replacing with NA for example. Uses stringr::str_detect().
#'
#' @param df data frame object with string values in column to match and replace
#'
#' @param col_id Column ID passed as vector, can be index or column name
#'
#' @param pattern String pattern to look for (passed to str_detect pattern parameter)
#'
#' @param replace_str String that will replace value if pattern matches in the
#' column passed
#'
#' @return The column passed as a vector
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_add_rep_num}},
#' \code{\link{epi_clean_add_colname_suffix}},
#' \code{\link[stringr]{str_detect}}.
#'
#' @examples
#'
#' \dontrun{
#' df_factor <- df <- data.frame(
#' var_id = rep(1:(n / 2), each = 2),
#' var_to_rep = rep(c('Pre', 'Post'), n / 2),
#' x = rnorm(n),
#' y = rbinom(n, 1, 0.50),
#' z = rpois(n, 2)
#' )
#' df_factor$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
#' # Convert to character first:
#' df_factor$date_col <- as.character(df_factor$date_col)
#' lapply(df_factor, class)
#' patterns <- c('2018', '2022')
#' # match values starting with string
#' pattern <- pattern <- sprintf('^%s', patterns[1])
#' epi_clean_replace_value(df_factor, 'date_col', pattern, NA)
#' df_factor$date_col
#' # In a loop:
#' for (i in seq_along(df_factor)) {
#' 	for (p in patterns) {
#' 		# match values starting with string:
#' 		pattern <- sprintf('^%s', p)
#' 		df_factor[[i]] <- epi_clean_replace_value(df_factor, i, pattern, NA)
#' 	}
#' }
#' df_factor$date_col
#' }
#'
#' @export
#'

epi_clean_replace_value <- function(df = NULL,
																		col_id = '',
																		pattern = '',
																		replace_str = NA
																		) {
	if (!requireNamespace('stringr', quietly = TRUE)) {
		stop("Package stringr needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	df[[col_id]] <- ifelse(stringr::str_detect(df[[col_id]],
																						 pattern = pattern) == TRUE,
												 replace_str,
												 df[[col_id]]
	)
	# df[[col_id]][which(str_detect(df[[col_id]], pattern = patterns))] <- NA
	return(df[[col_id]])
}
