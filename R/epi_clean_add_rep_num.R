#' @title Add a replicate number to rows with repeated IDs
#'
#' epi_clean_add_rep_num()
#'
#' @description Add a column with count of duplicate/replicate (ie for
#' repeated screening, replicate counts, etc.). Assumes the dataframe passed
#' is sorted logically with repeating IDs next to each other and
#' if date is used as a second ordering criteria for example,
#' then earlier dates are first. Useful for data with repeated
#' measurements but without a column/variable clearly identifying them as such.
#'
#' @param df a dataframe object as input
#'
#' @param var_id Column to use as ID, read as dataframe vector, can be index
#' or string. This will be used to test if row is duplicate, if it is it will add
#' a replicate number.
#'
#' @param var_to_rep Column variable that can distinguish replicates (eg date,
#' 'baseline' vs 'treated', etc.)
#'
#' @return Returns a dataframe with one column which can be merged with
#' existing dataframe.
#'
#' @note Facilitates spreading a dataframe and extracting baseline vs repeated
#' measurement rows for example
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @examples
#'
#' \dontrun{
#' n <- 20
#' df <- data.frame(
#' var_id = rep(1:(n / 2), each = 2),
#' var_to_rep = rep(c('Pre', 'Post'), n / 2),
#' x = rnorm(n),
#' 	y = rbinom(n, 1, 0.50),
#' 	z = rpois(n, 2)
#' )
#' var_id <- 'var_id'
#' var_to_rep <- 'var_to_rep'
#' reps <- epi_clean_add_rep_num(df, 'var_id', 'var_to_rep')
#' reps
#' # Sanity check:
#' identical(as.character(reps[[var_id]]),
#' 					as.character(df[[var_id]])) # should be TRUE
#' # Bind:
#' df2 <- as.tibble(cbind(df, 'rep_num' = reps$rep_num))
#' # merge() adds all rows from both data frames as there are duplicates
#' # so use cbind after making sure order is exact
#' epi_head_and_tail(df2, rows = 3)
#' epi_head_and_tail(df2, rows = 3, last_cols = TRUE)
#' df2
#' }
#'

epi_clean_add_rep_num <- function(df = NULL,
																	var_id = NULL,
																	var_to_rep = ''
																	) {
	output <- data.frame(var_id = df[[var_id]],
											 rep_num = rep(1, nrow(df)) # create a vector of 1's
	)
	names(output)[1] <- var_id
	for (i in 2:c(nrow(df))) {
		# Starts at i = 2:
		cond1 <- as.character(df[c(i), var_id]) == as.character(df[i - 1, var_id])
		cond2 <- as.character(df[c(i), var_to_rep]) != as.character(df[i - 1,
																																	 var_to_rep])
		result <- if (cond1 & cond2) {
			# If IDs are the same but the var_to_rep is different,
			# add 1:
			# df[i, rep_num_col] <- df[i - 1, rep_num_col] + 1
			result <- output[i - 1, 'rep_num'] + 1
		} else {
			# Otherwise stay the same:
			# df[i, rep_num_col] <- df[i, rep_num_col]
			result <- output[i, 'rep_num']
		}
		output[i, 'rep_num'] <- result
	}
	return(output)
	}
