#####################
#' Compare two columns which may have duplicated information
#' TO DO: test and complete, not working
# epi_clean_compare_dup_cols <- function(df, col_1, col_2) {
# 	require(compare)
# 	df[[col_1]] <- enc2utf8(df[[col_1]])
# 	df[[col_2]] <- enc2utf8(df[[col_2]])
# 	comp <- compare::compare(df[[col_1]],
# 													 df[[col_2]],
# 													 allowAll = TRUE)
# 	comp_diff <- which(comp$detailedResult == FALSE)
# 	names_diff_rows <- names(which(comp$detailedResult == FALSE))
# 	comp_results <- list('differing_rows' = comp_diff,
# 											 'rows_names' = names_diff_rows
# 											 )
# 	return(comp_results)
# }
#' Test:
#####################

######################
#' Change col names to baseline, time_1, time_2, etc.:
epi_clean_add_colname_suffix <- function(df = NULL,
																				 id_col_num = 1,
																				 suffix = ''
																				 ) {
	col_names <- names(df)[-id_col_num]
	col_names <- paste(col_names, suffix, sep = '')
	# names(df)[start_at:ncol(df)] <- col_names
	return(col_names)
}
#' # Test
#' df2 <- df
#' names(df2)
#' # Add .0 as suffix to all column names starting from column 2 (skip id col):
#' id_col <- 1
#' new_colnames <- epi_clean_add_colname_suffix(df2, id_col, '.0')
#' new_colnames
#' # Rename them in my data frame:
#' names(df2)[-id_col] <- new_colnames
#' names(df2)
######################

######################
#' Create a single data frame with unique rows (individuals) and repeated
#' observations across columns
#' A column with the replicate/repeated observation/time-point number for each row
#' must be provided
epi_clean_spread_repeated <- function(df = NULL,
																			rep_num_col = 1,
																			id_col_num = 2
																			) {
	reps <- unique(df[[rep_num_col]])
	output <- vector(mode = 'list')#, length = length(reps))
	for (i in reps) {
		# Create sets with distinct observations, use rep_num_col to filter rows:
		rep_df <- df[which(df[[rep_num_col]] == i), ]
		# Sanity check, should return an empty tibble:
		# print(get_all_dups(rep_df, id_col, 1))
		# Change col names to baseline, time_1, time_2, etc.:
		suffix <- sprintf('.%s', i)
		new_colnames <- add_colname_suffix(rep_df, id_col_num, suffix)
		names(rep_df)[-id_col_num] <- new_colnames
		output[[i]] <- rep_df
	}
	return(output)
}
#' Test:
#' df3 <- df
#' df3
#' df3 <- epi_clean_spread_repeated(df3, 'var_to_rep', 1)
#' df3
######################

######################
#' Recursively merge data frames that are stored as lists within a list
#' Flatten with eg purrr::flatten() if there is more than one level.
#' Assumes:
#' - there are are 3 or more data frames to merge
#' - there are no duplicates in any of the data frames
#' The function performs a full outer join with base R
#' merge(df1, df2, by = id_col, all = TRUE)
#' TO DO: if only two dfs in list, print message or do one merge
epi_clean_merge_nested_dfs <- function(nested_list_dfs = NULL,
																			 id_col = ''
																			 ) {
	# Initialise merge:
	df1 <- nested_list_dfs[[1]]
	df2 <- nested_list_dfs[[2]]
	temp_df <- merge(df1, df2, by = id_col, all = TRUE, suffixes = c('df_1', 'df_2'))
	# Loop through nested data frames and merge each to previous merged df:
	for (i in 3:length(nested_list_dfs)) { # skip 1 and 2 as these are
		                                     # the initial merge
		suffix_2 <- sprintf('_%s', i)
		df2 <- nested_list_dfs[[i]] # new df to merge, starting at 3
		temp_df <- merge(temp_df, df2, by = id_col, all = TRUE, suffix = c('', suffix_2))
		}
  return(temp_df)
	}
#' # Test
#' library(purrr)
#' library(tibble)
#' nested_list_dfs <- purrr::flatten(list(df3, df3, df3))
#' id_col <- 'var_id'
#' epi_list_head(nested_list_dfs, 2, 3)
#' epi_list_tail(nested_list_dfs, 2, 3)
#' all_merged <- epi_clean_merge_nested_dfs(nested_list_dfs, id_col)
#' dim(all_merged)
#' as.tibble(all_merged)
#' names(all_merged)
#'
#' # This function would be equivalent to iteratively doing:
#' # Create sets with distinct observations:
#' library(dplyr)
#' # use df2 after custom function above add_rep_num():
#' df2
#' # See how many replicates there are:
#' df2 %>%
#' 	transmute(as.factor(rep_num)) %>%
#' 	summary()
#'
#' # Generate a data frame for each:
#' baseline <- df2 %>%
#' 	filter(rep_num == 1)
#' baseline
#' # Sanity check, should be empty:
#' epi_clean_get_dups(baseline, 'var_id', 1)
#' # Change col names to baseline, time_1, time_2, etc.:
#' new_colnames <- epi_clean_add_colname_suffix(baseline, 1, '.0')
#' names(baseline)[2:ncol(baseline)] <- new_colnames
#' names(baseline)
#
#' # First set of repeated observations:
#' time_1 <- df2 %>%
#' 	filter(rep_num == 2)
#' time_1
#' epi_clean_get_dups(time_1, 'var_id', 1)
#' # Change col names:
#' new_colnames <- epi_clean_add_colname_suffix(time_1, 1, '.1')
#' names(time_1)[2:ncol(time_1)] <- new_colnames
#' names(time_1)
#
#' # Nothing left:
#' df2 %>%
#' 	filter(rep_num == 3)
#
#' # Merge the data frames into one:
#' all_merged <- merge(baseline, time_1, by = 'var_id', all = TRUE)
#' dim(all_merged)
#' as.tibble(all_merged)
#' names(all_merged)
#' epi_head_and_tail(all_merged)
#' epi_head_and_tail(all_merged, last_cols = TRUE)
#' View(all_merged)
######################

######################
#' Transposes a data frame file and preserves row and column names:
#' Assumes there is an id column with unique IDs
epi_clean_transpose <- function(df = NULL,
																id_col_num = ''
																) {
	if (!requireNamespace('data.table', quietly = TRUE)) {
		stop("Package data.table needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	# Save original IDs from first column:
	rows <- as.character(unlist(df[[id_col_num]]))
	# Save original IDs from first row:
	cols <- as.character(colnames(df))
	# Transpose file without first column (containing IDs):
	df_t <- as.data.table(data.table::transpose(df[, -id_col_num]))
	# Insert original IDs as new colnames in transposed:
	colnames(df_t) <- rows
	# Insert original IDs as first column into transposed:
	df_t <- cbind(as.character(cols[-1]), df_t) # Exclude first label
	return(as.data.frame(df_t))
	}
#' Test:
#' df_trans <- df
#' df_trans$id_col <- rownames(df_trans)
#' df_trans
#' id_col <- 6
#' df_t <- epi_clean_transpose(df = df_trans, id_col)
#' class(df_t)
#' dim(df)
#' dim(df_t)
#' df_t
#' names(df_t)
######################
