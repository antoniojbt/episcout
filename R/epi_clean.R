######################
# Write files to disk
# Similar to epi_write()
# # Pass additional parameters if needed with '...'
epi_write <- function(file_object, file_name, ...) {
	if (!requireNamespace('data.table', quietly = TRUE)) {
		stop("Package data.table needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	fwrite(file_object,
				 file_name,
				 row.names = FALSE,
				 quote = FALSE,
				 sep = '\t',
				 na = 'NA',
				 col.names = TRUE,
				 ...
				 # verbose = TRUE
				 # nThread = 4
				 )
}
# data.table::getDTthreads(verbose = TRUE)
######################

######################
# Generate a simple output name, splits at the last '.', drops the current suffix
# and adds the one provided
epi_output_name <- function(input_name, suffix = '.tsv') {
	# Split infile name at the last '.':
	output_name <- strsplit(input_name, "[.]\\s*(?=[^.]+$)", perl = TRUE)[[1]][1]
  output_name <- sprintf('%s%s', output_name, suffix)
  return(output_name)
  }
# Test:
# epi_output_name('something.yep')
######################

######################
# Print the first few rows and last few rows of a data frame
# Optionally print the last few columns
# TO DO: add error catch if number of rows is less than default, if so
# set default to nrow(df)
# TO DO: add error catch if number of cols is less than default, if so
# set default to ncol(df), eg
# if:
#  Error in `[.data.frame`(df, 1:rows, cols) : undefined columns selected
#  use cols = length(df), # or ncol(df)
# TO DO: add number of rows and cols when printing
epi_head_and_tail <- function(df, rows = 5, cols = 5, last_cols = FALSE) {
	df <- as.data.frame(df)
	if (last_cols == TRUE) {
		# Get the last columns:
		cols <- ((ncol(df) - cols):ncol(df))
	} else {
		# Otherwise get the first columns:
		cols <- 1:cols
		}
	heads <- df[1:rows, cols]
	last_rows <- ((nrow(df) - rows):nrow(df))
	tails <- df[last_rows, cols]
	print(rbind(heads, tails))
}
# Test:
# dim(df)
# epi_head_and_tail(df, rows = 2, cols = 2)
# epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE)
######################

######################
# Print first few rows of each element of a list
epi_print_list_head <- function(list, rows = 3, max = length(list)) {
	cat(sprintf('List has %s elements in total.\n', length(list)))
	cat(sprintf('First %s rows of first %s elements in list: \n', rows, max))
	for (item in 1:max) {
		print(head(list[[item]], rows))
	}
}
# Test:
# epi_print_list_head(as.list(df), 5, 4)
######################

######################
# Print last few rows of each element of a list
epi_print_list_tail <- function(list, rows = 3, max = length(list)) {
	cat(sprintf('List has %s elements in total.\n', length(list)))
	cat(sprintf('Last %s rows of first %s elements in list: \n', rows, max))
	for (item in 1:max) {
		print(tail(list[[item]], rows))
	}
}
# Test:
# epi_print_list_tail(as.list(df), 5, 4)
######################

######################
# base R duplicated() does not return the originals (duplicated - 1)
# https://stackoverflow.com/questions/16905425/find-duplicate-values-in-r
# Get all elements which are duplicated, including the originals:
epi_get_all_dups <- function(df, var, freq = 1) {
	# Create a table with frequencies:
	n_occur <- data.frame(table(df[[var]]))
	# Check those which have mor than 1, these are duplicated:
	dups <- n_occur$Freq > freq
	# Extract them:
	df_dups <- df[df[[var]] %in% n_occur$Var1[dups], ]
	return(df_dups)
}
# # Test:
# dim(df)
# epi_head_and_tail(df, rows = 2, cols = 2)
# # Get all duplicates:
# check_dups <- epi_get_all_dups(df, 'var_id', 1)
# dim(check_dups)
# check_dups
######################

######################
# Compare two rows which may have duplicated information
# va_id is passed as a string and grep'd without regex
# compare allows all transformations, sorting, etc. so can be loose
# compare can miss differences like this though
epi_compare_dup_rows <- function(df_dups, val_id, col_id, sub_index_1, sub_index_2) {
	if (!requireNamespace('compare', quietly = TRUE)) {
		stop("Package compare needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	val_id <- as.character(val_id)
	dup_indices <- which(grepl(val_id,
														 df_dups[[col_id]],
														 fixed = TRUE) # match as string, not regex
											 )
	# check_dups[dup_indices, 1:2]
	comp <- compare::compare(df_dups[dup_indices[sub_index_1], ],
													 df_dups[dup_indices[sub_index_2], ],
													 allowAll = TRUE
													 )
	comp_diff <- which(comp$detailedResult == FALSE)
	names_diff_cols <- names(which(comp$detailedResult == FALSE))
	comp_results <- list('differing_cols' = comp_diff,
											 'col_names' = names_diff_cols,
											 'duplicate_indices' = dup_indices)
	return(comp_results)
}
# # Test:
# # Check a few duplicated individuals:
# check_dups
# val_id <- '2' # TO DO: '1' matches '1' and '10' despited fixed = TRUE
# comp <- epi_compare_dup_rows(check_dups, val_id, 'var_id', 1, 2)
# comp
# View(t(check_dups[comp$duplicate_indices, ]))
# View(t(check_dups[comp$duplicate_indices, comp$differing_cols]))
######################

######################
# Compare two columns which may have duplicated information
# TO DO: test and complete, not working
# epi_compare_dup_cols <- function(df, col_1, col_2) {
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
# Test:
######################

######################
# Compare two strings and determine if 1 is substring of the other:
epi_compare_str <- function(df, row_n, fixed_chr_col, string_col) {
	if (!requireNamespace('stringi', quietly = TRUE)) {
		stop("Package stringi needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	fixed_chr <- as.character(df[row_n, fixed_chr_col])
	string <- as.character(df[row_n, string_col])
	# Match using fixed characters not regex:
	match_observed <- stri_detect(fixed = fixed_chr, str = string)
	return(match_observed)
}

# # test data:
# letts <- paste(letters, collapse = ' ')
# letts
# substr(letts, 1, 5)
# other_letts <- toupper(paste(letters, collapse = ' '))
# df_comp <- data.frame ('sub' = rep(x = substr(letts, 1, 5), 10),
# 									'str' = rep(x = substr(letts, 1, 5), 10),
# 									stringsAsFactors = FALSE)
# df2_comp <- data.frame ('sub' = rep(x = substr(letts, 1, 5), 10),
# 									'str' = rep(x = substr(other_letts, 6, 10), 10),
# 									stringsAsFactors = FALSE)
# df3 <- rbind(df_comp, df2_comp)
# df3
# col_1 <- 'sub'
# col_2 <- 'str'
# df_comp <- df3
# val_id <- 1
# df_comp[val_id, c(col_1, col_2)]
# epi_compare_str(df_comp, val_id, col_1, col_2)
######################

######################
# Check if column is integer or numeric to use for counts column wise with dplyr:
epi_cond_numeric <- function(col) {
	is.integer(col) == TRUE | is.numeric(col) == TRUE
	}
# Test:
# df %>%
# 	# select_if(is.integer) %>%
# 	# select_if(is.numeric) %>%
# 	select_if(~ epi_cond_numeric(.))
######################

######################
# Check if column is character or factor to use for counts column wise with dplyr:
epi_cond_chr_fct <- function(col) {
	is.character(col) == TRUE | is.factor(col) == TRUE
}
# Test:
# col_chr <- data.frame('chr1' = rep(c('A', 'B')),
# 											'chr2' = rep(c('C', 'D'))
# 											)
# df_cont_chr <- as.tibble(cbind(df, col_chr))
# df_cont_chr
# df_cont_chr %>%
# 	# select_if(is.character) %>%
# 	# select_if(is.factor) %>%
# 	select_if(~ epi_cond_chr_fct(.))
######################

######################
# Check if column is Date
# Use for counts column wise with dplyr for example
epi_cond_date <- function(col) {
	if (!requireNamespace('lubridate', quietly = TRUE)) {
		stop("Package lubridate needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	is.Date(col) == TRUE | is.POSIXt(col) == TRUE
	}
# Test:
# df_date <- df
# df_date$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
# df_date
# df_date %>%
# 	select_if(~ epi_cond_date(.))
######################

######################
# Add a column with count of duplicate (repeated screening, replicate count)
# Assumes df is sorted by id and eg if date is used, then earlier dates are first
# Returns a dataframe with one column which can be merged with existing df
# Useful when there are more than 2 replicates for each row.
epi_add_rep_num <- function(df, var_id, var_to_rep) {
	output <- data.frame(var_id = df[[var_id]],
											 rep_num = rep(1, nrow(df)) # create a vector of 1's
											 )
	names(output)[1] <- var_id
	for (i in 2:c(nrow(df))) {
		# Starts at i = 2:
		cond1 <- as.character(df[c(i), var_id]) == as.character(df[i - 1, var_id])
		cond2 <- as.character(df[c(i), var_to_rep]) != as.character(df[i - 1, var_to_rep])
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
# Test variables:
# var_id <- 'var_id'
# var_to_rep <- 'var_to_rep'
# df
# res <- epi_add_rep_num(df, 'var_id', 'var_to_rep')
# res
# # Sanity check:
# identical(as.character(res[[var_id]]),
# 					as.character(df[[var_id]])) # should be TRUE
# # Bind:
# df2 <- as.tibble(cbind(df, 'rep_num' = res$rep_num))
# # merge() will adds all rows from both data frames as there are duplicates
# # so use cbind after making sure order is exact
# epi_head_and_tail(df2, rows = 3)
# epi_head_and_tail(df2, rows = 3, last_cols = TRUE)
# df2
######################

######################
# Change col names to baseline, time_1, time_2, etc.:
epi_add_colname_suffix <- function(df, id_col_num, suffix) {
	col_names <- names(df)[-id_col_num]
	col_names <- paste(col_names, suffix, sep = '')
	# names(df)[start_at:ncol(df)] <- col_names
	return(col_names)
}
# # Test
# df2 <- df
# names(df2)
# # Add .0 as suffix to all column names starting from column 2 (skip id col):
# id_col <- 1
# new_colnames <- epi_add_colname_suffix(df2, id_col, '.0')
# new_colnames
# # Rename them in my data frame:
# names(df2)[-id_col] <- new_colnames
# names(df2)
######################

######################
# Create a single data frame with unique rows (individuals) and repeated
# observations across columns
# A column with the replicate/repeated observation/time-point number for each row
# must be provided
epi_spread_repeated <- function(df, rep_num_col, id_col_num) {
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
# Test:
# df3 <- df
# df3
# df3 <- epi_spread_repeated(df3, 'var_to_rep', 1)
# df3
######################

######################
# Recursively merge data frames that are stored as lists within a list
# Flatten with eg purrr::flatten() if there is more than one level.
# Assumes:
# - there are are 3 or more data frames to merge
# - there are no duplicates in any of the data frames
# The function performs a full outer join with base R
# merge(df1, df2, by = id_col, all = TRUE)
# TO DO: if only two dfs in list, print message or do one merge
epi_merge_nested_dfs <- function(nested_list_dfs, id_col) {
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
# # Test
# library(purrr)
# library(tibble)
# nested_list_dfs <- purrr::flatten(list(df3, df3, df3))
# id_col <- 'var_id'
# epi_print_list_head(nested_list_dfs, 2, 3)
# epi_print_list_tail(nested_list_dfs, 2, 3)
# all_merged <- epi_merge_nested_dfs(nested_list_dfs, id_col)
# dim(all_merged)
# as.tibble(all_merged)
# names(all_merged)
#
# # This function would be equivalent to iteratively doing:
# # Create sets with distinct observations:
# library(dplyr)
# # use df2 after custom function above add_rep_num():
# df2
# # See how many replicates there are:
# df2 %>%
# 	transmute(as.factor(rep_num)) %>%
# 	summary()
#
# # Generate a data frame for each:
# baseline <- df2 %>%
# 	filter(rep_num == 1)
# baseline
# # Sanity check, should be empty:
# epi_get_all_dups(baseline, 'var_id', 1)
# # Change col names to baseline, time_1, time_2, etc.:
# new_colnames <- epi_add_colname_suffix(baseline, 1, '.0')
# names(baseline)[2:ncol(baseline)] <- new_colnames
# names(baseline)
#
# # First set of repeated observations:
# time_1 <- df2 %>%
# 	filter(rep_num == 2)
# time_1
# epi_get_all_dups(time_1, 'var_id', 1)
# # Change col names:
# new_colnames <- epi_add_colname_suffix(time_1, 1, '.1')
# names(time_1)[2:ncol(time_1)] <- new_colnames
# names(time_1)
#
# # Nothing left:
# df2 %>%
# 	filter(rep_num == 3)
#
# # Merge the data frames into one:
# all_merged <- merge(baseline, time_1, by = 'var_id', all = TRUE)
# dim(all_merged)
# as.tibble(all_merged)
# names(all_merged)
# epi_head_and_tail(all_merged)
# epi_head_and_tail(all_merged, last_cols = TRUE)
# View(all_merged)
######################

######################
# Convert any chr class to factor if it has less than x unique results
# Avoid converting dates to factors though, only works if already specified as date
# it class is chr and number of unique values is less than the cut-off passed it'll
# get converted to factor
# Note that lubridate converted dates may return eg
# "POSIXct" "POSIXt"
# which with
# class(df[[i]]) != 'Date'
# will return a logical vector > 1 (ie
# [1] TRUE TRUE
# which will give a warning message:
# "... the condition has length > 1 and only the first element will be used"
epi_class_to_factor <- function(df, cutoff_unique = 10){
	for (i in seq_along(df)) {
		if (
			# if num. of unique values is less than cut-off
			length(unique(df[[i]])) < cutoff_unique &
			# and the class is not already a date:
			class(df[[i]]) != 'Date'
			) {
			# Convert to factor:
			df[[i]] <- as.factor(df[[i]])
		}
	}
	return(df)
}
# # Test:
# df_factor <- df
# df_factor$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)#nrow(df_factor))
# lapply(df_factor, class)
# lapply(df_factor, function(x) length(unique(x)))
# # Check conditions:
# i <- 'date_col'
# cutoff_unique <- 10
# # if num. of unique values is less than cut-off:
# length(unique(df_factor[[i]])) < cutoff_unique # should be TRUE
# # and the class is not already a date:
# class(df_factor[[i]]) != "Date" # should be FALSE
# epi_class_to_factor(df_factor) # should return date_col as Date
# df_factor <- epi_class_to_factor(df_factor, cutoff_unique = 10)
# lapply(df_factor, class)
# df_factor
######################

######################
# Transposes a data frame file and preserves row and column names:
# Assumes there is an id column with unique IDs
epi_transpose <- function(df, id_col_num) {
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
# Test:
# df_trans <- df
# df_trans$id_col <- rownames(df_trans)
# df_trans
# id_col <- 6
# df_t <- epi_transpose(df = df_trans, id_col)
# class(df_t)
# dim(df)
# dim(df_t)
# df_t
# names(df_t)
######################

#####################
# Check what types to expect for each column class:
# Note that columns with more than one class will be counted each time
# such as POSIX dates
epi_count_classes <- function(df) {
	if (!requireNamespace('dplyr', quietly = TRUE)) {
		stop("Package dplyr needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('purrr', quietly = TRUE)) {
		stop("Package purrr needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	df %>%
		map(., class) %>%
		flatten() %>% # this may double count if eg Date is POSIX as will have
		              # more than one class
		as.data.frame() %>%
		t() %>%
		table()
	}
# Test:
# df
# epi_count_classes(df)
#####################

#####################
# Replace values with string in a column
# Uses stringr to match pattern provided and replace value in column with
# string provided
# Useful for partial matching for eg dates and replacing with NA
epi_replace_value <- function(df, col_id, pattern, replace_str = NA) {
	if (!requireNamespace('stringr', quietly = TRUE)) {
		stop("Package stringr needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	df[[col_id]] <- ifelse(str_detect(df[[col_id]], pattern = pattern) == TRUE,
												 replace_str,
												 df[[col_id]]
	)
	# df[[col_id]][which(str_detect(df[[col_id]], pattern = patterns))] <- NA
	return(df[[col_id]])
}
# # Test:
# df_factor <- df
# df_factor$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
# # Convert to character first:
# df_factor$date_col <- as.character(df_factor$date_col)
# lapply(df_factor, class)
# patterns <- c('2018', '2022')
# pattern <- pattern <- sprintf('^%s', patterns[1]) # match values starting with string
# epi_replace_value(df_factor, 'date_col', pattern, NA)
# df_factor$date_col
# # In a loop:
# for (i in seq_along(df_factor)) {
# 	for (p in patterns) {
# 		# match values starting with string:
# 		pattern <- sprintf('^%s', p)
# 		df_factor[[i]] <- epi_replace_value(df_factor, i, pattern, NA)
# 	}
# }
# df_factor$date_col
#####################
