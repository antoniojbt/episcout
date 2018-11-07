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
