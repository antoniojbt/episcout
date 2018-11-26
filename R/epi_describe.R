######################
# Helper functions for describing testing phenotype data
# Antonio Berlanga-Taylor
######################

######################
# Test set df:
# n <- 20
# df <- data.frame(
# 	var_id = rep(1:(n / 2), each = 2),
#   var_to_rep = rep(c('Pre', 'Post'), n / 2),
# 	x = rnorm(n),
# 	y = rbinom(n, 1, 0.50),
# 	z = rpois(n, 2)
# 	)
# df
######################

######################
# Count how many outliers a vector has with a univariate approach
# Returns the number of observations that are greater than
# the cutoff.
# Outliers are detected with the Tukey method (above and below coef * IQR)
# coef = 0 returns no outliers, see ?boxplot.stats
# An alternative, not implemented, is to consider those eg > 5 * SD
# Not sure where these are needed for R's check():
# @importFrom grDevices boxplot.stats

# TO DO: add:
# SD * eg 5
# (IQR * 1.5) * 1.5 inner (or 3 for outer)
# See:
# https://stats.stackexchange.com/questions/350256/iterative-outlier-diagnostic?rq=1
# regression - Iterative outlier diagnostic - Cross Validated
# https://stats.stackexchange.com/questions/38001/detecting-outliers-using-standard-deviations?rq=1
# Detecting outliers using standard deviations - Cross Validated
# https://stats.stackexchange.com/questions/175999/determine-outliers-using-iqr-or-standard-deviation?rq=1
# Determine outliers using IQR or standard deviation? - Cross Validated
# https://stats.stackexchange.com/questions/1519/on-univariate-outlier-tests-or-dixon-q-versus-grubbs?rq=1
# hypothesis testing - On univariate outlier tests (or: Dixon Q versus Grubbs) - Cross Validated

epi_stat_count_outliers <- function(num_vec = NULL,
																		coef = 1.5,
																		...) {
	# get_SD <- sd(num_vec, na.rm = na.rm)
	# count_above <- length(get_SD * )
	outliers <- boxplot.stats(num_vec, coef = coef, ...)
	outliers <- length(outliers$out)
	return(outliers)
	}
# Test:
# epi_stat_count_outliers(num_vec = df$x)
######################

######################
# Get summary descriptive statistics for numeric/integer column.
# na.rm is TRUE by default
# Normality is tested with Shapiro-Wilk (small values indicate non-normality)
# Note that testing normality is contentious, likely uninformative and with Shapiro-Wilk
# can only be done for sample size between 3-5000.
# Kurtosis: lower values, broader shape and longer tails (platy ~<3), normal (meso ~3),
# slender/no tails (lepto ~<3)
# Skewness: negative/longer left tail, positive/longer right tail, >1 usually means non-normality
# Outliers are detected with the Tukey method (above and below 1.5 * IQR)
# ... is passed to skewness() and kurtosis()
# @importFrom stats median na.omit quantile sd shapiro.test var

# TO DO:
# add outlier_counts perc of total for column excluding NAs (?)
# add total n - NA (?) as column

epi_stats <- function(num_vec = NULL,
											na.rm = TRUE,
											coef = 1.5,
											...
											) {
	if (!requireNamespace('e1071', quietly = TRUE)) {
		stop("Package e1071 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	cond <- length(num_vec) > 3 & length(num_vec) < 5000
	if (cond) {
		normality <- shapiro.test(num_vec)
		normality <- normality$p.value
		} else {
		normality <- NA
		}
	desc_stats <- data.frame('min' = min(num_vec, na.rm = na.rm),
													 'quantile_25' = quantile(num_vec, probs = 0.25, names = FALSE, na.rm = na.rm),
													 'mean' = mean(num_vec, na.rm = na.rm),
													 'median' = median(num_vec, na.rm = na.rm),
													 'quantile_75' = quantile(num_vec, probs = 0.75, names = FALSE, na.rm = na.rm),
													 'max' = max(num_vec, na.rm = na.rm),
													 'SD' = sd(num_vec, na.rm = na.rm),
													 'variance' = var(num_vec, na.rm = na.rm),
													 'sem' = sd(num_vec, na.rm = na.rm) / sqrt(length(na.omit(num_vec))),
													 'skewness' = e1071::skewness(num_vec, na.rm = na.rm, ...),
													 'kurtosis' = e1071::kurtosis(num_vec, na.rm = na.rm, ...),
													 'Shapiro_Wilk_p_value' = normality,
													 'outlier_count' = count_outliers(num_vec, coef = coef),
													 'NA_count' = length(which(is.na(num_vec))),
													 'NA_percentage' = (length(which(is.na(num_vec))) / length(num_vec)) * 100
													 )
	return(desc_stats)
	}
# Test:
# num_vec <- df$x
# desc_stats <- epi_stats(num_vec)
# class(desc_stats)
# lapply(desc_stats, class)
# desc_stats
######################

######################
# Format a dataframe with numerical/integer columns so that digits appear even if they are x.00
# Useful for saving a table with descriptive statistics for example
# A data frame with an id column is expected
# Check if column is numeric or integer, other types are not formatted
# Pass a vector of column number to skip if needed
# Values are rounded to the option passed to 'digits'
# digits = 2 by default
# format(x, nsmall = digits) is used to ensure xx.00 are printed
# This may not produce the right results for very large or small numbers
# Also note that format() will change the class type to factor or character
epi_stat_format <- function(df = NULL,
														skip = NULL,
														digits = 2,
														...
														) {
	df <- as.data.frame(df)
	if (!is.null(skip)) {
		col_names <- names(df)[-skip]
		} else {
		col_names <- names(df)
		}
	for (i in col_names) {
		if (epi_clean_cond_numeric(df[[i]])) {
			df[[i]] <- format(round(df[[i]], digits), nsmall = digits, ...)
			}
		}
	return(df)
	}
# # Test:
# desc_stats
# # Add non-numeric columns:
# desc_stats <- cbind('id_col' = 1,
# 										desc_stats,
# 										'chr' = 'a_string')
# desc_stats
# dim(desc_stats)
# # Some tests:
# epi_stat_format(desc_stats[, 1]) # Formats
# epi_stat_format(desc_stats[, 'chr']) # Doesn't format
# epi_stat_format(desc_stats[, 'NA_count'], digits = 0) # Formats but no digits
# # Test skip:
# names(desc_stats)
# epi_stat_format(desc_stats, skip = c(1, 14))
######################

######################
# Provide summary descriptive statistics for columns belonging to either
# character and factor (class_type = 'chr_fct')
# or integer and numeric (class_type = 'int_num')
# while discarding values provided (codes_to_count).
# This is useful if data frame has contingency codes.
# Columns are ordered according to order in contingency codes option
# Rows are then ordered in decreasing order according to
# column provided.
epi_stat_summary <- function(df = NULL,
														 codes = NULL,
														 class_type = 'chr_fct', # 'int_num'
														 action = 'codes_only'   # 'exclude'
														 ) {
	if (!requireNamespace('dplyr', quietly = TRUE)) {
		stop("Package dplyr needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('purrr', quietly = TRUE)) {
		stop("Package purrr needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	df <- tibble::as.tibble(df)
	# Determine which group of columns to use:
	if (class_type == 'chr_fct') {
		cond <- expression(epi_clean_cond_chr_fct(.))
		} else if (class_type == 'int_num') {
		cond <- expression(epi_clean_cond_numeric(.))
		} else {
			stop('class_type parameter not specified correctly?')
		}
	# Determine what to do with the codes provided (count only codes or
	# exclude codes from counting):
	if (action == 'codes_only') {
		map_func <- expression(purrr::keep(., .p = (. %in% codes)))
		} else if (action == 'exclude') {
		map_func <- expression(purrr::discard(., .p = (. %in% codes)))
		} else {
			stop('action parameter not specified correctly?')
			}
	# Determine if to count or sum depending on class cond and action asked for
	# codes are expected to be summarised as factors (so count()) as they are
	# assumed to represent database codes for NA explanations
	# chr and factor columns would be counted regardless of codes only or codes excluded
	# so summary() should only be needed for num/int columns where codes are excluded
	if (class_type == 'int_num' & action == 'exclude') {
		sum_func <- expression(epi_stats(.))
		} else {
		# count is designed for data frames, not vectors, so pass as:
		sum_func <- expression(dplyr::count(data.frame(x = .x), x))
		}
	df <- df %>%
		dplyr::select_if(~ eval(cond)) %>%
		purrr::map(~ eval(map_func)) %>%
		purrr::map(~ eval(sum_func)) # Returns a list
	# Convert to dataframe with the same names for the var of interest:
	df <- as.data.frame(purrr::map_df(df,
																		tibble::rownames_to_column,
																		'var',
																		.id = 'id')
											)
	# Returns a list if sum_func is summary()
	df <- tibble::as.tibble(as.data.frame(df))
	# Drop 'var' col as not needed:
	df$var <- NULL
	# Make the rownames a column and order columns:
	# df$id <- rownames(df)
	# df <- df %>%
	# 	select(id,
	# 				 everything()
	# 	)
	return(df)
	}
# Tests below with next epi_stat_tidy_sum()
#####################

#####################
# Tidy output from epi_stat_summary functions to get a better summary
# Values are rounded to digits which defaults to 2
# format(x, nsmall = digits) is used to ensure xx.00 are printed
# Note that format() will likely change the class type
# Ordering uses as.numeric(as.character(x)) as 'percent' or other numeric
# column is assumed to be the preferred option
# 'decreasing' is passed to order
epi_stat_tidy_sum <- function(epi_stat_sum_df  = NULL,
															order_by = '',
															perc_n = NULL,
															digits = 2,
															decreasing = TRUE
															) {
	if (!requireNamespace('dplyr', quietly = TRUE)) {
		stop("Package dplyr needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('tidyr', quietly = TRUE)) {
		stop("Package tidyr needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('tibble', quietly = TRUE)) {
		stop("Package tibble needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	df <- tibble::as.tibble(epi_stat_sum_df)
	df <- df %>% tidyr::spread(., key = x, value = n)
	# Reorder columns as:
	df <- df %>%
		dplyr::select(id, # assumes there is a column called 'id'
									dplyr::everything()
					 )
	# Add row sum:
	df$row_sums <- rowSums(df[, -1], na.rm = TRUE) # assumes the first column is 'id'
	# Add percentage from total provided:
	df$percent <- (df$row_sums / perc_n) * 100
	# Re-order rows by column decreasing number:
	set_order <- order(as.numeric(as.character(df[[order_by]])),
										 decreasing = decreasing)
	df <- df[set_order, ]
	return(df)
	}
# # Test last two functions:
# col_chr <- data.frame('chr1' = rep(c('A', 'B')),
# 											'chr2' = rep(c('C', 'D'))
# 											)
# df_cont_chr <- as.tibble(cbind(df, col_chr))
# df_cont_chr
# codes <- c('Pre', 'A', 'C', '1', '3')
# # Test when codes are chr or factor and action is count only:
# epi_stat_sum1 <- epi_stat_summary(df_cont_chr,
# 												codes = codes,
# 												class_type = 'chr_fct',
# 												action = 'codes_only'
# 												)
# class(epi_stat_sum1)
# epi_stat_sum1
# # Add total for percentage calculation and order col:
# perc_n <- nrow(df_cont_chr)
# order_by <- 'percent'
# # Test epi_stat_tidy_sum:
# epi_stat_sum_tidy <- epi_stat_tidy_sum(epi_stat_sum_df = epi_stat_sum1,
# 														 order_by = order_by,
# 														 perc_n = perc_n
# 														 )
# epi_stat_sum_tidy
# epi_stat_format(epi_stat_sum_tidy[['row_sums']])
# # Test when codes are int or num and action is count only:
# epi_stat_sum2 <- epi_stat_summary(df_cont_chr,
# 												codes = codes,
# 												class_type = 'int_num',
# 												action = 'codes_only'
# 												)
# epi_stat_sum2
# # Test epi_stat_tidy_sum:
# epi_stat_sum_tidy <- epi_stat_tidy_sum(epi_stat_sum_df = epi_stat_sum2,
# 														 order_by = order_by,
# 														 perc_n = perc_n
# 														 )
# epi_stat_sum_tidy
# # Test when codes are chr or factor and action is exclude:
# epi_stat_sum3 <- epi_stat_summary(df_cont_chr,
# 												codes = codes,
# 												class_type = 'chr_fct',
# 												action = 'exclude'
# 												)
# epi_stat_sum3
# # Test epi_stat_tidy_sum:
# epi_stat_sum_tidy <- epi_stat_tidy_sum(epi_stat_sum_df = epi_stat_sum3,
# 														 order_by = order_by,
# 														 perc_n = perc_n
# 														 )
# epi_stat_sum_tidy
# # Test when codes are int or num and action is exclude:
# epi_stat_sum4 <- epi_stat_summary(df_cont_chr,
# 												codes = codes,
# 												class_type = 'int_num',
# 												action = 'exclude'
# 												)
# epi_stat_sum4
# as.data.frame(epi_stat_sum4)
# # Numeric data summary doesn't need to tidying so no epi_stat_sum_tidy() test
# # Test when the count is zero (returns empty rows):
# codes <- c('Per', 'X', '55')
# epi_stat_sum_zero <- epi_stat_summary(df_cont_chr,
# 												codes = codes,
# 														class_type = 'chr_fct',
# 														action = 'codes_only'
# 														)
# epi_stat_sum_zero
#####################


#####################
# Create a table with factor variables together
# This is a loop that creates a descriptive table with counts
# of factor variables.
# vars_list is a string of variable names from df, a data frame
# vars_list can be any list but only character and factor columns are used
epi_stat_fct_table <- function(df = NULL,
															 vars_list = ''
															 ) {
	desc_stats_fct <- vector(mode = 'list', length = length(vars_list))
	names(desc_stats_fct) <- vars_list
	for (i in 1:length(vars_list)) {
		# i <- 1
		val <- vars_list[i]
		# Get a table for one variable, use functions above:
		desc_stats <- epi_stat_summary(df[, val],
														class_type = 'chr_fct',
														action = 'exclude'
														)
	  # Tidy:
	  desc_stats <- epi_stat_tidy_sum(epi_stat_sum_df = desc_stats,
														 order_by = '<NA>',
														 perc_n = nrow(df[, vars_list])
														 )
	  # No need to format digits as should all be counts/integers
	  # Remove columns not informative:
	  desc_stats$row_sums <- NULL
	  desc_stats$percent <- NULL
	  # Rename NA column:
	  names(desc_stats)[grep(x = names(desc_stats), '<NA>', fixed = TRUE)] <- 'NA_count'
	  # Move headers to first row:
	  desc_stats <- rbind(colnames(desc_stats), desc_stats)
	  # Get rid of 'id' in row, leave blank:
	  desc_stats[1, 'id'] <- ' '
	  # Append as last rows:
    desc_stats_fct[[i]] <- desc_stats
    desc_stats_fct
    }
	return(desc_stats_fct)
	}
# Test:
# desc_stats_fct <- epi_stat_fct_table()
# epi_stat_fct_table(df, vars_list)
#####################

#####################
# Add univariate t-tests
# Only returns the p-value
# Useful for adding to descriptive table of cohort for instance
# Pass additional parameters if needed with '...'
# @importFrom stats t.test
epi_stat_get_t_test <- function(x = NULL,
																y = NULL,
																...
																) {
	i <- t.test(x = x, y = y, ...)
	return(i$p.value)
}
# Test:
# epi_stat_get_t_test(seq(1:100),
# 					 seq(50:150),
# 					 alternative = 'less'
# 					 )
# pval <- t.test(seq(1:100),
# 			 seq(50:150),
# 			 alternative = 'less'
# 			 )
# pval$p.value
#####################

#####################
# Extract values after limma differential analysis:
# TO DO: exclude for now, limma not in R 3.4 and 3.5? Causes travis to error
# epi_stat_get_top <- function(fit = NULL,
# 														 coef = NULL,
# 														 adjust = 'BH',
# 														 number = Inf,
# 														 ...
# 														 ) {
# 	if (!requireNamespace('limma', quietly = TRUE)) {
# 		stop("Package limma needed for this function to work. Please install it.",
# 				 call. = FALSE)
# 	}
# 	top <- limma::topTable(fit = fit,
# 												 adjust.method = adjust,
# 												 coef = coef,
# 												 number = number,
# 												 ...
# 												 )
# 	return(top)
# 	}
# Test:
# TO DO
#####################

#####################
# # Blurbs that may be useful:
# # Add 95% CIs of the mean in separate table
# # 95% CIs of the mean:
# get_sem <- function(i) round(sqrt(var(i, na.rm = TRUE) / length(na.omit(i))), 2)
# get_ci95 <- function(i) c(round(mean(i, na.rm = TRUE) - 2 * sem, 2),
# 													round(mean(i, na.rm = TRUE) + 2 * sem, 2))
# get_ci95up <- function(i) round((mean(i, na.rm = TRUE) + 2 * sem), 2)
# get_ci95low <- function(i) round((mean(i, na.rm = TRUE) - 2 * sem), 2)
#
# get_ci95s <- function(i) {
# 	ci95up <- get_ci95up(i)
# 	ci95low <- get_ci95low(i)
# 	nice_print <- sprintf('%s, %s', ci95low, ci95up)
# 	return(nice_print)
# }
# get_ci95s(all_data_reduced$vitd0)
# # Save table to file:
# print(xtable(main_table_2_sem), type = "html", file = 'BESTD_table_sem.html')

# # Save some text for legends or captions:
# title_table <- paste(
# 	'Table 1: Basic characteristics, baseline values.',
# 	sep = ''
# )
# cat(file = 'title_XXX_table.tsv', title_table,
# 		# "\t", xxx_var, '\n',
# 		append = FALSE)
#####################
