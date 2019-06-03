######################
# Helper functions for describing testing phenotype data
# Antonio Berlanga-Taylor
######################

######################
# Test set df:
# n <- 20
# df <- data.frame(
#   var_id = rep(1:(n / 2), each = 2),
#   var_to_rep = rep(c('Pre', 'Post'), n / 2),
#   x = rnorm(n),
#   y = rbinom(n, 1, 0.50),
#   z = rpois(n, 2)
#   )
# df
######################

#####################
# Create a table with factor variables together
# This is a loop that creates a descriptive table with counts
# of factor variables.
# vars_list is a string of variable names from df, a data frame
# vars_list can be any list but only character and factor columns are used
epi_stats_fct_table <- function(df = NULL,
                               vars_list = ''
                               ) {
  desc_stats_fct <- vector(mode = 'list', length = length(vars_list))
  names(desc_stats_fct) <- vars_list
  for (i in 1:length(vars_list)) {
    # i <- 1
    val <- vars_list[i]
    # Get a table for one variable, use functions above:
    desc_stats <- epi_stats_summary(df[, val],
                            class_type = 'chr_fct',
                            action = 'exclude'
                            )
    # Tidy:
    desc_stats <- epi_stats_tidy_sum(sum_df = desc_stats,
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

######################
# Create a unique ID column as not all rows have substudy_part_id
# Takes a data.frame and two columns names as input
# Returns the data.frame with a new ID column using 'col_1''sep''col_2'
# The ID column is return as the first column
# Test:
# df2 <- epi_clean_unique_id(input_data, 'substudy_part_id', 'BARCODE', sep = '_')
# epi_head_and_tail(df2)
# Add rownumber pastes the row number as part of the ID, useful to quickly create unique IDs when NAs are present
# with multiple ID columns.

epi_clean_unique_id <- function(df = NULL,
                                col_1 = '',
                                col_2 = '',
                                sep = '_',
                                add_rownames = FALSE
                                ) {
  if (!requireNamespace('dplyr', quietly = TRUE)) {
    stop("Package dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (add_rownames == TRUE) {
  df$unique_id <- paste(df[[col_1]], df[[col_2]], rownames(df), sep = sep)
  df <- df %>% dplyr::select(unique_id, dplyr::everything())
  # print(sprintf('Number of NAs in new column: %s', sum(is.na((df[ ,1])))))
  } else if (add_rownames == FALSE) {
    df$unique_id <- paste(df[[col_1]], df[[col_2]], sep = sep)
    df <- df %>% dplyr::select(unique_id, dplyr::everything())
    }
  names(df)[1] <- paste(col_1, col_2, sep = sep)
  return(df)
}
######################

#####################
# Add univariate t-tests
# Only returns the p-value
# Useful for adding to descriptive table of cohort for instance
# Pass additional parameters if needed with '...'
# @importFrom stats t.test
epi_stats_get_t_test <- function(x = NULL,
                                y = NULL,
                                ...
                                ) {
  i <- t.test(x = x, y = y, ...)
  return(i$p.value)
}
# Test:
# epi_stat_get_t_test(seq(1:100),
#            seq(50:150),
#            alternative = 'less'
#            )
# pval <- t.test(seq(1:100),
#        seq(50:150),
#        alternative = 'less'
#        )
# pval$p.value
#####################

#####################
# Extract values after limma differential analysis:
# TO DO: exclude for now, limma not in R 3.4 and 3.5? Causes travis to error
# epi_stat_get_top <- function(fit = NULL,
#                              coef = NULL,
#                              adjust = 'BH',
#                              number = Inf,
#                              ...
#                              ) {
#   if (!requireNamespace('limma', quietly = TRUE)) {
#     stop("Package limma needed for this function to work. Please install it.",
#          call. = FALSE)
#   }
#   top <- limma::topTable(fit = fit,
#                          adjust.method = adjust,
#                          coef = coef,
#                          number = number,
#                          ...
#                          )
#   return(top)
#   }
# Test:
# TO DO
#####################

#####################
# # Blurbs that may be useful:
# # Add 95% CIs of the mean in separate table
# # 95% CIs of the mean:
# get_sem <- function(i) round(sqrt(var(i, na.rm = TRUE) / length(na.omit(i))), 2)
# get_ci95 <- function(i) c(round(mean(i, na.rm = TRUE) - 2 * sem, 2),
#                           round(mean(i, na.rm = TRUE) + 2 * sem, 2))
# get_ci95up <- function(i) round((mean(i, na.rm = TRUE) + 2 * sem), 2)
# get_ci95low <- function(i) round((mean(i, na.rm = TRUE) - 2 * sem), 2)
#
# get_ci95s <- function(i) {
#   ci95up <- get_ci95up(i)
#   ci95low <- get_ci95low(i)
#   nice_print <- sprintf('%s, %s', ci95low, ci95up)
#   return(nice_print)
# }
# get_ci95s(all_data_reduced$vitd0)
# # Save table to file:
# print(xtable(main_table_2_sem), type = "html", file = 'BESTD_table_sem.html')

# # Save some text for legends or captions:
# title_table <- paste(
#   'Table 1: Basic characteristics, baseline values.',
#   sep = ''
# )
# cat(file = 'title_XXX_table.tsv', title_table,
#     # "\t", xxx_var, '\n',
#     append = FALSE)
#####################
