# # Tests episcout

# Test set df:
n <- 20
df <- data.frame(
	var_id = rep(1:(n / 2), each = 2),
	var_to_rep = rep(c('Pre', 'Post'), n / 2),
	x = rnorm(n),
	y = rbinom(n, 1, 0.50),
	z = rpois(n, 2)
)
df

#
# epi_write(df,
# 					'tests/simple_df_test_set.tsv')

# #
# epi_read()
#
# #

#
# #
# dim(df)
# epi_head_and_tail(df, rows = 2, cols = 2)
# epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE)
#
# #
# epi_list_head(as.list(df), 5, 4)
#
# #
# epi_list_tail(as.list(df), 5, 4)

# # Test:
# dim(df)
# epi_head_and_tail(df, rows = 2, cols = 2)
# # Get all duplicates:
# check_dups <- epi_get_all_dups(df, 'var_id', 1)
# dim(check_dups)
# check_dups

