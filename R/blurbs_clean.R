#####################
# Compare two columns which may have duplicated information
# TO DO: test and complete, not working
# epi_clean_compare_dup_cols <- function(df, col_1, col_2) {
#   require(compare)
#   df[[col_1]] <- enc2utf8(df[[col_1]])
#   df[[col_2]] <- enc2utf8(df[[col_2]])
#   comp <- compare::compare(df[[col_1]],
#                            df[[col_2]],
#                            allowAll = TRUE)
#   comp_diff <- which(comp$detailedResult == FALSE)
#   names_diff_rows <- names(which(comp$detailedResult == FALSE))
#   comp_results <- list('differing_rows' = comp_diff,
#                        'rows_names' = names_diff_rows
#                        )
#   return(comp_results)
# }
# Test:
#####################

