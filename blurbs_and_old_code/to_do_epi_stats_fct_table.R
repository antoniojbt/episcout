#' @title Create a table from factor variables
#'
#' @description epi_stats_fct_table() creates a table using only factor variables
#' present in a data.frame. Runs a loop that creates a descriptive table with counts
#' of factor variables.
#'
#' @param df Data frame with factor variables to use as input.
#' @param vars_list A string of variable names to create a table from. Takes
#' character and factor columns.

#' @return
#'
#' @note
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{functioname}},
#' \code{\link[packagename]{functioname}}.
#'
#' @examples
#'
#' \dontrun{
#' library(dplyr)
#' n <- 20
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c('Pre', 'Post'), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#'   )
#' df
#' # Add character/factor columns for summary, tidy, format functions:
#' col_chr <- data.frame('chr1' = rep(c('A', 'B'), length.out = n),
#'                       'chr2' = rep(c('C', 'D'), length.out = n)
#'                       )
#' dim(col_chr)
#' df <- tibble::as.tibble(cbind(df, col_chr))
#' epi_head_and_tail(df)
#' epi_head_and_tail(df, last_cols = TRUE)
#' # Check variable types are what you expect:
#' epi_clean_count_classes(df)
#' str(df)
#' dim(df)
#' # Convert var_id to character:
#' df$var_id <- as.character(df$var_id)
#' # Get variables which are factors or characters:
#' vars_list <- df %>% select_if(~ epi_clean_cond_chr_fct(.)) %>% names(.)
#' vars_list <- c('var_to_rep', 'chr1', 'chr2')
#' # Generate a table:
#' desc_stats_fct <- epi_stats_fct_table(df = df,
#'                                       vars_list = vars_list
#'                                       )
#' desc_stats_fct
#'
#' }
#'
# #' @export
#


# TO DO: output is a list of lists, not a table
# How is this different to running epi_stats_summary() ?
# Leave as legacy

# epi_stats_fct_table <- function(df = NULL,
#                                 vars_list = ''
#                                ) {
#   desc_stats_fct <- vector(mode = 'list', length = length(vars_list))
#   names(desc_stats_fct) <- vars_list
#   for (i in 1:length(vars_list)) {
#     # i <- 1
#     val <- vars_list[i]
#     # Get a table for one variable, use functions above:
#     desc_stats <- epi_stats_summary(df[, val],
#                                     class_type = 'chr_fct',
#                                     action = 'exclude'
#                                     )
#     # Tidy:
#     desc_stats <- epi_stats_tidy(sum_df = desc_stats,
#                                  order_by = '<NA>',
#                                  perc_n = nrow(df[, vars_list])
#                                  )
#     # No need to format digits as should all be counts/integers
#     # Remove columns not informative:
#     desc_stats$row_sums <- NULL
#     desc_stats$percent <- NULL
#     # Rename NA column:
#     names(desc_stats)[grep(x = names(desc_stats), '<NA>', fixed = TRUE)] <- 'NA_count'
#     # Move headers to first row:
#     desc_stats <- rbind(colnames(desc_stats), desc_stats)
#     # Get rid of 'id' in row, leave blank:
#     desc_stats[1, 'id'] <- ' '
#     # Append as last rows:
#     desc_stats_fct[[i]] <- desc_stats
#     desc_stats_fct
#     }
#   return(desc_stats_fct)
#   }
