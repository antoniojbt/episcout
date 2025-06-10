#' @title Recursively merge data frames that are stored as lists within a list
#'
#' @description Recursively merge data frames that are stored as lists within a list.
#' Flattens with purrr::flatten() if there is more than one level. Assumes:
#' - there are are 3 or more data frames to merge
#'   (if only two are supplied the function performs a single merge and returns)
#' - there are no duplicates in any of the data frames
#' The function performs a full outer join with base R
#' merge(df1, df2, by = id_col, all = TRUE)
#'
#' @param nested_list_dfs A nested list of dataframes to merge, such as the output
#' from \code{\link{epi_clean_spread_repeated}}.
#'
#' @param id_col A string to identify the column to merge by. This is passed to
#' the by parameter in merge(). Requires all dataframes to have the same column
#' name.
#'
#' @param all.x corresponds to merge() all.x parameter. TRUE by default.
#'
#' @param ... any further arguments that merge.data.frame or merge.data.table can
#' take.
#'
#' @return A data.table in wide format with each sub-dataframe contained as a
#' sub-list
#'
#' @note This function helps with spreading and gathering long and wide dataframes.
#' You may want to see \code{\link[tidyr]{gather}}, \code{\link[tidyr]{spread}} as
#' well as similar base functions and other packages such as data.table depending
#' on your problem. See example below in case you have a messier dataframe which
#' doesn't easily yield to existing workflows and functions. Note that
#' merge.data.table is dispatched (as opposed to merge.data.frame). To get all = TRUE,
#' pass all.x = TRUE and all.y = TRUE.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_add_colname_suffix}},
#' \code{\link{epi_clean_spread_repeated}},
#' \code{\link{epi_clean_transpose}}, \code{\link[base]{merge}}.
#'
#' @examples
#' \dontrun{
#' # Generate some data:
#' n <- 20
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c("Pre", "Post"), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' df
#' # Create a nested list of dataframes using the repeated measurements variable:
#' df_spread <- epi_clean_spread_repeated(df, "var_to_rep", 1)
#' # Returns a nested list:
#' df_spread
#'
#' # Run an example with epi_clean_merge_nested_dfs()
#' # to create a single dataframe with repeated observations spread and
#' # no duplicate IDs (create a wide instead of a long dataframe):
#' library(purrr)
#' library(tibble)
#' nested_list_dfs <- purrr::flatten(list(df_spread, df_spread, df_spread))
#' id_col <- "var_id"
#' epi_list_head(nested_list_dfs, 2, 3)
#' epi_list_tail(nested_list_dfs, 2, 3)
#' all_merged <- epi_clean_merge_nested_dfs(nested_list_dfs, id_col)
#' dim(all_merged)
#' as.tibble(all_merged)
#' names(all_merged)
#'
#' # The above with epi_clean_merge_nested_dfs() would be equivalent to
#' # iteratively doing the following:
#' library(dplyr)
#'
#' # Create sets with distinct observations:
#' var_id <- "var_id"
#' var_to_rep <- "var_to_rep"
#' reps <- epi_clean_add_rep_num(df, "var_id", "var_to_rep")
#' reps
# Sanity check:
#' identical(
#'   as.character(reps[[var_id]]),
#'   as.character(df[[var_id]])
#' ) # should be TRUE
#' # Bind:
#' df2 <- as.tibble(cbind(df, "rep_num" = reps$rep_num))
#' # merge() adds all rows from both data frames as there are duplicates
#' # so use cbind after making sure order is exact
#' epi_head_and_tail(df2, rows = 3)
#' epi_head_and_tail(df2, rows = 3, last_cols = TRUE)
#'
#' # See how many replicates there are:
#' df2 %>%
#'   transmute(as.factor(rep_num)) %>%
#'   summary()
#'
#' # Generate a data frame for each:
#' baseline <- df2 %>% filter(rep_num == 1)
#' baseline
#' # Sanity check, should be empty:
#' epi_clean_get_dups(baseline, "var_id", 1)
#' # Change col names to baseline, time_1, time_2, etc.:
#' new_colnames <- epi_clean_add_colname_suffix(baseline, 1, ".0")
#' names(baseline)[2:ncol(baseline)] <- new_colnames
#' names(baseline)
#'
#' # First set of repeated observations:
#' time_1 <- df2 %>% filter(rep_num == 2)
#' time_1
#' epi_clean_get_dups(time_1, "var_id", 1)
#' # Change col names:
#' new_colnames <- epi_clean_add_colname_suffix(time_1, 1, ".1")
#' names(time_1)[2:ncol(time_1)] <- new_colnames
#' names(time_1)
#'
#' # Nothing left:
#' df2 %>% filter(rep_num == 3)
#'
#' # Merge the data frames into one:
#' all_merged <- merge(baseline, time_1, by = "var_id", all = TRUE)
#' dim(all_merged)
#' as.tibble(all_merged)
#' names(all_merged)
#' epi_head_and_tail(all_merged)
#' epi_head_and_tail(all_merged, last_cols = TRUE)
#' View(all_merged)
#' }
#'
#' @export
#'

epi_clean_merge_nested_dfs <- function(nested_list_dfs = NULL,
                                       id_col = "",
                                       all.x = TRUE,
                                       ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package data.table needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  # Check there are at least two data frames
  if (length(nested_list_dfs) < 2) {
    stop("nested_list_dfs must contain at least two data frames")
  }
  # Initialise merge:
  df1 <- data.table::as.data.table(nested_list_dfs[[1]])
  df2 <- data.table::as.data.table(nested_list_dfs[[2]])
  print("Merging first two data frames")
  # If there are names in list but they are duplicated:
  if (!is.null(names(nested_list_dfs)) && any(duplicated(names(nested_list_dfs)))) {
   message('Duplicated names in list passed. Using default suffixes.')
  }
  # If there are names and no duplicates:
  if (!is.null(names(nested_list_dfs)) && !any(duplicated(names(nested_list_dfs)))) {
    suffix_1 <- paste0('_', names(nested_list_dfs)[1])
    suffix_2 <- paste0('_', names(nested_list_dfs)[2])
    print(sprintf('Using suffixes: %s and %s',
                  suffix_1,
                  suffix_2)
          )
  } else {
    suffix_1 <- "_1"
    suffix_2 <- "_2"
    print(sprintf(
      "Duplicated names or no names in list passed,
                   using %s and %s as suffixes",
      suffix_1,
      suffix_2
    ))
  }
  temp_df <- merge(df1, df2,
    by = id_col,
    all.x = all.x,
    suffixes = c(suffix_1, suffix_2),
    ...
  )
  if (length(nested_list_dfs) == 2) {
    print("Only two data frames provided, returning result of single merge.")
    return(temp_df)
  }
  # Loop through nested data frames and merge each to previous merged df:
  # TO DO: if there were truly many and large DFs could add a parallel option
  print("Merging further dataframes.")
  print("Suffixes are only used if there are clashes.")
  for (i in 3:length(nested_list_dfs)) { # skip 1 and 2 as these are
                                         # the initial merge
    if (!is.null(names(nested_list_dfs)) && !any(duplicated(names(nested_list_dfs)))) {
        # suffix_1 should just be blank as will be a merged df already
        suffix_2 <- paste0('_', names(nested_list_dfs)[i])
        } else {
          suffix_2 <- sprintf('_%s', i)
        }
      print(sprintf('Suffix to use: %s', suffix_2))
      df2 <- data.table::as.data.table(nested_list_dfs[[i]]) # new df to merge, starting at 3
      temp_df <- merge(temp_df,
                       df2,
                       by = id_col,
                       all.x = all.x,
                       suffixes = c('', suffix_2),
                       ...
                       )
    # option suffix is only used if there is a clash
    }
  print("Done merging")
  return(temp_df)
}
