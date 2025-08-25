#' @title Apply Factor Levels and Labels Based on a Lookup Table
#'
#' @description This function takes a dataframe and a lookup dataframe that contains the
#' mapping of factors' levels and labels for specific variables. It applies
#' these levels and labels to the corresponding variables in the target dataframe.
#'
#' @param data_df A dataframe to which factor levels and labels will be applied.
#'   This dataframe should contain columns that match the variable names specified
#'   in the lookup dataframe.
#' @param lookup_df A lookup dataframe containing at least three columns: `variable`,
#'   `level`, and `label`, where `variable` is the name of the column in `data_df`
#'   for which levels and labels are specified, `level` are the factor levels (as
#'   character), and `label` are the corresponding labels for these levels.
#'
#' @return A modified version of `data_df` with specified columns converted to factors
#'   with levels and labels as defined in `lookup_df`.
#'
#' @examples
#' # Assume data_df is already loaded and contains 'ORIGEN' and 'SECTOR' columns
#' # Assume lookup_df is set up with columns 'variable', 'level', and 'label'
#' # result_df <- epi_clean_label(data_df, lookup_df)
#'
#' @export
#'

epi_clean_label <- function(data_df, lookup_df) {
  required_cols <- c("variable", "level", "label")
  if (!all(required_cols %in% names(lookup_df))) {
    stop(sprintf("lookup_df must contain columns: %s", paste(required_cols, collapse = ", ")))
  }
  lookup_df <- lookup_df %>% dplyr::mutate(level = as.character(level))

  missing_vars <- setdiff(unique(lookup_df$variable), names(data_df))
  if (length(missing_vars) > 0) {
    stop(sprintf("Variables not found in data_df: %s", paste(missing_vars, collapse = ", ")))
  }

  for (target_col in unique(lookup_df$variable)) {
    levels_and_labels <- lookup_df %>%
      dplyr::filter(variable == target_col) %>%
      dplyr::select(level, label) %>%
      dplyr::arrange(as.numeric(level))

    data_levels <- unique(as.character(data_df[[target_col]]))
    lookup_levels <- levels_and_labels$level
    extra_lookup <- setdiff(lookup_levels, data_levels)
    if (length(extra_lookup) > 0) {
      warning(sprintf("Lookup for %s contains levels not in data_df: %s", target_col, paste(extra_lookup, collapse = ", ")))
    }
    extra_data <- setdiff(data_levels, lookup_levels)
    if (length(extra_data) > 0) {
      warning(sprintf("data_df for %s contains levels not in lookup_df: %s", target_col, paste(extra_data, collapse = ", ")))
    }

    data_df[[target_col]] <- factor(
      as.character(data_df[[target_col]]),
      levels = levels_and_labels$level,
      labels = levels_and_labels$label
    )
  }

  data_df
}
