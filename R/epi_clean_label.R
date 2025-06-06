
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
    # Ensure the lookup dataframe is in the correct format
    lookup_df <- lookup_df %>%
        dplyr::mutate(level = as.character(level))

    # Iterate over each variable in the lookup table
    for (v in unique(lookup_df$variable)) {
        target_col <- if (v %in% names(data_df)) v else if ("variable" %in% names(data_df)) "variable" else NULL
        if (!is.null(target_col)) {
            levels_and_labels <- lookup_df %>%
                dplyr::filter(variable == v) %>%
                dplyr::select(level, label) %>%
                dplyr::arrange(as.numeric(level))

            data_df[[target_col]] <- factor(data_df[[target_col]],
                                            levels = levels_and_labels$level,
                                            labels = levels_and_labels$label)
        }
    }

    return(data_df)
}

# TO DO:
# Needs error handling for missing variables or discrepancies in level assignments
