#' Summarize Factor Variables
#'
#' @param df A dataframe containing factor variables
#' @return A tibble summarizing factor variables in wide format
epi_stats_factors <- function(df) {
    df %>%
        dplyr::select(dplyr::where(is.factor)) %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Value") %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise(
            n_missing = sum(is.na(Value)),  # Missing values count
            complete_rate = mean(!is.na(Value)),  # Proportion of non-missing values
            ordered = unique(is.ordered(Value)),  # Is the factor ordered?
            n_unique = dplyr::n_distinct(Value, na.rm = TRUE),  # Unique levels count
            top_counts = paste(names(sort(table(Value), decreasing = TRUE)[1:3]),
                                      sort(table(Value), decreasing = TRUE)[1:3],
                                      sep = " (", collapse = "), ") # Most common factor levels
        ) %>%
        dplyr::ungroup()
}

#' Summarize Character Variables
#'
#' @param df A dataframe containing character variables
#' @return A tibble summarizing character variable statistics
epis_stats_chars <- function(df) {
    if (!requireNamespace('stringr', quietly = TRUE)) {
        stop('Package stringr needed for this function to work. Please install it.',
             call. = FALSE)
    }
    df %>%
        dplyr::select(dplyr::where(is.character)) %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Value") %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise(
            n_missing = sum(is.na(Value)),  # Count of missing values
            complete_rate = mean(!is.na(Value)),  # Proportion of non-missing values
            min_length = dplyr::if_else(n_missing < dplyr::n(), min(nchar(Value), na.rm = TRUE), NA_integer_),  # Shortest character length
            max_length = dplyr::if_else(n_missing < dplyr::n(), max(nchar(Value), na.rm = TRUE), NA_integer_),   # Longest character length
            empty = sum(Value == "", na.rm = TRUE),  # Count of empty strings
            n_unique = dplyr::n_distinct(Value, na.rm = TRUE),  # Count of unique values
            whitespace = sum(stringr::str_trim(Value) == "", na.rm = TRUE)  # Count of whitespace-only strings
        ) %>%
        dplyr::ungroup()
}
