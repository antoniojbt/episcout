#' Summarize Factor Variables
#'
#' @param df A dataframe containing factor variables
#' @return A tibble summarizing factor variables in wide format
epi_stats_factors <- function(df) {
    df %>%
        select(where(is.factor)) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        group_by(Variable) %>%
        summarise(
            n_missing = sum(is.na(Value)),  # Missing values count
            complete_rate = mean(!is.na(Value)),  # Proportion of non-missing values
            ordered = unique(is.ordered(Value)),  # Is the factor ordered?
            n_unique = n_distinct(Value, na.rm = TRUE),  # Unique levels count
            top_counts = paste(names(sort(table(Value), decreasing = TRUE)[1:3]),
                                      sort(table(Value), decreasing = TRUE)[1:3],
                                      sep = " (", collapse = "), ") # Most common factor levels
        ) %>%
        ungroup()
}

#' Summarize Character Variables
#'
#' @param df A dataframe containing character variables
#' @return A tibble summarizing character variable statistics
epis_stats_chars <- function(df) {
    df %>%
        select(where(is.character)) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        group_by(Variable) %>%
        summarise(
            n_missing = sum(is.na(Value)),  # Count of missing values
            complete_rate = mean(!is.na(Value)),  # Proportion of non-missing values
            min_length = if_else(n_missing < n(), min(nchar(Value), na.rm = TRUE), NA_integer_),  # Shortest character length
            max_length = if_else(n_missing < n(), max(nchar(Value), na.rm = TRUE), NA_integer_),   # Longest character length
            empty = sum(Value == "", na.rm = TRUE),  # Count of empty strings
            n_unique = n_distinct(Value, na.rm = TRUE),  # Count of unique values
            whitespace = sum(str_trim(Value) == "", na.rm = TRUE)  # Count of whitespace-only strings
        ) %>%
        ungroup()
}
