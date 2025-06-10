#' Summarize Factor Variables
#'
#' @param df A dataframe containing factor variables
#' @return A tibble summarizing factor variables in wide format
epi_stats_factors <- function(df) {
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package purrr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  factor_df <- df %>%
    dplyr::select(dplyr::where(is.factor))

  purrr::imap_dfr(factor_df, function(col, nm) {
    counts <- sort(table(col), decreasing = TRUE)
    tibble::tibble(
      Variable = nm,
      n_missing = sum(is.na(col)),
      complete_rate = mean(!is.na(col)),
      ordered = is.ordered(col),
      n_unique = dplyr::n_distinct(col, na.rm = TRUE),
      top_counts = paste(names(counts)[seq_len(min(3, length(counts)))],
        counts[seq_len(min(3, length(counts)))],
        sep = " (", collapse = ", "
      )
    )
  })
}

#' Summarize Character Variables
#'
#' @param df A dataframe containing character variables
#' @return A tibble summarizing character variable statistics
epis_stats_chars <- function(df) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package stringr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  df %>%
    dplyr::select(dplyr::where(is.character)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(
      n_missing = sum(is.na(Value)), # Count of missing values
      complete_rate = mean(!is.na(Value)), # Proportion of non-missing values
      min_length = dplyr::if_else(n_missing < dplyr::n(), min(nchar(Value), na.rm = TRUE), NA_integer_), # Shortest character length
      max_length = dplyr::if_else(n_missing < dplyr::n(), max(nchar(Value), na.rm = TRUE), NA_integer_), # Longest character length
      empty = sum(Value == "", na.rm = TRUE), # Count of empty strings
      n_unique = dplyr::n_distinct(Value, na.rm = TRUE), # Count of unique values
      whitespace = sum(stringr::str_trim(Value) == "", na.rm = TRUE) # Count of whitespace-only strings
    ) %>%
    dplyr::ungroup()
}
