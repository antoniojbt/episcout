#' Generate a Wide-Format Summary Table for Multiple Variables
#'
#' This function creates a summary table for a dependent variable and one or more independent variables.
#' It calculates frequencies, reshapes the data into a wide format, and adds totals and percentages dynamically.
#'
#' @param df A data frame containing the data to analyze.
#' @param dep_var A string specifying the dependent variable (column in `df`).
#' @param ind_vars A character vector specifying the independent variable(s) (columns in `df`).
#' @return A data frame summarizing the relationships between the dependent variable and the independent variable(s).
#' @examples
#' # Example dataset
#' set.seed(42)
#' test_data <- data.frame(
#'   Outcome = sample(c("Yes", "No"), 100, replace = TRUE),
#'   Group = sample(c("A", "B", "C"), 100, replace = TRUE),
#'   Gender = sample(c("Male", "Female"), 100, replace = TRUE)
#' )
#'
#' # Generate a summary table
#' result <- epi_stats_contingency_nxn(test_data, dep_var = "Outcome", ind_vars = c("Group", "Gender"))
#' print(result)
#'
#' @importFrom tidyr pivot_wider
#' @importFrom stats xtabs
#' @importFrom stats reformulate ftable
#' @export
epi_stats_contingency_nxn <- function(df, dep_var, ind_vars) {
  # Validate input
  if (!all(c(dep_var, ind_vars) %in% colnames(df))) {
    stop("All specified variables must exist in the data frame.")
  }

  # Return empty data frame early if input has no rows
  if (nrow(df) == 0) {
    return(df[0, c(ind_vars, dep_var)])
  }

  # Create the formula for xtabs dynamically
  # formula_str <- sprintf("~ %s + %s", dep_var, paste(ind_vars, collapse = " + "))
  # formula_obj <- as.formula(formula_str)

  formula_obj <- reformulate(c(dep_var, ind_vars))

  # Create the contingency table
  f_tab <- ftable(xtabs(formula = formula_obj, data = df))

  # Convert the table to a data frame
  df_f_tab <- as.data.frame(f_tab)

  dep_var_levels <- unique(c(df[[dep_var]], "Yes", "No"))
  df_f_tab[[dep_var]] <- factor(df_f_tab[[dep_var]], levels = dep_var_levels)

  category_names <- character()

  df_f_tab_wide <- tidyr::pivot_wider(
    df_f_tab,
    names_from = dplyr::all_of(dep_var),
    values_from = "Freq",
    names_expand = TRUE,
    values_fill = 0
  )

  # Add totals and percentages dynamically
  if (all(dep_var_levels %in% colnames(df_f_tab_wide))) {
    category_names <- dep_var_levels
    df_f_tab_wide$total <- rowSums(df_f_tab_wide[, category_names], na.rm = TRUE)
    df_f_tab_wide[paste0("perc_", category_names)] <- round(
      sweep(df_f_tab_wide[, category_names], 1, df_f_tab_wide$total, "/") * 100, 2
    )
  }

  # Sort by the percentage of the first category
  if (length(category_names) > 0) {
    df_f_tab_wide <- df_f_tab_wide[order(df_f_tab_wide[[paste0("perc_", category_names[1])]], decreasing = TRUE), ]
  }

  # Return the final data frame
  return(df_f_tab_wide)
}
