#' Generate a Summary Table
#'
#' This function creates a summary table from a dataset, focusing on a dependent variable and its relationship with one or more independent variables.
#' It calculates frequencies, reshapes the data into a wide format, and adds totals and percentages for easy interpretation.
#'
#' @param df A data frame containing the data to analyze.
#' @param dep_var A string specifying the name of the dependent variable (column in `df`).
#' @param ind_vars A string or character vector specifying the names of the independent variable(s) (columns in `df`).
#' @return A data frame with the reshaped summary table including frequencies, totals, and percentages, sorted by the percentage of "vacante".
#' @examples
#' # Example dataset
#' data_f <- data.frame(
#'   PLZOCU = c(1, 0, 1, 0, 1),
#'   CLASIF_UNIDAD = c("A", "A", "B", "B", "B")
#' )
#'
#' # Call the function
#' result <- epi_stats_table(data_f, dep_var = "PLZOCU", ind_vars = "CLASIF_UNIDAD")
#' print(result)
#'
#' @importFrom tidyr pivot_wider
#' @importFrom stats xtabs
#' @export

# df = data_f
# dep_var = "PLZOCU"
# ind_vars = c("CLASIF_UNIDAD")

epi_stats_table <- function(df, dep_var, ind_vars) {
  # Create the formula for xtabs dynamically
  formula_str <- sprintf("~ %s + %s", dep_var, ind_vars)
  formula_obj <- as.formula(formula_str)
  formula_obj

  # Create the ftable
  f_tab <- ftable(xtabs(formula_obj, data = df))

  # Convert to data frame
  df_f_tab <- as.data.frame(f_tab)

  # Reshape to wide format
  df_f_tab_wide <- tidyr::pivot_wider(
    df_f_tab,
    names_from = all_of(dep_var),
    values_from = c(Freq)
  )

  # Rename columns
  colnames(df_f_tab_wide)[colnames(df_f_tab_wide) == "0"] <- "vacante"
  colnames(df_f_tab_wide)[colnames(df_f_tab_wide) == "1"] <- "ocupada"

  # Add totals and percentages
  df_f_tab_wide$total <- df_f_tab_wide$vacante + df_f_tab_wide$ocupada
  df_f_tab_wide$perc_vacante <- round((df_f_tab_wide$vacante / df_f_tab_wide$total) * 100, 2)
  df_f_tab_wide$perc_ocupada <- round((df_f_tab_wide$ocupada / df_f_tab_wide$total) * 100, 2)

  # Sort by percentage of vacante
  df_f_tab_wide <- df_f_tab_wide[order(df_f_tab_wide$perc_vacante, decreasing = TRUE), ]

  # Return the final data frame for inspection
  return(df_f_tab_wide)
}
