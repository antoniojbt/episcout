#' Calculate the proportion of individuals who suffered an event
#' at a given point in time
#'
#' It can be used to estimate the case fatality rate.
#'
#' @param df A data frame containing the data.
#' @param outcome_var_window A string representing the column name of the outcome variable at the analysis window. Expects an integer where 1 = event, 0 = no event.
#' @param pop_at_risk_var A string representing the column name of the population at risk variable. This is matched against the analysis window to obtain a subset.
#' @param analysis_window A value from the variable passed as `pop_at_risk_var` representing the analysis window.
#' @param round_dig An integer representing the number of decimal places to round the result to. Default is 4.
#'
#' @return The proportion of individuals who suffered the event at the given point, printed and returned as a numeric value.
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   d_T0_outcome = c(1, 0, 1, 0, 1),
#'   d_time_cuts_prev = c('T0', 'T1', 'T0', 'T1', 'T0')
#' )
#' outcome_var_window <- 'd_T0_outcome'
#' pop_at_risk_var <- 'd_time_cuts_prev'
#' analysis_window <- 'T0'
#' epi_stats_prop_outcome(df, outcome_var_window, pop_at_risk_var, analysis_window)
#' }
#' @export

epi_stats_prop_outcome <- function(df,
                                   outcome_var_window,
                                   pop_at_risk_var,
                                   analysis_window,
                                   round_dig = 4) {
  # Population with outcome:
  pop_w_outcome <- length(which(df[[outcome_var_window]] == 1))

  # Population at risk:
  pop_at_risk <- length(which(df[[pop_at_risk_var]] == analysis_window))

  # Proportion of deaths
  prop_death <- round((pop_w_outcome / pop_at_risk), round_dig)

  # Print result
  result <- sprintf('Proportion of deaths at %s: %s', analysis_window, prop_death)
  print(result)

  # Return the proportion for further use
  return(prop_death)
}
