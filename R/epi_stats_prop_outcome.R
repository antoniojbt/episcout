#' Calculate the proportion of individuals who suffered an event at a given point in time
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
#' @details Both the event count and its denominator are calculated from rows matching `analysis_window`. Eligible outcomes must be non-missing binary numeric or logical values. The function fails if the requested columns are absent, no rows match the analysis window, or eligible outcomes are missing or non-binary.
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   d_T0_outcome = c(1, 0, 1, 0, 1),
#'   d_time_cuts_prev = c("T0", "T1", "T0", "T1", "T0")
#' )
#' outcome_var_window <- "d_T0_outcome"
#' pop_at_risk_var <- "d_time_cuts_prev"
#' analysis_window <- "T0"
#' epi_stats_prop_outcome(df, outcome_var_window, pop_at_risk_var, analysis_window)
#' }
#' @export

epi_stats_prop_outcome <- function(df,
                                   outcome_var_window,
                                   pop_at_risk_var,
                                   analysis_window,
                                   round_dig = 4) {
  if (!is.data.frame(df)) {
    stop("df must be a data frame.", call. = FALSE)
  }
  if (!is.character(outcome_var_window) || length(outcome_var_window) != 1L ||
        is.na(outcome_var_window) || !nzchar(outcome_var_window)) {
    stop("outcome_var_window must be a single non-empty column name.", call. = FALSE)
  }
  if (!outcome_var_window %in% names(df)) {
    stop(
      sprintf("outcome_var_window `%s` is absent from df.", outcome_var_window),
      call. = FALSE
    )
  }
  if (!is.character(pop_at_risk_var) || length(pop_at_risk_var) != 1L ||
        is.na(pop_at_risk_var) || !nzchar(pop_at_risk_var)) {
    stop("pop_at_risk_var must be a single non-empty column name.", call. = FALSE)
  }
  if (!pop_at_risk_var %in% names(df)) {
    stop(
      sprintf("pop_at_risk_var `%s` is absent from df.", pop_at_risk_var),
      call. = FALSE
    )
  }
  if (length(analysis_window) != 1L || is.na(analysis_window)) {
    stop("analysis_window must be a single non-missing value.", call. = FALSE)
  }

  window_values <- df[[pop_at_risk_var]]
  eligible <- !is.na(window_values) & window_values == analysis_window
  if (!any(eligible)) {
    stop("No eligible rows match analysis_window.", call. = FALSE)
  }

  eligible_outcomes <- df[[outcome_var_window]][eligible]
  if (anyNA(eligible_outcomes)) {
    stop("Eligible rows contain a missing outcome.", call. = FALSE)
  }
  if ((!is.numeric(eligible_outcomes) && !is.logical(eligible_outcomes)) ||
        any(!eligible_outcomes %in% c(0, 1))) {
    stop("Eligible outcomes must be binary 0/1 values.", call. = FALSE)
  }

  pop_w_outcome <- sum(eligible_outcomes == 1)
  pop_at_risk <- length(eligible_outcomes)

  prop_death <- round((pop_w_outcome / pop_at_risk), round_dig)

  # Print result
  result <- sprintf("Proportion of deaths at %s: %s", analysis_window, prop_death)
  print(result)

  # Return the proportion for further use
  prop_death
}
