#' Subsample a dataset while preserving class balance
#'
#' Draws a stratified random sample from a data frame using an outcome
#' variable to maintain class balance. The same proportion of rows is drawn
#' from each class.
#'
#' @param data A data.frame containing the data to sample from.
#' @param outcome_var A character string giving the name of the outcome column.
#' @param sample_prop A numeric value between 0 and 1 indicating the proportion
#'   of rows to sample from each outcome class.
#' @param seed Optional numeric seed to make the sampling reproducible.
#'
#' @return A data frame containing the stratified subsample.
#' @export
#'
#' @examples
#' df <- data.frame(outcome = rep(c("a", "b"), c(80, 20)), value = rnorm(100))
#' epi_sub_sample(df, "outcome", 0.5, seed = 123)
epi_sub_sample <- function(data, outcome_var, sample_prop, seed = NULL) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  if (!is.character(outcome_var) || length(outcome_var) != 1) {
    stop("outcome_var must be a single character string")
  }
  if (!outcome_var %in% names(data)) {
    stop("outcome_var must be a column in data")
  }
  if (!is.numeric(sample_prop) || length(sample_prop) != 1 ||
    sample_prop <= 0 || sample_prop > 1) {
    stop("sample_prop must be a numeric between 0 and 1")
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be a single numeric value")
    }
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv))
    }
    set.seed(seed)
  }
  data %>%
    dplyr::group_by(.data[[outcome_var]]) %>%
    dplyr::sample_frac(sample_prop) %>%
    dplyr::ungroup()
}
