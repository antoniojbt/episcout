#' Sub-sample a data frame balancing a binary outcome
#'
#' Draw a sub-sample without replacement from a data frame ensuring an
#' approximately balanced binary outcome distribution.
#'
#' @param df Data frame containing the data to be sampled.
#' @param outcome Name of the binary outcome column.
#' @param proportion Proportion of rows to sample (0 < proportion <= 1).
#'
#' @return A data frame containing the sampled rows.
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   outcome = rbinom(1000, 1, 0.7),
#'   value = rnorm(1000)
#' )
#' sub_df <- epi_sub_sample(df, "outcome", proportion = 0.1)
#' table(sub_df$outcome)
#' @export
epi_sub_sample <- function(df, outcome, proportion = 0.1) {
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  if (!outcome %in% names(df)) {
    stop("outcome must exist in df")
  }
  if (
    !is.numeric(proportion) || length(proportion) != 1 ||
      proportion <= 0 || proportion > 1
  ) {
    stop("proportion must be a number in (0, 1]")
  }
  outcome_vals <- df[[outcome]]
  outcome_levels <- unique(outcome_vals)
  if (length(outcome_levels) != 2) {
    stop("outcome must be binary")
  }
  n <- nrow(df)
  sample_size <- round(n * proportion)
  sample_size <- 2 * floor(sample_size / 2)
  group1 <- df[outcome_vals == outcome_levels[1], , drop = FALSE]
  group2 <- df[outcome_vals == outcome_levels[2], , drop = FALSE]
  per_group <- sample_size / 2
  if (per_group > nrow(group1) || per_group > nrow(group2)) {
    stop("Not enough rows in one outcome category for requested sample.")
  }
  idx1 <- sample(seq_len(nrow(group1)), size = per_group, replace = FALSE)
  idx2 <- sample(seq_len(nrow(group2)), size = per_group, replace = FALSE)
  rbind(group1[idx1, , drop = FALSE], group2[idx2, , drop = FALSE])
}
