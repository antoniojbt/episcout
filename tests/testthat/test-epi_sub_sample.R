library(testthat)

set.seed(42)
source_data <- data.frame(
  outcome = rep(c("yes", "no"), c(80, 20)),
  value = rnorm(100)
)

expected_counts <- round(table(source_data$outcome) * 0.5)

sampled <- epi_sub_sample(source_data, "outcome", 0.5, seed = 123)

# counts should be equal to expected round of sample size
expect_equal(table(sampled$outcome), expected_counts)

# proportion should match original outcome distribution
orig_prop <- prop.table(table(source_data$outcome))
sampled_prop <- prop.table(table(sampled$outcome))
expect_equal(sampled_prop, orig_prop, tolerance = 0.01)
