test_that("epi_sub_sample balances outcome via weights", {
  set.seed(123)
  df <- data.frame(
    id = 1:1000,
    outcome = rbinom(1000, 1, 0.7),
    other = rnorm(1000)
  )
  original <- prop.table(table(df$outcome))
  sub <- epi_sub_sample(df, "outcome", proportion = 0.2)
  tab <- prop.table(table(sub$outcome))
  # Threshold for considering the original data imbalanced: if the absolute difference
  # between class proportions is greater than 0.1, we consider it imbalanced for this test.
  IMBALANCE_THRESHOLD <- 0.1
  expect_gt(abs(original[1] - original[2]), IMBALANCE_THRESHOLD)
  expect_true(all(abs(tab - 0.5) < balance_tolerance))
})

test_that("epi_sub_sample samples without replacement", {
  set.seed(456)
  df <- data.frame(
    id = 1:100,
    outcome = rbinom(100, 1, 0.6)
  )
  sub <- epi_sub_sample(df, "outcome", proportion = 0.5)
  expect_equal(nrow(sub), 50)
  expect_equal(length(unique(sub$id)), nrow(sub))
})
