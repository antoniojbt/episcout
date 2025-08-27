test_that("epi_sub_sample preserves outcome distribution", {
  set.seed(123)
  df <- data.frame(
    id = 1:1000,
    outcome = rbinom(1000, 1, 0.7),
    other = rnorm(1000)
  )
  original <- prop.table(table(df$outcome))
  sub <- epi_sub_sample(df, "outcome", sample_prop = 0.2)
  tab <- prop.table(table(sub$outcome))
  expect_equal(tab, original, tolerance = 0.05)
})

test_that("epi_sub_sample samples without replacement", {
  set.seed(456)
  df <- data.frame(
    id = 1:100,
    outcome = rbinom(100, 1, 0.6)
  )
  sub <- epi_sub_sample(df, "outcome", sample_prop = 0.5)
  expect_equal(nrow(sub), 50)
  expect_equal(length(unique(sub$id)), nrow(sub))
})
