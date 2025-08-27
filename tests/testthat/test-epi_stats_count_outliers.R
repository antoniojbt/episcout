test_that("coef zero returns zero outliers", {
  expect_equal(epi_stats_count_outliers(num_vec = c(1, 2, 100), coef = 0), 0L)
})

test_that("negative coef errors", {
  expect_error(epi_stats_count_outliers(num_vec = 1:5, coef = -1))
})

test_that("non-numeric vector errors", {
  expect_error(epi_stats_count_outliers(num_vec = c("a", "b")))
})

test_that("all NA vector returns zero", {
  expect_equal(
    epi_stats_count_outliers(num_vec = c(NA_real_, NA_real_, NA_real_)),
    0L
  )
})
