
test_that("integer columns are converted to factors", {
  dt <- data.table::data.table(
    a = 1:3,
    b = as.integer(4:6),
    c = data.table::as.IDate("2020-01-01") + 0:2
  )
  res <- epi_clean_int_to_factor(dt)
  expect_true(is.factor(res$a))
  expect_true(is.factor(res$b))
  expect_true(inherits(res$c, "IDate"))
})
