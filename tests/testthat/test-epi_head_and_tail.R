# tests for epi_head_and_tail
library(testthat)
library(episcout)

test_that("rows and cols are clamped to data dimensions", {
  df <- data.frame(a = 1:3, b = 4:6)
  out <- epi_head_and_tail(df, rows = 10, cols = 10)
  expect_equal(nrow(out), 6)
  expect_equal(ncol(out), 2)
})

test_that("rows and cols below 1 are adjusted", {
  df <- data.frame(a = 1:3, b = 4:6)
  out <- epi_head_and_tail(df, rows = 0, cols = 0)
  expect_equal(nrow(out), 2)
  expect_equal(ncol(out), 1)
})

test_that("last_cols selects columns from the end", {
  df <- data.frame(a = 1:3, b = 4:6)
  out <- epi_head_and_tail(df, rows = 1, cols = 1, last_cols = TRUE)
  expect_equal(colnames(out), "b")
})
