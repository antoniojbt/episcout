# tests/testthat/test-epi_stats_fct_table.R

library(testthat)

context("epi_stats_fct_table")

test_that("counts factor levels including NA", {
  df <- tibble::tibble(
    f1 = factor(c("a", "a", "b", NA)),
    f2 = c("x", "x", "y", "x"),
    num = 1:4
  )

  res <- epi_stats_fct_table(df)

  expect_s3_class(res, "tbl_df")
  expect_equal(colnames(res), c("variable", "level", "count"))
  expect_setequal(unique(res$variable), c("f1", "f2"))

  f1_counts <- dplyr::filter(res, variable == "f1")
  expect_equal(dplyr::filter(f1_counts, level == "a")$count, 2)
  expect_equal(dplyr::filter(f1_counts, level == "b")$count, 1)
  expect_equal(dplyr::filter(f1_counts, level == "NA")$count, 1)
})

test_that("vars_list restricts columns", {
  df <- tibble::tibble(
    f1 = factor(c("a", "b")),
    f2 = factor(c("x", "y"))
  )

  res <- epi_stats_fct_table(df, vars_list = "f2")
  expect_true(all(res$variable == "f2"))
})
