context("additional coverage tests 2")

library(episcout)
library(testthat)


test_that("epi_clean_cond_date ignores string dates", {
  date_chr <- c("2020-01-01", "2020-01-02")
  expect_false(epi_clean_cond_date(date_chr))
})


test_that("epi_clean_count_classes counts factors", {
  df <- data.frame(a = factor(c("x","y")), b = 1:2)
  cc <- epi_clean_count_classes(df)
  expect_true("factor" %in% names(cc))
  expect_equal(unname(cc["factor"]), 1)
})


test_that("epi_stats_tidy errors when perc_n missing", {
  df <- data.frame(id = c("g1","g2"), x = c("A","B"), n = c(1,2), stringsAsFactors = FALSE)
  expect_error(epi_stats_tidy(df), "perc_n must be passed")
})


test_that("epi_plot_hist maps variable correctly", {
  skip_if_not_installed("ggthemes")
  df <- data.frame(x = rnorm(10))
  p <- epi_plot_hist(df, "x")
  # mapping stored as .data[[var]] expression
  expect_true(grepl("x", deparse(p$mapping$x)))
  expect_s3_class(p, "ggplot")
})


test_that("epi_clean_compare_str detects substring", {
  df <- data.frame(a = "abcdef", b = "cde", stringsAsFactors = FALSE)
  expect_true(epi_clean_compare_str(df, 1, "a", "b"))
  df$b <- "xyz"
  expect_false(epi_clean_compare_str(df, 1, "a", "b"))
})


test_that("epi_stats_corr returns melted tables", {
  skip_if_not_installed("Hmisc")
  df <- data.frame(a = 1:5, b = 1:5)
  res <- epi_stats_corr(df)
  expect_true(all(c("cormat","cormat_melted_r","cormat_melted_pval") %in% names(res)))
  expect_equal(nrow(res$cormat_melted_r), 4)
  expect_equal(res$cormat$r[1,2], 1)
})


test_that("epi_read returns tibble with correct types", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a,b","1,2","3,4"), tmp)
  df <- epi_read(tmp)
  expect_s3_class(df, "tbl_df")
  expect_equal(ncol(df), 2)
  expect_true(is.integer(df$a))
  unlink(tmp)
})


test_that("epi_clean_replace_value replaces matches", {
  df <- data.frame(col = c("cat","dog"), stringsAsFactors = FALSE)
  res <- epi_clean_replace_value(df, "col", "cat", "mouse")
  expect_equal(res[1], "mouse")
  expect_equal(res[2], "dog")
})


test_that("epi_clean_transpose preserves names", {
  df <- data.frame(id=1:2, a=3:4)
  df$id_col <- df$id
  tdf <- epi_clean_transpose(df, 3)
  expect_equal(as.character(tdf[1,1]), "a")
  expect_equal(colnames(tdf)[2], "1")
})

