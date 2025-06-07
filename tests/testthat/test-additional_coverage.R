context("additional coverage tests")

library(episcout)
library(testthat)
library(dplyr)
library(stringi)

######################
print("Function being tested: epi_clean_count_classes with POSIXct")

test_that("epi_clean_count_classes counts multi-class columns", {
  df <- data.frame(time = as.POSIXct("2020-01-01") + 0:1,
                   value = 1:2)
  cc <- epi_clean_count_classes(df)
  expect_true(all(c("POSIXct","POSIXt","integer") %in% names(cc)))
  expect_equal(unname(cc["POSIXct"]), 1)
  expect_equal(unname(cc["POSIXt"]), 1)
})

######################
print("Function being tested: epi_stats_tidy")

test_that("epi_stats_tidy computes row sums and percent", {
  sum_df <- data.frame(id = c("g1","g1","g2","g2"),
                       x = c("A","B","A","B"),
                       n = c(2,3,4,1),
                       stringsAsFactors = FALSE)
  tidy_df <- epi_stats_tidy(sum_df, perc_n = 10)
  expect_equal(tidy_df$row_sums, c(5,5))
  expect_equal(tidy_df$percent, c(50,50))
  expect_equal(names(tidy_df)[1], "id")
})

######################
print("Function being tested: epi_plot_hist additional args")

test_that("epi_plot_hist passes extra parameters", {
  df <- data.frame(x = rnorm(100))
  skip_if_not_installed("ggthemes")
  p <- epi_plot_hist(df, "x", binwidth = 0.5)
  expect_s3_class(p, "ggplot")
  bw <- p$layers[[1]]$stat_params$binwidth
  expect_equal(bw, 0.5)
})

######################
print("Function being tested: epi_clean_compare_str with options")

test_that("epi_clean_compare_str uses additional parameters", {
  df <- data.frame(a = c("abc","ABC"), b = c("ABC","abc"), stringsAsFactors = FALSE)
  expect_false(epi_clean_compare_str(df, 2, "a", "b"))
  expect_true(epi_clean_compare_str(df, 2, "a", "b", opts_fixed = list(case_insensitive = TRUE)))
})

######################
print("Function being tested: epi_stats_corr")

test_that("epi_stats_corr returns correct correlations", {
  df <- data.frame(x = 1:5, y = seq(2,10,2))
  skip_if_not_installed("Hmisc")
  res <- epi_stats_corr(df, method = "pearson")
  expect_equal(res$cormat$r[1,2], 1)
  expect_equal(res$cormat$r[2,1], 1)
})

######################
print("Function being tested: epi_plot_bar custom palette")

test_that("epi_plot_bar applies custom palette", {
  df <- data.frame(cat = c("a","b","a","b"))
  p <- epi_plot_bar(df, "cat", custom_palette = c("red","blue"))
  expect_s3_class(p, "ggplot")
  cols <- p$scales$scales[[1]]$palette(2)
  expect_equal(cols, c("red","blue"))
})

######################
print("Function being tested: epi_read with custom NA strings")

test_that("epi_read handles additional NA strings", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a,b","1,NA","2,N/A"), tmp)
  df <- epi_read(tmp, na.strings = c("NA","N/A"))
  expect_true(all(is.na(df$b[1:2])))
  unlink(tmp)
})

######################
print("Function being tested: epi_stats_dates")

test_that("epi_stats_dates returns key statistics", {
  dates <- as.Date(c("2020-01-01","2020-06-01",NA,"2020-12-31"))
  res <- epi_stats_dates(dates)
  expect_equal(res$Value[res$Statistic=="N"], "4")
  expect_equal(res$Value[res$Statistic=="N Missing"], "1")
  expect_equal(res$Value[res$Statistic=="Min"], "2020-01-01")
  expect_equal(res$Value[res$Statistic=="Max"], "2020-12-31")
})

######################
print("Function being tested: epi_stats_dates_multi")

test_that("epi_stats_dates_multi summarises multiple columns", {
  df <- data.frame(a = as.Date("2020-01-01") + 0:2,
                   b = as.Date("2021-01-01") + 0:2)
  skip_if_not_installed("lubridate")
  res <- epi_stats_dates_multi(df)
  expect_true(all(c("Column","N","Min") %in% names(res)))
  expect_equal(nrow(res),2)
})

######################
print("Function being tested: epi_stats_summary error handling")

test_that("epi_stats_summary errors with bad parameters", {
  df <- data.frame(a = 1:3)
  expect_error(epi_stats_summary(df, class_type = "wrong"))
  expect_error(epi_stats_summary(df, action = "oops"))
})

######################
print("Function being tested: epi_clean_make_names options")

test_that("epi_clean_make_names handles duplicates and replacement", {
  strings <- c("a","a","b c")
  out <- epi_clean_make_names(strings, str_replacement = "-")
  expect_true(length(unique(out)) == length(out))
  expect_true(!any(grepl("-", out)))
  expect_true(all(make.names(out) == out))
})

######################
print("Function being tested: epi_clean_replace_value no match")

test_that("epi_clean_replace_value leaves values when no match", {
  df <- data.frame(col = c("cat","dog"), stringsAsFactors = FALSE)
  res <- epi_clean_replace_value(df, "col", "bird", "new")
  expect_equal(res, df$col)
})

######################
print("Function being tested: epi_clean_transpose roundtrip")

test_that("epi_clean_transpose roundtrip works", {
  df <- data.frame(id=1:3, a=4:6)
  df$id_col <- df$id
  tdf <- epi_clean_transpose(df, 3)
  # Recreate original by transposing back
  back <- epi_clean_transpose(tdf, 1)
  expect_equal(dim(back), dim(df))
})
