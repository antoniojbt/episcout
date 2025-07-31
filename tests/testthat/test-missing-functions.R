context("episcout additional function tests")

library(episcout)
library(testthat)

print("Function being tested: epi_create_dir")

test_that("epi_create_dir creates directories", {
  tmp_base <- file.path(tempdir(), "epi_create_dir_test")
  expect_message(
    path <- epi_create_dir(tmp_base, subdir = "sub"),
    "Created directory"
  )
  expect_true(dir.exists(path))
  expect_equal(dirname(path), normalizePath(tmp_base))
  expect_equal(basename(path), "sub")
  expect_message(
    epi_create_dir(tmp_base, subdir = "sub"),
    "Directory already exists"
  )
  unlink(tmp_base, recursive = TRUE)
})

print("Function being tested: epi_create_dir with date")

test_that("epi_create_dir uses today's date when subdir is NULL", {
  tmp_base <- file.path(tempdir(), "epi_create_dir_test_date")
  date_dir <- format(Sys.Date(), "%d_%m_%Y")
  path <- epi_create_dir(tmp_base)
  expect_true(dir.exists(path))
  expect_equal(dirname(path), normalizePath(tmp_base))
  expect_equal(basename(path), date_dir)
  unlink(tmp_base, recursive = TRUE)
})

print("Function being tested: epi_stats_prop_outcome")

test_that("epi_stats_prop_outcome computes proportion", {
  df <- data.frame(
    d_T0_outcome = c(1, 0, 1, 0, 1),
    d_time_cuts_prev = c("T0", "T1", "T0", "T1", "T0")
  )
  expect_output(
    res <- epi_stats_prop_outcome(
      df,
      "d_T0_outcome",
      "d_time_cuts_prev",
      "T0"
    ),
    "Proportion of deaths at T0: 1"
  )
  expect_equal(res, 1)
})

print("Function being tested: epi_write_df")

test_that("epi_write_df writes file and returns path", {
  skip_if_not_installed("data.table")
  df <- data.frame(a = 1:2)
  tmp_dir <- tempdir()
  expect_message(
    path <- epi_write_df(df, tmp_dir, "testfile", "tsv"),
    "File saved to"
  )
  expect_true(file.exists(path))
  expect_equal(path, file.path(tmp_dir, "testfile.tsv"))
  unlink(path)
})

print("Function being tested: epi_stats_factors")

test_that("epi_stats_factors summarises factor columns", {
  df <- data.frame(
    f1 = factor(c("a", "b", "a", NA, "a"), ordered = TRUE),
    f2 = factor(c("x", "x", "y", "y", "y"))
  )
  res <- epi_stats_factors(df)
  expect_equal(nrow(res), 2)
  f1 <- res[res$Variable == "f1", ]
  f2 <- res[res$Variable == "f2", ]
  expect_equal(f1$n_missing, 1)
  expect_equal(f1$complete_rate, 0.8)
  expect_true(f1$ordered)
  expect_equal(f1$n_unique, 2)
  expect_equal(f1$top_counts, "a (3), b (1)")
  expect_equal(f2$top_counts, "y (3), x (2)")
})

print("Function being tested: epi_stats_chars")

test_that("epi_stats_chars summarises character columns", {
  skip_if_not_installed("stringr")
  df <- data.frame(
    c1 = c("a", "", "c", "  ", NA),
    c2 = c("aa", "bb", "bb", "bb", ""),
    stringsAsFactors = FALSE
  )
  res <- epi_stats_chars(df)
  expect_equal(nrow(res), 2)
  c1 <- res[res$Variable == "c1", ]
  c2 <- res[res$Variable == "c2", ]
  expect_equal(c1$n_missing, 1)
  expect_equal(c1$empty, 1)
  expect_equal(c1$whitespace, 1)
  expect_equal(c2$max_length, 2)
  expect_equal(c2$n_unique, 3)
})

print("Function being tested: truncate_name and make_unique_name")

test_that("truncate_name limits length", {
  long <- paste(rep("a", 35), collapse = "")
  expect_equal(getFromNamespace("truncate_name", "episcout")(long), substr(long, 1, 29))
  expect_equal(getFromNamespace("truncate_name", "episcout")("short"), "short")
})

test_that("make_unique_name generates unique names", {
  names1 <- c("Sheet1", "Sheet2")
  expect_equal(getFromNamespace("make_unique_name", "episcout")("Sheet3", names1), "Sheet3")
  names2 <- c("Sheet", "Sheet_2", "Sheet_3")
  expect_equal(getFromNamespace("make_unique_name", "episcout")("Sheet", names2), "Sheet_4")
  names3 <- "name"
  expect_equal(getFromNamespace("make_unique_name", "episcout")("NAME", names3), "NAME_2")
})
