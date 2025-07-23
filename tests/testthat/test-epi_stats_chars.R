# tests/testthat/test-epi_stats_chars.R

library(testthat)
library(dplyr)
library(tidyr)
library(stringr)

# If you haven't installed your package or sourced the function yet, uncomment:
# source("R/epi_stats_chars.R")

context("epi_stats_chars")

test_that("correct summary for a simple character column", {
  df <- tibble(
    x = c("abc", NA, "")
  )
  res <- epi_stats_chars(df)

  # There should be one row for variable "x"
  expect_equal(nrow(res), 1L)
  expect_equal(res$Variable, "x")

  # n_missing: 1 NA
  expect_equal(res$n_missing, 1L)
  # complete_rate: 2 non-missing out of 3
  expect_equal(res$complete_rate, 2 / 3)
  # min_length: shortest non-NA is "" → length 0
  expect_equal(res$min_length, 0L)
  # max_length: longest non-NA is "abc" → length 3
  expect_equal(res$max_length, 3L)
  # empty: one empty string
  expect_equal(res$empty, 1L)
  # n_unique: distinct non-NA values are "abc" and "" → 2
  expect_equal(res$n_unique, 2L)
  # whitespace: only whitespace strings counted (empty excluded)
  expect_equal(res$whitespace, 0L)
})

test_that("all-NA column yields NA lengths and zero other counts", {
  df <- tibble(
    a = c(NA, NA, NA)
  )
  res <- epi_stats_chars(df)

  expect_equal(nrow(res), 1L)
  expect_equal(res$Variable, "a")
  expect_equal(res$n_missing, 3L)
  expect_equal(res$complete_rate, 0)
  expect_true(is.na(res$min_length))
  expect_true(is.na(res$max_length))
  expect_equal(res$empty, 0L)
  expect_equal(res$n_unique, 0L)
  expect_equal(res$whitespace, 0L)
})

test_that("counts whitespace-only strings separately from empty", {
  df <- tibble(
    w = c("   ", "\t", "x", "")
  )
  res <- epi_stats_chars(df)

  expect_equal(res$n_missing, 0L)
  expect_equal(res$complete_rate, 1)
  # min_length over non-NA: lengths are 3, 1, 1, 0 → min = 0, max = 3
  expect_equal(res$min_length, 0L)
  expect_equal(res$max_length, 3L)
  # empty: one "" → 1
  expect_equal(res$empty, 1L)
  # whitespace: whitespace-only strings (excludes empty) → 2
  expect_equal(res$whitespace, 2L)
  # n_unique: distinct non-NA values = "   ", "\t", "x", "" → 4
  expect_equal(res$n_unique, 4L)
})

test_that("multiple character columns are summarized independently", {
  df <- tibble(
    name = c("Alice", "Bob", NA),
    city = c("LA", "", "NY")
  )
  res <- epi_stats_chars(df)

  # Two rows: one for 'name', one for 'city'
  expect_setequal(res$Variable, c("name", "city"))

  # Check 'name'
  name_row <- dplyr::filter(res, Variable == "name")
  expect_equal(name_row$n_missing, 1L)
  expect_equal(name_row$complete_rate, 2 / 3)

  # Check 'city'
  city_row <- dplyr::filter(res, Variable == "city")
  expect_equal(city_row$n_missing, 0L)
  expect_equal(city_row$complete_rate, 1)
  expect_equal(city_row$empty, 1L) # one empty string
})
