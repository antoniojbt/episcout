context("epi_clean_drop_zero_levels_vector")

library(testthat)
library(episcout)

######################
print("Function being tested: epi_clean_drop_zero_levels_vector")

test_that("drops levels with zero occurrences", {
  # create a factor with levels a, b, c
  f <- factor(c("a", "b", "a", "c", "b", "a", "b", "c"))
  # introduce NAs so that level 'b' and 'c' go to zero count
  f[c(1, 3, 6, 8)] <- NA # leaves only original 'b' slots but we set to NA
  # now table(f) should only have counts for 'b' and 'c' zero?
  cleaned <- epi_clean_drop_zero_levels_vector(f)
  expect_true(is.factor(cleaned))
  # only levels that had >0 non-NA entries should remain:
  expect_equal(levels(cleaned), as.character(c("b", "c")))
  expect_equal(as.character(cleaned), as.character(c(NA, "b", NA, "c", "b", NA, "b", NA)))
})

test_that("retains levels that still have observations", {
  f <- factor(c("x", "y", "z", "x", "y"))
  # drop all 'z'
  f[3] <- NA
  cleaned <- epi_clean_drop_zero_levels_vector(f)
  expect_equal(levels(cleaned), c("x", "y"))
  # check that the data mapping is preserved
  expect_equal(as.character(cleaned), as.character(f))
})

test_that("throws an error if input is not a factor", {
  expect_error(
    epi_clean_drop_zero_levels_vector(letters[1:5]),
    "The input variable is not a factor"
  )
})

test_that("works on factors with no unused levels", {
  f <- factor(c("a", "b", "a", "c"))
  # nothing to drop
  cleaned <- epi_clean_drop_zero_levels_vector(f)
  expect_equal(levels(cleaned), levels(f))
  expect_equal(as.character(cleaned), as.character(f))
})

test_that("preserves order of levels", {
  f <- factor(c("low", "medium", "high", "medium", "low"), levels = c("low", "medium", "high"))
  # assign “high” as NA,     f[3] <- NA changes the value not the level
  f[f == "high"] <- NA
  # and drop the level
  f <- droplevels(f)
  cleaned <- epi_clean_drop_zero_levels_vector(f)
  expect_equal(levels(cleaned), c("low", "medium"))
  # original factor ordering
  expect_equal(unclass(cleaned), unclass(f))
})
######################
