context("episcout stats function tests")

######################
library(episcout)
library(testthat)
library(e1071)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
######################

######################
# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd("")
######################

######################
# Set a test set:

#####
# Test set df:
set.seed(12345)
n <- 1000
df <- data.frame(
  var_id = rep(1:(n / 2), each = 2),
  var_to_rep = rep(c("Pre", "Post"), n / 2),
  x = rnorm(n),
  y = rbinom(n, 1, 0.50),
  z = rpois(n, 2)
)
# df
# length(which(complete.cases(df)))
#####

#####
# Create a dataset with missing NAs:
df2 <- as.data.frame(lapply(df, function(cc) {
  cc[sample(c(TRUE, NA),
    prob = c(0.85, 0.15),
    size = length(cc),
    replace = TRUE
  )]
}))
# The blurb above to introduce NAs is straight from:
# https://stackoverflow.com/questions/27454265/randomly-insert-nas-into-dataframe-proportionaly
#####

#####
# Add character/factor columns for summary, tidy, format functions:
col_chr <- data.frame(
  "chr1" = rep(c("A", "B"), length.out = 1000),
  "chr2" = rep(c("C", "D"), length.out = 1000)
)
# dim(col_chr)
df_cont_chr <- tibble::as_tibble(cbind(df, col_chr))
# epi_head_and_tail(df_cont_chr)
# epi_head_and_tail(df_cont_chr, last_cols = TRUE)

# Check variable types are what you expect:
# epi_clean_count_classes(df_cont_chr)
# str(df_cont_chr)
# dim(df_cont_chr)
# var_id, y and z can be treated as factors or characters.
# summary(as.factor(df_cont_chr$y))
# summary(as.factor(df_cont_chr$z))
# Here we'll only transform y though:
df_cont_chr$y <- as.factor(df_cont_chr$y)
# epi_clean_count_classes(df_cont_chr)
# str(df_cont_chr)

# Designate some values as codes to be counted separately:
codes <- c("Pre", "A", "C", "1", "3")

# Add total for percentage calculation and order column to tidy up results:
perc_n <- nrow(df_cont_chr)
order_by <- "percent"
#####
######################

######################
print("Function being tested: epi_stats_na_perc")

test_that("epi_stats_na_perc", {
  # output is silent if successful
  # matches values, attributes, and type:
  # Get summary of counts and percentages for missing values across columns:
  na_cols <- epi_stats_na_perc(df2, margin = 2)
  # na_cols
  expect_equal(na_cols[1, 1], 129)
  expect_equal(na_cols[5, 2], 14.4)
  # Get summary of counts and percentages for missing values across rows:
  na_rows <- epi_stats_na_perc(df2, margin = 1)
  # epi_head_and_tail(na_rows, cols = 2)
  # summary(na_rows)
  expect_identical(length(which(na_rows$na_counts == 0)), length(which(complete.cases(df2))))
})
######################

######################
print("Function being tested: epi_stats_count_outliers")

test_that("epi_stats_count_outliers", {
  # output is silent if successful
  # matches values, attributes, and type:
  expect_equal(epi_stats_count_outliers(num_vec = df$x, coef = 0), 0)
  q1 <- quantile(df$x, 0.25, type = 7)
  q3 <- quantile(df$x, 0.75, type = 7)
  iqr_val <- q3 - q1
  expected <- sum(df$x < q1 - 1.5 * iqr_val | df$x > q3 + 1.5 * iqr_val)
  expect_identical(epi_stats_count_outliers(num_vec = df$x), expected)
})
######################

######################
print("Function being tested: epi_stats_numeric")

test_that("epi_stats_numeric", {
  # output is silent if successful
  # matches values, attributes, and type:
  desc_stats <- epi_stats_numeric(df$x)
  # desc_stats
  # dim(desc_stats)
  expect_equal(class(desc_stats), "data.frame")
  expect_equal(desc_stats$min, min(df$x), tolerance = 1e-8)
  expect_equal(desc_stats$mean, mean(df$x), tolerance = 1e-8)
  expect_equal(desc_stats[1, "NA_percentage"], 0)
})
######################

######################
print("Functions being tested: epi_stats_summary, ")
print("epi_stats_tidy and")
print("epi_stats_format")

test_that("epi_stats_summary, epi_stats_tidy and epi_stats_format", {
  #####
  # Count when codes are present, pass these as character or factor, specify
  #  action is to count codes only:
  stat_sum1 <- epi_stats_summary(
    df = df_cont_chr,
    codes = codes,
    class_type = "chr_fct",
    action = "codes_only"
  )
  expect_equal(class(stat_sum1)[1], "tbl_df")
  expect_equal(as.character(stat_sum1[1, 1]), "var_to_rep")
  expect_equal(as.character(stat_sum1[4, 3]), "500")
  #####

  #####
  stat_sum_tidy <- epi_stats_tidy(
    sum_df = stat_sum1,
    order_by = order_by,
    perc_n = perc_n
  )
  expect_equal(class(stat_sum_tidy)[3], "data.frame")
  # Format them if needed:
  digit_0 <- epi_stats_format(stat_sum_tidy, digits = 0)
  expect_equal(as.character(digit_0[1, 6]), "500")
  digit_2 <- epi_stats_format(stat_sum_tidy, digits = 2)
  expect_equal(as.character(digit_2[1, 6]), "500.00")
  #####
})

test_that("epi_stats_summary, epi_stats_tidy and epi_stats_format", {
  #####
  # Count integer or numeric codes:
  stat_sum2 <- epi_stats_summary(df_cont_chr,
    codes = codes,
    class_type = "int_num",
    action = "codes_only"
  )
  # stat_sum2
  expect_equal(class(stat_sum2)[1], "tbl_df")
  expect_equal(as.character(stat_sum2[1, 1]), "var_id")
  expect_equal(as.character(stat_sum2[4, 3]), "172")
  # Tidy and format them:
  stat_sum_tidy <- epi_stats_tidy(
    sum_df = stat_sum2,
    order_by = order_by,
    perc_n = perc_n
  )
  # stat_sum_tidy
  expect_equal(class(stat_sum_tidy)[3], "data.frame")
  # Format them if needed:
  digit_0 <- epi_stats_format(stat_sum_tidy, digits = 0)
  expect_equal(as.character(digit_0[1, 5]), "45")
  digit_2 <- epi_stats_format(stat_sum_tidy, digits = 2)
  expect_equal(as.character(digit_2[1, 5]), "45.10")
  #####
})

test_that("epi_stats_summary, epi_stats_tidy and epi_stats_format", {
  #####
  # Get summary stats excluding contingency codes for character and factor columns:
  stat_sum3 <- epi_stats_summary(df_cont_chr,
    codes = codes,
    class_type = "chr_fct",
    action = "exclude"
  )
  # stat_sum3
  expect_equal(class(stat_sum3)[2], "tbl")
  expect_equal(as.character(stat_sum3[1, 2]), "Post")
  expect_equal(as.character(stat_sum3[2, 3]), "503")

  # Tidy and format:
  stat_sum_tidy <- epi_stats_tidy(
    sum_df = stat_sum3,
    order_by = order_by,
    perc_n = perc_n
  )
  # stat_sum_tidy
  expect_equal(class(stat_sum_tidy)[3], "data.frame")
  # Format them if needed:
  digit_0 <- epi_stats_format(stat_sum_tidy, digits = 0)
  expect_equal(as.character(digit_0[1, 5]), " NA")
  digit_2 <- epi_stats_format(stat_sum_tidy, digits = 2)
  expect_equal(as.character(digit_2[1, 2]), "503.00")
  #####
})

test_that("epi_stats_summary, epi_stats_tidy and epi_stats_format", {
  #####
  # Get summary stats for numeric/integer columns
  # while excluding certain codes/values:
  stat_sum4 <- epi_stats_summary(
    df = df_cont_chr,
    codes = codes,
    class_type = "int_num",
    action = "exclude"
  )
  # stat_sum4
  # as.data.frame(stat_sum4)
  # dim(stat_sum4)
  expect_equal(class(stat_sum4)[2], "tbl")
  expect_equal(as.character(stat_sum4[1, 2]), "996")
  expect_equal(as.character(stat_sum4[2, 3]), "1000")
  expect_equal(as.character(stat_sum4[2, 7]), "-2.77832551031467")
  expect_equal(as.character(colnames(stat_sum4)[16]), "variance")
  # Numeric data summary doesn't need tidying but could be formatted:
  digit_2 <- epi_stats_format(stat_sum4, digits = 2)
  expect_equal(as.character(digit_2[1, 2]), " 996.00")
  expect_equal(as.character(digit_2[3, 16]), "    2.71")
  #####
})

test_that("epi_stats_summary, epi_stats_tidy and epi_stats_format", {
  #####
  # If there are no codes to return the result is an empty data.frame (tibble):
  codes <- c("Per", "X", "55")
  stat_sum_zero <- epi_stats_summary(df_cont_chr,
    codes = codes,
    class_type = "chr_fct",
    action = "codes_only"
  )
  # as.data.frame(stat_sum_zero)
  # class(stat_sum_zero)
  expect_equal(class(stat_sum_zero)[2], "tbl")
  # TO DO: add back but failing at the moment on travis, passes locally (04/June/2019)
  # expect_equal(as.character(as.data.frame(stat_sum_zero))[1], "character(0)")
  #####
})
######################
