context("episcout stats description utility functions")

######################
library(episcout)
library(testthat)
library(magrittr)
library(dplyr)
library(Hmisc)
library(data.table)
######################

######################
# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd('/Users/antoniob/Documents/github.dir/AntonioJBT/episcout/tests/testthat/')
######################

######################
# Set a test set:
# Test set df:
set.seed(12345)
n <- 20
df <- data.frame(var_id = rep(1:(n / 2), each = 2),
                 var_to_rep = rep(c("Pre", "Post"), n / 2),
                 x = rnorm(n),
                 y = rbinom(n, 1, 0.50),
                 z = rpois(n, 2)
                 )
# df
# epi_clean_count_classes(df)
# Correlation matrices:
df_corr <- df %>%select_if(~ epi_clean_cond_numeric(.))
df_corr <- df_corr[, -1] # exclude var_id
cormat_all <- epi_stats_corr(df_corr, method = 'pearson')
# cormat_all
melted_triangles <- epi_stats_corr_triangle(cormat = cormat_all$cormat)
vars_list <- c('x', 'y', 'z')
var_labels <- c('numeric', 'binomial', 'poisson')
renamed_triangles <- epi_stats_corr_rename(melted_triangles$cormat_melted_triangle_r,
                                           melted_triangles$cormat_melted_triangle_pval,
                                           vars_list = vars_list,
                                           var_labels = var_labels
                                           )
######################

######################
print("Function being tested: epi_stats_corr")

test_that("epi_stats_corr", {
  expect_true(all(c("cormat", "cormat_melted_r", "cormat_melted_pval") %in% names(cormat_all)))
  expect_true(is.matrix(cormat_all$cormat$r))
  expect_true(is.data.frame(cormat_all$cormat_melted_r))
  expect_true(is.data.frame(cormat_all$cormat_melted_pval))
  expect_identical(class(cormat_all), "list")
  })
######################

######################
print("Function being tested: epi_stats_corr_triangle")
test_that("epi_stats_corr_triangle", {
  expect_true(nrow(melted_triangles$cormat_melted_triangle_r) > 0)
  expect_true(nrow(melted_triangles$cormat_melted_triangle_pval) > 0)
  expect_identical(class(melted_triangles), "list")
  })
######################

######################
print("Function being tested: epi_stats_corr_rename")

test_that("epi_stats_corr_rename", {
  expect_true(nrow(renamed_triangles$cormat_melted_triangle_r) > 0)
  expect_true(all(c("Var1", "Var2", "value") %in%
                    names(renamed_triangles$cormat_melted_triangle_r)))
  })
######################

