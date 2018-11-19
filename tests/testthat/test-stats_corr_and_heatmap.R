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
	expect_output(str(names(cormat_all)), '"cormat" "cormat_melted_r" "cormat_melted_pval"')
	expect_output(str(cormat_all$cormat$r), '1 -0.1814 -0.0104 -0.1814')
	expect_output(str(cormat_all$cormat_melted_r), 'alue: num  1 -0.1814 -0.0104 -0.1814 1 ...')
	expect_output(str(cormat_all$cormat_melted_pval), 'value: num  NA 0.444 0.965 0.444 NA ...')
	expect_identical(class(cormat_all), 'list')
  }
  )
######################

######################
print("Function being tested: epi_stats_corr_triangle")
test_that("epi_stats_corr_triangle", {
	expect_output(str(melted_triangles$cormat_melted_triangle_r),
								'Var1 : Factor w/ 3 levels "x","y","z": 1 2 3 2 3 3')
	expect_output(str(melted_triangles$cormat_melted_triangle_r),
								'Var2 : Factor w/ 3 levels "x","y","z": 1 1 1 2 2 3')
	expect_output(str(melted_triangles$cormat_melted_triangle_r),
								'value: num  1 -0.1814 -0.0104 1 0.3693 ...')
	expect_output(str(melted_triangles$cormat_melted_triangle_pval),
								'value: num  0.444 0.965 0.109')
	expect_identical(class(melted_triangles), 'list')
  }
  )
######################

######################
print("Function being tested: epi_stats_corr_rename")

test_that("epi_stats_corr_rename", {
	# names(melted_triangles$cormat_melted_triangle_r)
	# names(melted_triangles$cormat_melted_triangle_pval)
	# unique(melted_triangles$cormat_melted_triangle_pval[, 'Var1'])
	# unique(renamed_triangles$cormat_melted_triangle_pval[, 'Var1'])
	# str(renamed_triangles$cormat_melted_triangle_pval)
	expect_output(str(renamed_triangles),
								'Var1 : Factor w/ 3 levels "numeric","binomial"')
	# expect_output(str(renamed_triangles$cormat_melted_triangle_r),
	# 							'value: num  1 -0.18 1 -0.01 0.37 1')
	expect_output(str(renamed_triangles$cormat_melted_triangle_r),
								'Var2 : Factor w/ 3 levels "numeric","binomial"')
	# expect_output(str(renamed_triangles$cormat_melted_triangle_pval),
	# 							'value: num  0.44 0.97 0.11')
  }
  )
######################

# # TO DO: missing tests for
# epi_plot_heatmap()
# epi_plot_heatmap_triangle()
# # Within epi_plot_themes.R, which should be tested with the plots:
# epi_plot_theme_1()
# epi_plot_theme_2()
# scale_colour_epi_plot_theme_2()
# scale_fill_epi_plot_theme_2()
