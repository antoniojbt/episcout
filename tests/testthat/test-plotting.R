context("episcout plot function tests")

######################
library(episcout)
library(testthat)
library(dplyr)
library(magrittr)
library(ggplot2)
library(cowplot)
# library()
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
df[, 'var_id'] <- as.character(df[, 'var_id'])
vars_to_plot <- df %>%
	select_if(epi_clean_cond_numeric) %>%
	names()
my_plot_list <- epi_plot_list(vars_to_plot)
# my_plot_list
# Generate plots:
for (i in names(my_plot_list)) {
	# print(i)
	# my_plot_list[[i]] <- ggplot2::qplot(data = df, y = , geom = 'boxplot')
	my_plot_list[[i]] <- ggplot2::ggplot(df, aes_string(y = i)) + geom_boxplot()
}
# Not in use but keep tests:
# Calculate how many plots can be passed to one grid (one page):
grid_size <- epi_plot_grid_size(my_plot_list)
# grid_size
# Pass to a grid and save to file:
# length(my_plot_list)
my_plot_grid <- epi_plots_to_grid(my_plot_list[1:length(my_plot_list)])
# epi_plot_cow_save(file_name = 'plots_1.pdf', plot_grid = my_plot_grid)
######################

######################
print("Function being tested: epi_plot_list")

test_that("epi_plot_list", {
	expect_output(str(names(my_plot_list)), '"x" "y" "z"')
  }
  )
######################

######################
print("Function being tested: epi_plot_grid_size")

test_that("epi_plot_grid_size", {
	expect_output(str(grid_size), 'ncol_grid: num 2')
	expect_output(str(grid_size), 'nrow_grid: num 1')
  }
  )
######################

######################
print("Function being tested: epi_plots_to_grid")

test_that("epi_plots_to_grid", {
# TO DO:
# my_plot_grid # output is a plot to screen
  }
  )
######################


######################
print("Function being tested: epi_plot_cow_save")

test_that("epi_plot_cow_save", {
# TO DO: output is a plot saved to disk
# epi_plot_cow_save(file_name = 'plots_1.pdf', plot_grid = my_plot_grid)
  }
  )
######################

