context("episcout plot function tests")

######################
library(episcout)
library(testthat)
library(dplyr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(ggthemes)
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
# # Test for histogram:
# my_hist_plot <- epi_plot_hist(df, 'x') # pass with quotes as using ggplot2::aes_string()
# my_hist_plot
# # Change the bins:
# my_hist_plot <- epi_plot_hist(df, 'x', breaks = seq(-3, 3, by = 1))
# my_hist_plot
# # Add titles and axis names:
# my_hist_plot <- my_hist_plot +
# 	labs(title = "Histogram for X") +
# 	labs(x = "X", y = "Count")
# my_hist_plot
# # Add axis limits:
# my_hist_plot <- my_hist_plot +
# 	xlim(c(-4, 4)) +
# 	ylim(c(0, 10))
# my_hist_plot
# # Histogram with density curve:
# my_hist_plot <- my_hist_plot + geom_density(col = 2)
# my_hist_plot
# # Histogram overlaid with kernel density curve:
# # http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# my_hist_plot <- my_hist_plot +
# # Density instead of count on y-axis:
# 	geom_histogram(aes( y = ..density..),
# 							   binwidth = 0.5,
# 							   colour = "black",
# 								 fill = "white") +
# 	geom_density(alpha = 0.2, fill = "#FF6666") + # Overlay with transparent density plot
# 	ylab('Density')
# my_hist_plot
######################

######################
# # Boxplot of one variable:
# epi_plot_box(df, var_y = 'x')
# # Add notch:
# epi_plot_box(df, var_y = 'x', notch = TRUE)

# # Boxplot for x and y variables:
# df$x # continuous variable
# df$var_to_rep # factor
# epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x')
# # Change colours, remove legend, etc.:
# my_boxplot <- epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x')
# my_boxplot +
# 	# scale_fill_grey() +
# scale_fill_brewer(palette = "Blues") +
# 	# scale_fill_brewer(palette = "Dark2") +
# 	theme(legend.position = "none") # Remove legend
# # dev.off()
######################

######################
# # Bar plots of one and two variables:
# df
# lapply(df, class)
# # Barplot for single variable:
# summary(df$var_to_rep)
# epi_plot_bar(df, 'var_to_rep')
# # Barplot for two variables:
# epi_head_and_tail(df[, c('var_to_rep', 'y')], cols = 2)
# as.tibble(df) %>% count(y)
# as.tibble(df) %>% count(var_to_rep)
# as.tibble(df) %>% group_by(var_to_rep) %>% count(y)
# epi_plot_bar(df,
# 						 var_y = 'y',
# 						 var_x = 'z',
# 						 x_lab = '',
# 						 fill = 'var_to_rep'
# 						 )
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
# TO DO: see code above
# my_plot_grid # output is a plot to screen
  }
  )
######################


######################
print("Function being tested: epi_plot_cow_save")

test_that("epi_plot_cow_save", {
# TO DO: output is a plot saved to disk, see code above
# epi_plot_cow_save(file_name = 'plots_1.pdf', plot_grid = my_plot_grid)
  }
  )
######################

######################
print("Function being tested: epi_plot_hist")

test_that("epi_plot_hist", {
  # TO DO: test plot output
  #
  }
  )
######################

######################
print("Function being tested: epi_plot_box")

test_that("epi_plot_box", {
	# TO DO: test plot output, see code above
	#
}
)
######################

######################
print("Function being tested: epi_plot_bar")

test_that("epi_plot_bar", {
	# TO DO: test plot output, see code above
	#
}
)
######################

######################
print("Function being tested: epi_plot_volcano")

test_that("epi_plot_volcano", {
	# TO DO: test plot output, needs example code
	#
}
)
######################

######################
print("Function being tested: epi_plot_XXX")

test_that("epi_plot_XXX", {
	# TO DO: test plot output
	#
}
)
######################
