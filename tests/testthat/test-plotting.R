######################
library(episcout)
library(testthat)
library(vdiffr)
library(dplyr)
library(reshape2)
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
# Dummy tests for workflow with ggplot2, testthat and vdiffr for image regression testing
# See create_an_r_package_2.R for more info and references
# Workflow:
# Add test cases in eg XXXX/episcout/tests/testthat/test-plotting.R such as:
context("dummy_tests_vdiffr") # this will be the name that the folder wil get as eg
                         # XXXX/episcout/tests/figs/distributions
test_that("histograms draw correctly - vdiffr dummy run", {
  hist_ggplot <- ggplot(mtcars, aes(disp)) + geom_histogram()
  vdiffr::expect_doppelganger("ggplot2 histogram", hist_ggplot)

  hist_base <- function() hist(mtcars$disp)
  vdiffr::expect_doppelganger("Base graphics histogram", hist_base)
})
# Run:
# vdiffr::manage_cases(filter = 'plot')
# within RStudio to get the vdiffr widget and validate images manually
# Run devtools::test() as usual to test
# Update as needed for failed tests
# Consider these as monitoring tools with regression testing as opposed to strict
# unit tests
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
                              z = rpois(n, 2),
	                            w = sample(1:20, 20)
                 )
df$id_unique <- paste0(df[['var_id']], '_', df[['var_to_rep']])
# df
df[, 'var_id'] <- as.character(df[, 'var_id'])
df[, 'y'] <- as.factor(df[, 'y'])
# str(df)
######################

######################
context("episcout_plots")
# All episcout reference plots will/should be saved in
# XXXX/episcout/tests/figs/episcout_plots
print("episcout plot function tests")
print("Function being tested: epi_plot_list")

vars_to_plot <- df %>%
  select_if(epi_clean_cond_numeric) %>%
  names()
my_plot_list <- epi_plot_list(vars_to_plot)
# my_plot_list

test_that("epi_plot_list", {
  expect_output(str(names(my_plot_list)), '"x" "z" "w"')
  }
  )
######################

######################
print("Function being tested: epi_plot_grid_size")
# Generate plots:
for (i in names(my_plot_list)) {
  # print(i)
  # my_plot_list[[i]] <- ggplot2::qplot(data = df, y = , geom = 'boxplot')
  my_plot_list[[i]] <- ggplot2::ggplot(df, aes(y = .data[[i]])) + geom_boxplot()
}
# Not in use but keep tests:
# Calculate how many plots can be passed to one grid (one page):
# grid_size <- epi_plot_grid_size(my_plot_list)
# grid_size
# Not exported so errors with 'could not find function', leave as reference though
test_that("epi_plot_grid_size", {
#   expect_output(str(grid_size), 'ncol_grid: num 2')
#   expect_output(str(grid_size), 'nrow_grid: num 1')
  }
  )
######################

######################
print("Function being tested: epi_plots_to_grid")
# Pass to a grid and save to file:
# length(my_plot_list)
my_plot_grid <- epi_plots_to_grid(my_plot_list[1:length(my_plot_list)])

test_that("epi_plots_to_grid", {
  vdiffr::expect_doppelganger("epi_plots_to_grid", my_plot_grid)
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
# Test for histogram:
print("Function being tested: epi_plot_hist")

test_that("epi_plot_hist", {
  # my_hist_plot <- epi_plot_hist(df, 'x') # pass with quotes as using ggplot2::aes_string()
  # Change the bins:
  my_hist_plot <- epi_plot_hist(df, 'x', breaks = seq(-3, 3, by = 1))
  # Add titles and axis names:
  my_hist_plot <- my_hist_plot +
    labs(title = "Histogram for X") +
    labs(x = "X", y = "Count")
  # Add axis limits:
  my_hist_plot <- my_hist_plot +
    xlim(c(-4, 4)) +
    ylim(c(0, 10))
  # my_hist_plot
  vdiffr::expect_doppelganger("epi_plot_hist_1", my_hist_plot)

  # Histogram with density curve:
  my_hist_plot <- my_hist_plot + geom_density(col = 2)
  # my_hist_plot
  vdiffr::expect_doppelganger("epi_plot_hist_density", my_hist_plot)

  # Histogram overlaid with kernel density curve:
  # http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
  my_hist_plot <- my_hist_plot +
  # Density instead of count on y-axis:
    geom_histogram(aes(y = after_stat(density)),
                   binwidth = 0.5,
                   colour = "black",
                   fill = "white") +
    geom_density(alpha = 0.2, fill = "#FF6666") + # Overlay with transparent density plot
    ylab('Density')
  # my_hist_plot
  vdiffr::expect_doppelganger("epi_plot_hist_kernel", my_hist_plot)
  }
  )
######################

######################
print("Function being tested: epi_plot_box")

test_that("epi_plot_box", {
  # Boxplot of one variable:
  my_boxplot <- epi_plot_box(df, var_y = 'x')
  vdiffr::expect_doppelganger("epi_plot_box_1_var", my_boxplot)

  # Add notch:
  my_boxplot <- epi_plot_box(df, var_y = 'x', notch = TRUE)
  vdiffr::expect_doppelganger("epi_plot_box_1_var_notch", my_boxplot)

  # Boxplot for x and y variables:
  # df$x # continuous variable
  # df$var_to_rep # factor
  my_boxplot <- epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x')
  vdiffr::expect_doppelganger("epi_plot_box_2_var", my_boxplot)

  # Change colours, remove legend, etc.:
  my_boxplot <- epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x')
  my_boxplot +
    # scale_fill_grey() +
    scale_fill_brewer(palette = "Blues") +
    # scale_fill_brewer(palette = "Dark2") +
    theme(legend.position = "none") # Remove legend
  # my_boxplot
  vdiffr::expect_doppelganger("epi_plot_box_2_var_colours", my_boxplot)
  }
  )

######################

######################
# Bar plots of one and two variables:
print("Function being tested: epi_plot_bar")
test_that("epi_plot_bar", {
  # df
  # lapply(df, class)
  # Barplot for single variable:
  # summary(df$var_to_rep)
  plot_bar <- epi_plot_bar(df, 'var_to_rep')
  # plot_bar
  vdiffr::expect_doppelganger("epi_plot_bar_1_var", plot_bar)

  # Barplot for two variables side by side:
  df_bar <- reshape2::melt(df[, c('w', 'z', 'id_unique')], id.vars = 'id_unique')
  # epi_head_and_tail(df, cols = 7)
  # epi_head_and_tail(df_bar, cols = 3)
  # ggplot(df_bar, aes(x = id_unique, y = value, fill = variable)) +
  #       geom_bar(stat = 'identity', position = 'dodge') +
  # 	theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot_bar <- epi_plot_bar(df_bar,
                           var_x = 'id_unique',
                           var_y = 'value',
                           fill = 'variable') +
  	theme(axis.text.x = element_text(angle = 90, hjust = 1))
	# plot_bar
  vdiffr::expect_doppelganger("epi_plot_bar_2_var", plot_bar)
  }
  )
######################

######################
print("Functions being tested: epi_plot_heatmap and epi_plot_heatmap_triangle")

test_that("epi_plot_heatmap", {
	# Set-up data:
	df[, 'y'] <- as.integer(df[, 'y'])
	df_corr <- df %>% select_if(~ epi_clean_cond_numeric(.))
  df_corr <- df_corr[, -1] # exclude var_id
  cormat_all <- epi_stats_corr(df_corr, method = 'pearson')
  melted_triangles <- epi_stats_corr_triangle(cormat = cormat_all$cormat)
  vars_list <- c('x', 'y', 'z')
  var_labels <- c('numeric', 'binomial', 'poisson')
  renamed_triangles <- epi_stats_corr_rename(melted_triangles$cormat_melted_triangle_r,
                                             melted_triangles$cormat_melted_triangle_pval,
                                             vars_list = vars_list,
                                             var_labels = var_labels)
  expect_true(nrow(renamed_triangles$cormat_melted_triangle_r) > 0)

  # Test epi_plot_heatmap:
  # Correlation values:
  heat_r <- epi_plot_heatmap(cormat_all$cormat_melted_r)
  vdiffr::expect_doppelganger("epi_plot_heat_r", heat_r)
  # As a triangle:
  heat_r_triangle <- epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_r)
  vdiffr::expect_doppelganger("epi_plot_heat_r_triangle", heat_r_triangle)
  # P-values as a triangle:
  heat_p_triangle <- epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_pval)
  vdiffr::expect_doppelganger("epi_plot_heat_p_triangle", heat_p_triangle)

  # Test epi_plot_heatmap_triangle:
  # Nicer triangle:
  nicer_triangle <- epi_plot_heatmap_triangle(renamed_triangles$cormat_melted_triangle_r,
                            renamed_triangles$cormat_melted_triangle_pval,
                            show_values = 'pval'#'corr'
                            )
  vdiffr::expect_doppelganger("epi_plot_heat_nicer_triangle", nicer_triangle)
  }
)
######################

######################
print("Function being tested: epi_plot_volcano")

test_that("epi_plot_volcano", {
  # TO DO: test plot output, needs example code
  # Deprecated, just use limma::volcanoplot()
}
)
######################

# TO DO: missing tests for, moved this to blurbs, update properly and move back to R/
# epi_plot_en_masse

# TO DO: Within epi_plot_themes.R, which should be tested with the plots:
# epi_plot_theme_1()
# epi_plot_theme_2()
# scale_colour_epi_plot_theme_2()
# scale_fill_epi_plot_theme_2()
