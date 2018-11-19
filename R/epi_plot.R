######################
# Helper functions for plotting
# Antonio Berlanga-Taylor
######################

######################
# Examples of visualisations
# https://www.kdnuggets.com/2018/06/7-simple-data-visualizations-should-know-r.html
######################

######################
# TO DO: get BESTD plots
######################

######################
# TO DO: move this to the same package
# Source functions from custom R scripts:
# stats_utils <- '~/Documents/github.dir/EpiCompBio/stats_utils/stats_utils/'
#source(file.path(stats_utils, 'ggtheme.R'))
######################

######################
# Test set df:
# n <- 20
# df <- data.frame(
# 	var_id = rep(1:(n / 2), each = 2),
#   var_to_rep = rep(c('Pre', 'Post'), n / 2),
# 	x = rnorm(n),
# 	y = rbinom(n, 1, 0.50),
# 	z = rpois(n, 2)
# 	)
# df
######################

######################
# TO DO: plot names, labels and saving
# plot_name <- svg(sprintf('%s_%s_%s.svg', input_name, x_var_name, y_var_name))
# ggsave(plot_name)
# or save_plot() # with cowplot
######################

######################
# Create lists to hold plots, have max 8 per list holder:
# Assumes vars_to_plot is a string with variable names or possibly labels
epi_plot_list <- function(vars_to_plot = NULL) {
	plot_list <- vector(mode = 'list', length = length(vars_to_plot))
	names(plot_list) <- vars_to_plot
	return(plot_list)
	}
# Test:
# vars_to_plot <- df %>%
# 	select_if(is.numeric) %>%
# 	names()
# my_plot_list <- epi_plot_list(vars_to_plot)
# my_plot_list
######################

######################
# Get a size for the grid plot for multi-plot figures
# Preferably to have a grid size at most 2 cols by 3 rows for 6 plots total
# Can pass more with max_cols and max_rows though
# max_rows should be
epi_plot_grid_size <- function(plot_list = NULL,
															 max_cols = 2,
															 max_rows = 6
															 ) {
	grid_size <- vector(mode = 'list', length = 2)
	names(grid_size) <- c('ncol_grid', 'nrow_grid')
	# single plot:
	if (length(plot_list) == 1) {
		grid_size$ncol_grid <- 1
		grid_size$nrow_grid <- length(plot_list) # which should be 1
	} else {# multi-plots but max per page
		# prefer cols over rows, fix as two and then vary rows:
		grid_size$ncol_grid <- max_cols
		grid_size$nrow_grid <- min(floor(length(plot_list) / grid_size$ncol_grid),
																floor(max_rows / max_cols))
		# # Give a warning if many plots in list:
		# if (length(plot_list) > max_rows) {
		# 	# print('Could not determine sizes')
		# 	print(sprintf('You may have more than %s plots in your list.', max_rows))
		# 	print(sprintf('Probably best to create one grid page per %s plots.', max_rows))
		# 	print(sprintf('%s is hard coded into this function.', max_rows))
		# 	print(sprintf('Only returning %s x %s.', grid_size$ncol_grid,
		# 								                           grid_size$nrow_grid
		# 								)
			# )
		# }
	}
	return(grid_size)
}
# # Test:
# hist(df$z)
# # Create several plots:
# for (i in vars_to_plot) {
# 	print(i)
# 	my_plot_list[[i]] <- ggplot(df, aes_string(i)) + geom_histogram()
# 	}
# names(my_plot_list)
# length(my_plot_list)
# epi_plot_grid_size(my_plot_list)
######################

######################
# Send a list of plots to a grid using cowplot
# Makes assumptions and hard-codes preferences
# Use ... to pass further options not listed.
epi_plots_to_grid <- function(plot_list = NULL,
															align = 'hv',
															axis = 'lrtb',
															labels = 'AUTO',
															label_size = 12,
															ncol = NULL,
															nrow = NULL,
															...
															) {
	if (!requireNamespace('cowplot', quietly = TRUE)) {
		stop("Package cowplot needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	# plot_sizes <- grid_size(plot_list)
	# print(plot_sizes)
	my_plot_grid <- plot_grid(plotlist = plot_list,
														# nrow = plot_sizes$nrow_grid,
														# ncol = plot_sizes$ncol_grid,
														align = align,
														axis = axis,
														labels = labels,
														label_size = label_size,
														ncol = ncol,
														nrow = nrow,
														...
														)
	return(my_plot_grid)
	}
# Test:
# multi_plot_figure <- epi_plots_to_grid(my_plot_list)
# dev.off()
######################

######################
# Save plots to disk with cowplot::save_plot()
# base height and width are for A4 size
# ... to pass further parameters to cowplot::
epi_plot_cow_save <- function(plot_grid = NULL,
															input_name,
															base_height = 11.69, # A4
															base_width = 8.27, # A4
															...
															) {
	if (!requireNamespace('cowplot', quietly = TRUE)) {
		stop("Package cowplot needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	save_plot(filename = input_name,
						plot = plot_grid,
						# the ratio can be very skewed with many plots
						# axis labels appear out of proportion and not scaled
						# base_aspect_ratio = 3,
						base_height = base_height,
						base_width = base_width,
						...
	)
	}
# Test:
# epi_plot_cow_save(multi_plot_figure, 'epi_plot_save_test.svg')
######################

######################
# Histogram wrapper function using ggplot2
# ... passes arguments to geom_ such as breaks, colour, fill, alpha, etc.
# For other options, save as object and build on the layers, see examples below
epi_plot_hist <- function(df = NULL,
													var_x = NULL,
													...
													) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	# var_x <- enquo(var_x) # enquosure required for non-standard R object evaluation
	hist_plot <- ggplot(data = df, aes_string(var_x)) + # aes_string() is soft-deprecated
		geom_histogram(...) +
		epi_plot_theme_2() + # Needs ggtheme.R functions in this package
	  labs(y = 'Count')
	return(hist_plot)
	}
# # Test:
# df$x # continuous variable
# my_hist_plot <- epi_plot_hist(df, 'x') # no quotes for ggplot2 if only using aes()
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
# my_hist_plot <- my_hist_plot +
# 	geom_density(col = 2)
# my_hist_plot
# # Histogram overlaid with kernel density curve:
# # http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# my_hist_plot <- my_hist_plot +
# 	geom_histogram(aes( y = ..density..), # Histogram with density instead of count on y-axis
# 							   binwidth = 0.5,
# 							   colour = "black",
# 								 fill = "white") +
# 	geom_density(alpha = 0.2, fill = "#FF6666") + # Overlay with transparent density plot
# 	ylab('Density')
# my_hist_plot
######################

######################
# TO DO
# qqnorm
# qqplot
######################

######################
# Boxplot for one variable wrapper function using ggplot2
# ... passes arguments to geom_ such as breaks, colour, fill, alpha, etc.
# For other options, save as object and build on the layers, see examples for epi_plot_hist
# ggplot2 boxplot for single var are a bit fiddly and ugly, use base with eg:
# box_plot <- boxplot(df[[var_y]])
epi_plot_box_one <- function(df = NULL,
														 var_y,
														 out_alpha = 0.7,
														 fill = 'grey80',
														 colour = 'grey20',#'black',
														 ...
														 ) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	box_plot <- ggplot(data = df, aes_string(y = var_y)) + # aes_string() is soft-deprecated
		geom_boxplot(outlier.alpha = out_alpha,
								 fill = fill,
								 colour = colour,
								 ...) +
		theme_bw() +
		theme(axis.title.x = element_blank(),
					axis.text.x = element_blank(),
					axis.ticks.x = element_blank()
					)
	return(box_plot)
	}
# # Test:
# df$x # continuous variable
# epi_plot_box_one(df, var_y = 'x')
# # Add notch:
# epi_plot_box_one(df, var_y = 'x', notch = TRUE)
# # dev.off()
######################

######################
# Boxplot for more than one variable wrapper function using ggplot2
# ... passes arguments to geom_ such as breaks, colour, fill, alpha, etc.
# For other options, save as object and build on the layers, see examples for epi_plot_hist
epi_plot_box <- function(df = NULL,
												 var_y = NULL,
												 var_x = NULL,
												 ...
												 ) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	box_plot <- ggplot(data = df, aes_string(y = var_y, x = var_x, fill = var_x)) + # aes_string() is soft-deprecated
		stat_boxplot(geom = 'errorbar', width = 0.5) +
		geom_boxplot(...) +
		geom_jitter(shape = 16, position = position_jitter(0.2), alpha = 0.5) +
		stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4) +
		epi_plot_theme_2()
	return(box_plot)
	}
# # Test:
# df$x # continuous variable
# df$var_to_rep # factor
# epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x')
# # Change colours, remove legend, etc.:
# my_boxplot <- epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x')
# my_boxplot +
# 	# scale_fill_grey() +
# 	scale_fill_brewer(palette = "Blues") +
# 	# scale_fill_brewer(palette = "Dark2") +
# 	theme(legend.position = "none") # Remove legend
# # Add notch:
# # my_boxplot <- epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x', notch = TRUE)
# # dev.off()
######################

######################
# Barplot for single variable
# Uses stat = 'count', coloured according to the x variable passed,
# black borders for bars and no legend by default. These are hard-coded.
# ... to pass additional parameters to geom_bar
epi_plot_bar_one <- function(df = NULL,
														 var_x = NULL,
														 ...
														 ) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	bar_plot <- ggplot(df, aes_string(x = var_x, fill = var_x)) +
		geom_bar(stat = 'count', colour = 'black', ...) +
		guides(fill = FALSE) +
		labs(y = 'Count')
	return(bar_plot)
	}
# # Test:
# lapply(df, class)
# epi_plot_bar_one(df, 'var_to_rep')
######################

######################
# Barplot by groups
# Assumes you want to colour according to the x variable passed
# and that stat = 'identity' is what's needed. No legend by default.
# These are hardcoded.
# stat = 'identity' uses the height of the bar to represent
# the value of the passed column.
# ... to pass additional parameters to geom_
epi_plot_bar <- function(df = NULL,
												 var_y = NULL,
												 var_x = NULL,
												 ...
												 ) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	bar_plot <- ggplot(df, aes_string(y = var_y, x = var_x, fill = var_x)) +
		geom_bar(stat = 'identity', ...) +
		guides(fill = FALSE)
	return(bar_plot)
	}
# Test:
# epi_plot_bar(df, var_y = 'y', var_x = 'var_to_rep')
######################

######################
# # TO DO
# # Area chart
# #data("airquality") #dataset used
# require(ggplot2)
#
# airquality %>%
# 	group_by(Day) %>%
# 	summarise(mean_wind = mean(Wind)) %>%
# 	ggplot() +
# 	geom_area(aes(x = Day, y = mean_wind)) +
# 	labs(title = "Area Chart of Average Wind per Day",
# 			 subtitle = "using airquality data",
# 			 y = "Mean Wind")
######################

######################
# # TO DO
# # Scatterplot
# # Plot Ozone and Temperature measurements for only the month of September
# with(subset(airquality,Month==9),plot(Wind,Ozone,col='steelblue',pch=20,cex=1.5))
# title('Wind and Temperature in NYC in September of 1973')
#
# ggplot(screen_data, aes(FIBRINOGEN, age)) +
# 	geom_point(shape=1) +    # Use hollow circles
# 	geom_smooth(method=lm)   # Add linear regression line, (by default includes 95% confidence region)
# ggsave('scatter_FIBRINOGEN_age.pdf')
#
# # Scatterplot and legend:
# # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
# # Setup:
# x_var_label <- x_var
# y_var_label <- y_var
# plot_name <- sprintf('%s_%s_%s_scatterplot.svg', input_name, x_var, y_var)
# # Plot:
# ggplot(input_data, aes(x = input_data[, x_var], y = input_data[, y_var], colour = var3_factor)) +
# 	geom_point() +
# 	geom_smooth(method = lm) +
# 	ylab(y_var_label) +
# 	xlab(x_var_label) +
# 	labs(colour = var3) +
# 	theme_classic()
# # Save:
# ggsave(plot_name)
# # Prevent Rplots.pdf from being generated. ggsave() without weight/height opens a device.
# # Rscript also saves Rplots.pdf by default, these are deleted at the end of this script.
# dev.off()
######################

######################
# # TO DO
# # Correlogram
# require(corrplot)
# data("mtcars")
# corr_matrix <- cor(mtcars)
#
# # with circles
# corrplot(corr_matrix)
#
# # with numbers and lower
# corrplot(corr_matrix,
# 				 method = 'number',
# 				 type = "lower")
######################

######################
# Volcano plot, designed to take limma's output as input
epi_plot_volcano <- function(logFC = NULL,
														 adj.P.Val = NULL,
														 main = NULL,
														 pch = 20,
														 ...
														 ) {
	volcano <- plot(2^(logFC),
									-log10(adj.P.Val),
									pch = pch,
									main = main,
									abline(h = 2, v = c(0.8, 1.2)),
									...
	)
	return(volcano)
}
# Test
# TO DO
# epi_plot_volcano()
######################
