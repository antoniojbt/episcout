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
# TO DO: plot names, labels and saving
# plot_name <- svg(sprintf('%s_%s_%s.svg', input_name, x_var_name, y_var_name))
# ggsave(plot_name)
# or save_plot() # with cowplot
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
														 y = var_y,
														 out_alpha = 0.7,
														 fill = 'grey80',
														 colour = 'grey20',#'black',
														 ...
														 ) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	box_plot <- ggplot2::ggplot(data = df,
															ggplot2::aes_string(y = var_y)
															) + # aes_string() is soft-deprecated
		ggplot2::geom_boxplot(outlier.alpha = out_alpha,
								 fill = fill,
								 colour = colour,
								 ...) +
		ggplot2::theme_bw() +
		ggplot2::theme(axis.title.x = ggplot2::element_blank(),
									 axis.text.x = ggplot2::element_blank(),
									 axis.ticks.x = ggplot2::element_blank()
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
	box_plot <- ggplot2::ggplot(data = df,
															ggplot2::aes_string(y = var_y, x = var_x, fill = var_x)
															) + # aes_string() is soft-deprecated
		ggplot2::stat_boxplot(geom = 'errorbar', width = 0.5) +
		ggplot2::geom_boxplot(...) +
		ggplot2::geom_jitter(shape = 16,
												 position = ggplot2::position_jitter(0.2), alpha = 0.5) +
		ggplot2::stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4) +
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
	bar_plot <- ggplot2::ggplot(df,
										 ggplot2::aes_string(x = var_x, fill = var_x)
										 ) +
		ggplot2::geom_bar(stat = 'count', colour = 'black', ...) +
		ggplot2::guides(fill = FALSE) +
		ggplot2::labs(y = 'Count')
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
	bar_plot <- ggplot2::ggplot(df,
															ggplot2::aes_string(y = var_y,
																									x = var_x,
																									fill = var_x)
															) +
		ggplot2::geom_bar(stat = 'identity', ...) +
		ggplot2::guides(fill = FALSE)
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
# Assumes small effect sizes with vertical line cuts at 0.8 and 1.2
# @importFrom graphics abline plot

epi_plot_volcano <- function(logFC = NULL,
														 adj.P.Val = NULL,
														 main = NULL,
														 pch = 20,
														 h_abline = 2,
														 v_abline = c(0.8, 1.2),
														 ...
														 ) {
	volcano <- plot(2^(logFC),
									-log10(adj.P.Val),
									pch = pch,
									main = main,
									abline(h = h_abline,
												 v = v_abline
												 ),
									...
									)
	return(volcano)
	}

# Test
# TO DO
# epi_plot_volcano()
######################
