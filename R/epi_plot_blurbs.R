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
# # TO DO
# # Area chart
# #data("airquality") #dataset used
# require(ggplot2)
#
# airquality %>%
#   group_by(Day) %>%
#   summarise(mean_wind = mean(Wind)) %>%
#   ggplot() +
#   geom_area(aes(x = Day, y = mean_wind)) +
#   labs(title = "Area Chart of Average Wind per Day",
#        subtitle = "using airquality data",
#        y = "Mean Wind")
######################

######################
# # TO DO
# # Scatterplot
# # Plot Ozone and Temperature measurements for only the month of September
# with(subset(airquality,Month==9),plot(Wind,Ozone,col='steelblue',pch=20,cex=1.5))
# title('Wind and Temperature in NYC in September of 1973')
#
# ggplot(screen_data, aes(FIBRINOGEN, age)) +
#   geom_point(shape=1) +    # Use hollow circles
#   geom_smooth(method=lm)   # Add linear regression line, (by default includes 95% confidence region)
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
#   geom_point() +
#   geom_smooth(method = lm) +
#   ylab(y_var_label) +
#   xlab(x_var_label) +
#   labs(colour = var3) +
#   theme_classic()
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
#          method = 'number',
#          type = "lower")
######################
