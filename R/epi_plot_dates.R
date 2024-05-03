#############
# library(ggplot2)
#
# test_dates
# str(test_dates)
#
# # Histograms: frequency of dates or intervals:
# ggplot(data.frame(date = test_dates), aes(x = date)) +
#     geom_histogram(binwidth = 30)  # binwidth depends on the data density
#
# # Box Plots: visualise range, median, quartiles, and outliers in date distributions:
# ggplot(data.frame(date = test_dates), aes(x = factor(1), y = date)) +
#     geom_boxplot()
#
# # Time Series Plot: Visualise distribution over time:
# plot_data <- data.frame(date = test_dates, value = seq_along(test_dates))
# head(plot_data)
# str(plot_data)
# ggplot(plot_data, aes(x = date, y = value)) + geom_line() + geom_point()
#############


#############

####
# Time Series Plot: Visualise distribution over time:

# ###
# # Needs dates ordered, increasing:
# i <- 'FECHA_INGRESO'
# ord_i <- sort(as.Date(data_f[[i]]))
# # ord_i <- data_f[ord_i, i]
# # ord_i <- as.vector(ord_i)
# str(ord_i)
# head(ord_i)
# tail(ord_i)
# length(ord_i)
#
# # Get a subset to plot:
# rand_indices <- sort(sample(length(ord_i), size = 1000))
# # rand_indices <- rand_indices[order(rand_indices)]
# head(rand_indices)
# tail(rand_indices)
#
# # Subset the data frame:
# date <- ord_i[rand_indices]
# value <- seq_along(date)
# plot_data <- data.frame(i = date, value = value)
# colnames(plot_data)[1] <- i
# str(plot_data)
# epi_head_and_tail(plot_data, cols = 2)
#
# # Plot:
# ggplot(plot_data, aes(x = !!sym(i), y = value)) +
#     geom_line() +
#     geom_point()
# ###
#
# ###
# time_list <- epi_plot_list(vars_to_plot = col_dates)
# for (i in names(time_list)) {
#     # Get date, sort it, pass it as vector, add value var for plotting:
#     date <- sort(as.Date(data_f[[i]]))
#     value <- seq_along(date)
#     plot_data <- data.frame(i = date, value = value)
#     colnames(plot_data)[1] <- i
#     # Plot:
#     time_list[[i]] <- ggplot(plot_data, aes(x = !!sym(i), y = value)) +
#         geom_line() +
#         geom_point()
# }
# # time_list
#
# # Save plots
# # Plot 4 per page or so:
# per_file <- 4
# jumps <- seq(1, length(time_list), per_file)
# length(jumps)
#
# # i <- 2
# for (i in jumps) {
#     file_name <- sprintf('plots_time_%s.pdf', i)
#     start_i <- i
#     end_i <- i + 3
#     my_plot_grid <- epi_plots_to_grid(time_list[start_i:end_i])
#     epi_plot_cow_save(file_name = file_name, plot_grid = my_plot_grid)
# }
# ####
# ############

#############
