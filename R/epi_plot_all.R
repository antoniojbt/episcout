
############
# Plots

num_vars <- list()
for (i in colnames(data_f)) {
    if (epi_clean_cond_numeric(data_f[[i]])) {
        num_vars <- c(num_vars, i)
    }
}
num_vars


###
# Numeric, boxplots:
epi_plot_box(df = data_f, var_y = 'EDAD')

# i <- "EDAD"
num_list <- epi_plot_list(vars_to_plot = num_vars)
for (i in names(num_list)) {
    num_list[[i]] <- epi_plot_box(df = data_f, var_y = i)
}

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(num_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_box'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile
    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(num_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###
