# ////////////
# Parallelising plots ----

# Get numeric variables:
num_vars <- list()
for (i in colnames(data_f)) {
    if (epi_clean_cond_numeric(data_f[[i]])) {
        num_vars <- c(num_vars, i)
    }
}
num_vars
# ////////////


# ////////////

# ===
# Boxplot for one ----
epi_plot_box(df = data_f, var_y = 'EDAD')
# ===

# ===
# Boxplots for all in a loop ----
# i <- "EDAD"
num_list <- epi_plot_list(vars_to_plot = num_vars)
for (i in names(num_list)) {
    num_list[[i]] <- epi_plot_box(df = data_f, var_y = i)
}
# ===

# ===
# Save plots in a loop ----
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
# ===
# ////////////

# ////////////
# ===
# Boxplots for all in parallel ----
# TO DO:
# test functions, add code tests, test examples
generate_bar_plot_list <- function(vars_to_plot,
                                   data,
                                   custom_palette,
                                   n_cores = num_cores
                                   ) {
  library(parallel)
  bar_list <- mclapply(vars_to_plot, generate_bar_plot, data = data, custom_palette = custom_palette, mc.cores = n_cores)
  # mclapply() should run with shared memory on *nix
  names(bar_list) <- vars_to_plot
  bar_list
}
# ===

# ===
# Save all in parallel ----
save_bar_plot_grids <- function(plot_list,
                                results_subdir,
                                file_n = "plots_bar",
                                per_file = 4, # per page
                                suffix = "pdf",
                                n_cores = num_cores
                                ) {
  library(cowplot)  # For plot grids
  library(parallel)

  jumps <- seq(1, length(plot_list), per_file)

  save_plots <- function(start_i) {
    end_i <- min(start_i + per_file - 1, length(plot_list))
    plot_grid <- plot_grid(plotlist = plot_list[start_i:end_i], ncol = 2)

    outfile <- sprintf(fmt = '%s/%s_%s.%s', results_subdir, file_n, start_i, suffix)
    ggsave(outfile, plot_grid, device = "pdf", width = 15, height = 15)
  }
  # mclapply() should run with shared memory on *nix
  mclapply(jumps, save_plots, mc.cores = n_cores)
}
# ===
# ////////////


# ////////////
# How to run ----

# ===
# Input data and variables
data_f <- data.frame()  # Your data goes here
results_subdir <- "path/to/results"  # Ensure this directory exists
fact_cols <- c("SEXO", "OTHER_FACTOR")  # Replace with your factor column names
custom_palette <- c("red", "blue")  # Replace with your custom palette

# Detect number of cores
num_cores <- detectCores() - 1
# ===

# ===
# Step 1: Generate bar plots in parallel
bar_list <- generate_bar_plot_list(vars_to_plot = fact_cols,
                                   data = data_f,
                                   custom_palette = custom_palette,
                                   n_cores = num_cores
                                   )
# ===

# ===
# Step 2: Save bar plots to files
save_bar_plot_grids(plot_list = bar_list,
                    results_subdir = results_subdir,
                    per_file = 4 # per page,
                    n_cores = num_cores
                    )
# ===
# ////////////
