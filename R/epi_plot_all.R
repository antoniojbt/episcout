#' @title Parallel bar plot utilities
#'
#' @description
#' Helper functions to generate and save bar plots in parallel.

#' @name epi_plot_bar_parallel
NULL

#' @title Generate bar plots in parallel
#'
#' @description
#' Create a named list of bar plots for the specified factor variables using
#' [epi_plot_bar()] in parallel.
#'
#' @param vars_to_plot Character vector of factor variables to plot.
#' @param data A data frame containing `vars_to_plot`.
#' @param custom_palette Optional vector of colours passed to
#'   [epi_plot_bar()].
#' @param n_cores Number of cores for parallel processing. Defaults to
#'   `parallel::detectCores() - 1`.
#'
#' @return A named list of ggplot objects.
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   sex = factor(sample(c("F", "M"), 20, replace = TRUE)),
#'   group = factor(sample(letters[1:3], 20, replace = TRUE))
#' )
#' plot_list <- epi_plot_bar_list_parallel(
#'   vars_to_plot = c("sex", "group"),
#'   data = df,
#'   custom_palette = c("#1b9e77", "#d95f02"),
#'   n_cores = 1
#' )
#' plot_list[[1]]
#' }
#' @export
epi_plot_bar_list_parallel <- function(vars_to_plot,
                                       data,
                                       custom_palette = NULL,
                                       n_cores = parallel::detectCores() - 1) {
  n_cores <- max(1, n_cores)

  generate_bar_plot <- function(var) {
    epi_plot_bar( # nolint: object_usage_linter
      df = data,
      var_x = var,
      custom_palette = custom_palette
    )
  }

  plot_list <- parallel::mclapply(
    vars_to_plot,
    generate_bar_plot,
    mc.cores = n_cores
  )
  names(plot_list) <- vars_to_plot
  plot_list
}

#' @title Save bar plot grids in parallel
#'
#' @description
#' Split a list of plots into groups and save each group as a grid to disk in
#' parallel.
#'
#' @param plot_list List of ggplot objects.
#' @param results_subdir Directory where files will be written.
#' @param file_n File name prefix. Defaults to "plots_bar".
#' @param per_file Number of plots per saved grid. Defaults to 4.
#' @param suffix File extension, e.g. "pdf".
#' @param n_cores Number of cores for parallel processing. Defaults to
#'   `parallel::detectCores() - 1`.
#'
#' @return A vector of output file paths.
#' @examples
#' \dontrun{
#' df <- data.frame(sex = factor(c("F", "M"), levels = c("F", "M")))
#' plots <- epi_plot_bar_list_parallel("sex", df, n_cores = 1)
#' dir <- tempdir()
#' epi_plot_bar_grid_save(
#'   plot_list = plots,
#'   results_subdir = dir,
#'   file_n = "my_plots",
#'   per_file = 1,
#'   suffix = "png",
#'   n_cores = 1
#' )
#' list.files(dir, pattern = "my_plots")
#' }
#' @export
epi_plot_bar_grid_save <- function(plot_list,
                                   results_subdir,
                                   file_n = "plots_bar",
                                   per_file = 4,
                                   suffix = "pdf",
                                   n_cores = parallel::detectCores() - 1) {
  if (!dir.exists(results_subdir)) {
    stop("results_subdir must exist", call. = FALSE)
  }
  n_cores <- max(1, n_cores)
  jumps <- seq(1, length(plot_list), by = per_file)

  save_plots <- function(start_i) {
    end_i <- min(start_i + per_file - 1, length(plot_list))
    my_plot_grid <- epi_plots_to_grid(plot_list[start_i:end_i], ncol = 2) # nolint: object_usage_linter
    outfile <- sprintf("%s/%s_%s.%s", results_subdir, file_n, start_i, suffix)
    epi_plot_cow_save(outfile, my_plot_grid) # nolint: object_usage_linter
    outfile
  }

  unlist(parallel::mclapply(jumps, save_plots, mc.cores = n_cores))
}
