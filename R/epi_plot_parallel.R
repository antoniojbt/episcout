#' Generate plots in parallel
#'
#' @description `epi_plot_parallel()` creates a list of plots for selected
#' variables using a parallel backend. The default plotting function is
#' [epi_plot_hist()], but any function returning a ggplot object and accepting
#' `df` and a variable name can be supplied.
#'
#' @param df A data frame containing variables to plot.
#' @param vars_to_plot Character vector of variable names to plot. If `NULL`,
#'   variables will be selected based on `var_type`.
#' @param var_type Type of variables to select when `vars_to_plot` is `NULL`.
#'   One of "numeric", "integer" or "factor".
#' @param plot_fun Function used to generate each plot. Defaults to
#'   [epi_plot_hist()].
#' @param num_cores Number of cores to use. Passed to [epi_utils_multicore()].
#'   Defaults to all available minus one.
#' @param future_plan Strategy for parallel execution. See
#'   [future::plan()].
#' @param ... Additional arguments passed to [epi_utils_multicore()].
#'
#' @return A named list of plots with an attribute `workers` reporting the
#'   number of workers used.
#' @export
#'
#' @examples
#' \dontrun{
#' plots <- epi_plot_parallel(mtcars,
#'   vars_to_plot = c("mpg", "disp"),
#'   num_cores = 2
#' )
#' }
#'
#' @seealso [epi_plot_save_parallel()], [epi_utils_multicore()]
#'
#' @importFrom foreach `%dopar%`
#' @importFrom magrittr `%>%`
#'
#' @rdname epi_plot_parallel
epi_plot_parallel <- function(df,
                              vars_to_plot = NULL,
                              var_type = "numeric",
                              plot_fun = epi_plot_hist,
                              num_cores = NULL,
                              future_plan = "multisession",
                              ...) {
  if (is.null(vars_to_plot)) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package dplyr needed for this function to work. Please install it.",
        call. = FALSE
      )
    }
    cond <- switch(var_type,
      numeric = dplyr::select_if(df, is.numeric),
      integer = dplyr::select_if(df, is.integer),
      factor = dplyr::select_if(df, is.factor),
      stop("Unsupported var_type")
    )
    vars_to_plot <- names(cond)
  }
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("Package foreach needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  epi_utils_multicore(num_cores = num_cores, future_plan = future_plan, ...)
  workers <- foreach::getDoParWorkers()
  plot_list <- foreach::foreach(
    v = vars_to_plot,
    .export = "plot_fun"
  ) %dopar% {
    plot_fun(df, v)
  }
  names(plot_list) <- vars_to_plot
  attr(plot_list, "workers") <- workers
  plot_list
}

#' Save plots in parallel
#'
#' @description `epi_plot_save_parallel()` saves plots produced by
#' [epi_plot_parallel()] to disk in parallel.
#'
#' @param plot_list Named list of plots to save.
#' @param file_prefix Prefix used to create file names.
#' @param plot_type File type passed to [cowplot::save_plot()].
#' @param plot_step Number of plots per file.
#' @inheritParams epi_plot_parallel
#'
#' @return A character vector of file names with an attribute `workers`
#'   reporting the number of workers used.
#' @export
#'
#' @examples
#' \dontrun{
#' plots <- epi_plot_parallel(mtcars,
#'   vars_to_plot = c("mpg", "disp"),
#'   num_cores = 2
#' )
#' files <- epi_plot_save_parallel(plots, file_prefix = "plots", plot_type = "png")
#' }
#'
#' @seealso [epi_plot_parallel()], [epi_plots_to_grid()], [epi_plot_cow_save()]
#'
#' @rdname epi_plot_parallel
epi_plot_save_parallel <- function(plot_list,
                                   file_prefix = "",
                                   plot_type = "svg",
                                   plot_step = 6,
                                   num_cores = NULL,
                                   future_plan = "multisession",
                                   ...) {
  if (!length(plot_list)) {
    stop("plot_list must contain at least one plot")
  }
  epi_utils_multicore(num_cores = num_cores, future_plan = future_plan, ...)
  workers <- foreach::getDoParWorkers()
  idx <- seq(1, length(plot_list), by = plot_step)
  files <- foreach::foreach(
    i = idx,
    .export = c("plot_list", "plot_step", "file_prefix", "plot_type")
  ) %dopar% {
    end_i <- min(i + plot_step - 1, length(plot_list))
    page_name <- paste0("plot_", i)
    grid <- epi_plots_to_grid(plot_list[i:end_i])
    file_name <- epi_output_name(file_prefix, sprintf("_%s.%s", page_name, plot_type))
    epi_plot_cow_save(plot_grid = grid, file_name = file_name)
    file_name
  }
  files <- unlist(files)
  attr(files, "workers") <- workers
  files
}
