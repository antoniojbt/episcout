#' @title Batch processing for plots into figures for many columns
#'
#' @description epi_plot_en_masse() takes a data.frame and column type,
#' creates multi-plot figures for all and saves to disk.
#' Useful when plotting tens to hundreds of plots.
#' By default runs in parallel.
#'
#' @param df Dataframe with variables to plot.
#'
#' @param file_prefix Prefix to use for filenames. Default is ''.
#'
#' @param plot_type Plot type to generate, this is passed to cowplot::save_plot.
#' Default is 'svg'.
#'
# @param vars_list List of variables names to plot.
#
#' @param var_type Type of variables to plot, numeric, integer or factor.
#' Default is 'numeric'.
#'
#' @param plot_step Number of plots per page (file). Default is 6.
#'
#' @param ... Arguments to pass to epi_utils_multicore().
#'
#' @return Nothing, saves plots to disk.
#'
#' @note epi_utils_multicore() will run with defaults (max cores - 1 and multiprocess).
#' You may need to experiment first to see how many plots fit per page.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plots_to_grid}},
#' \code{\link{epi_plot_cow_save}},
#' \code{\link{epi_utils_multicore}}.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Adjust labels and generate plots:
#' lab <- stringr::str_replace_all(string = i, pattern = '_', replacement = ' ')
#' lab <- stringr::str_to_upper(lab)
#' }
#
# @export
#'
# @importFrom foreach `%dopar%`
# @importFrom magrittr `%>%`

# TO DO: test and complete
# decide how to pass plotting function

epi_plot_en_masse <- function(df = NULL,
                              file_prefix = '',
                              plot_type = 'svg',
                              var_type = 'numeric',
                              plot_step = 6,
                              ...
                              ) {
  if (!requireNamespace('dplyr', quietly = TRUE)) {
    stop("Package dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('foreach', quietly = TRUE)) {
    stop("Package foreach needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('future', quietly = TRUE)) {
    stop("Package future needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Set-up types of variable to plot:
  if (var_type == 'numeric') {
    var_type <- expression(is.numeric(.))
  } else if (var_type == 'integer') {
    var_type <- expression(is.integer(.))
  } else if (var_type == 'factor') {
    var_type <- expression(is.factor(.))
  }
  print('Collecting variables to plot...')
  # var_type
  vars_list <- df %>% dplyr::select_if(~ eval(var_type)) %>% names()
  print('First few variables:')
  print(head(vars_list))
  # Create list:
  plot_list <- epi_plot_list(vars_list)
  # length(hist_list)

  # Set-up type of plot to use:
  #

  # Set-up multi-core:
  epi_utils_multicore(...)

  # TO DO: pass user function
  print('Plotting...')
  plot_list <- foreach::foreach(i = vars_list, .verbose = TRUE) %dopar% {
    # for (i in vars_list) {
    # TO DO: insert user call for arbitrary plot:
    plot_list[[i]] <- epi_plot_hist(df, i) #+ ggplot2::xlab(label = lab)
  }
  print('Done plotting.')
  # Check one:
  names(plot_list)
  plot_list[1]

  # Save to file in multiple pages:
  plot_step <- plot_step

  # Set-up multi-core:
  epi_utils_multicore(...)

  print('Saving plots...')
  save_plots <- foreach::foreach(i = seq(1, length(vars_list),
                                         by = plot_step),
                                 .verbose = TRUE
                                 ) %dopar% {
    # for (i in seq(1, length(vars_list), by = plot_step)) {
    page_start <- i
    page_stop <- i + plot_step - 1
    page_name <- paste0('plot_', i)
    plot_grid <- epi_plots_to_grid(plot_list = plot_list[page_start:page_stop])
    file_name <- epi_output_name(file_prefix,
                                 sprintf('_%s.%s', page_name, plot_type)
    )
    epi_plot_cow_save(plot_grid = plot_grid,
                      file_name = file_name)
  }
  # End:
  print('Done plotting and saving.')
  }
