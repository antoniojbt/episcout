#' @title Send a list of plots to a grid object
#'
#' @description A light wrapper for cowplot::plot_grid().
#' Send a list of plots to a grid for multi-plot figures.
#' Makes assumptions and hard-codes preferences. All options are passed to
#' plot_grid().
#'
#' @param plot_list List of plots to be arranged into the grid.
#' @param align cowplot vertical and/or horizontal alignment. Default is 'hv'.
#' @param axis Align left, right, top or bottom and order. Default is 'lrtb'.
#' @param label_size Default is 12.
#' @param ncol Number of columns in the plot grid.
#' @param nrow Number of rows in the plot grid.
#' @param ... Pass any other parameter from plot_grid()
#'
#' @return a cowplot grid object
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plot_cow_save}},
#' \code{\link[cowplot]{plot_grid}},
#' \code{\link[cowplot]{save_plot}}.
#'
#' @note See example in \code{\link{epi_plot_cow_save}} and ggplot2 wrappers epi_plot_*().
#'
#' @export
#'

epi_plots_to_grid <- function(plot_list = NULL,
                              align = 'hv',
                              axis = 'lrtb',
                              label_size = 12, # for the panel "A", "B", etc. labels only, does not affect axis or other plot text
                              ncol = NULL,
                              nrow = NULL,
                              ...) {
    if (!requireNamespace('cowplot', quietly = TRUE)) {
        stop("Package cowplot needed for this function to work. Please install it.",
             call. = FALSE)
    }

    # Disable labels if there's only one plot
    labels <- if (length(plot_list) > 1) "AUTO" else NULL

    my_plot_grid <- cowplot::plot_grid(plotlist = plot_list,
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
