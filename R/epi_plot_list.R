#' @title Lists to hold plots
#'
#' @description Create a list to hold plots for later plotting (in a multi-
#' plot figure for example). Allows maximum 8 plots per list holder.
#'
#' @param vars_to_plot string with variable names or possibly labels
#'
#' @return a list object
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plots_to_grid}},
#' \code{\link{epi_plot_cow_save}}.
#'
#' @note See example in \code{\link{epi_plot_cow_save}} and ggplot2 wrappers epi_plot_*().
#'
#' @export
#'

epi_plot_list <- function(vars_to_plot = NULL) {
  plot_list <- vector(mode = "list", length = length(vars_to_plot))
  names(plot_list) <- vars_to_plot
  return(plot_list)
}
