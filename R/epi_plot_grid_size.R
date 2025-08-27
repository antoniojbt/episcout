#' Calculate grid layout for multi-plot figures
#'
#' Determine the number of rows and columns required to arrange a list of
#' plots in a grid. The default aims for at most a 2 x 3 layout (six plots)
#' but can handle other layouts by adjusting `max_cols` and `max_rows`.
#'
#' @param plot_list List of plots to arrange.
#' @param max_cols Maximum number of columns. Default is 2.
#' @param max_rows Maximum number of rows. Default is 6.
#'
#' @return A list with elements `ncol_grid` and `nrow_grid` indicating the
#'   grid dimensions.
#'
#' @note It may be simpler to define these values manually unless plotting a
#'   large number of variables.
#'
#' @keywords internal
#'
#' @author Antonio Berlanga-Taylor <https://github.com/AntonioJBT/episcout>
#'
#' @examples
#' ## Calculate grid size for four plots
#' plots <- list(a = 1, b = 2, c = 3, d = 4)
#' epi_plot_grid_size(plots)
#'
#' @noRd
epi_plot_grid_size <- function(plot_list = NULL,
                               max_cols = 2,
                               max_rows = 6) {
  grid_size <- vector(mode = "list", length = 2)
  names(grid_size) <- c("ncol_grid", "nrow_grid")
  if (length(plot_list) == 1) {
    grid_size$ncol_grid <- 1
    grid_size$nrow_grid <- length(plot_list)
  } else {
    grid_size$ncol_grid <- max_cols
    grid_size$nrow_grid <- min(
      floor(length(plot_list) / grid_size$ncol_grid),
      floor(max_rows / max_cols)
    )
  }
  grid_size
}
