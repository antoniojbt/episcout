#' @title Save a grid of plots
#'
#' @description epi_plot_cow_save() A light wrapper to save plots to disk with
#' cowplot::save_plot()
#'
#' @param file_name Name of the file to save
#' @param plot_grid plot to save, more often expecting a grid.
#' @param base_height Height in inches of the plot. Default is 11.69 (A4 size)
#' @param base_width Width in inches of the plot. Default is 8.27 (A4 size)
#' @param ... Pass any other parameter from cowplot::save_plot()
#'
#' @return None, file saved to disk.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plots_to_grid}},
#' \code{\link[cowplot]{plot_grid}},
#' \code{\link[cowplot]{save_plot}}.
#'
#' @note height and width are for A4 size.
#' See also ggplot2 wrappers epi_plot_*().
#'
#' @examples
#' \dontrun{
#' set.seed(12345)
#' n <- 20
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c("Pre", "Post"), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' df
#' df[, "var_id"] <- as.character(df[, "var_id"])
#' vars_to_plot <- df %>%
#'   select_if(epi_clean_cond_numeric) %>%
#'   names()
#' my_plot_list <- epi_plot_list(vars_to_plot)
#' my_plot_list
#' # Generate plots:
#' for (i in names(my_plot_list)) {
#'   print(i)
#'   my_plot_list[[i]] <- ggplot2::ggplot(df, aes_string(y = i)) +
#'     geom_boxplot()
#' }
#' # Pass to a grid and save to file:
#' # length(my_plot_list)
#' my_plot_grid <- epi_plots_to_grid(my_plot_list[1:length(my_plot_list)])
#' epi_plot_cow_save(file_name = "plots_1.pdf", plot_grid = my_plot_grid)
#' }
#'
#' @export
#'

epi_plot_cow_save <- function(file_name = NULL,
                              plot_grid = NULL,
                              base_height = 11.69, # A4
                              base_width = 8.27, # A4
                              ...) {
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package cowplot needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  # Check if plot_grid is a single ggplot plot, avoids adding "A" to figure:
  if (inherits(plot_grid, "ggplot")) {
    message("Saving a single ggplot object.")
    cowplot::save_plot(
      filename = file_name,
      plot = plot_grid,
      base_height = base_height,
      base_width = base_width,
      ...
    )
  } else {
    message("Saving a multi-plot grid.")
    cowplot::save_plot(
      filename = file_name,
      plot = cowplot::plot_grid(plotlist = plot_grid),
      base_height = base_height,
      base_width = base_width,
      ...
    )
  }
}
