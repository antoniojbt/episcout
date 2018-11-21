#' @title Calculate number of plots to pass to grid for plotting
#'
#' @description Get a size for the grid plot for multi-plot figures.
#' Preferably to have a grid size at most 2 cols by 3 rows for 6 plots total.
#' Can pass more with max_cols and max_rows though max_rows should be 6 when
#' max_cols is 1
#'
#' @param plot_list String of plot names.
#' @param max_cols Maximum number of columns in grid (page) Default is 2.
#' @param max_rows Maximum number of rows in grid. Default is 6.
#'
#' @return a list object containing nrow and ncols to use per grid
#'
#' @note It is probably easier to define these variables manually unless you are
#' plotting many variables.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plots_to_grid}},
#' \code{\link{epi_plot_cow_save}}.
#'
#' @note See example in \code{\link{epi_plot_cow_save}} and ggplot2 wrappers epi_plot_*().
#'
# @export keep internal for now as not really practical
#'
#' @keywords internal
#'

epi_plot_grid_size <- function(plot_list = NULL,
															 max_cols = 2,
															 max_rows = 6
															 ) {
	grid_size <- vector(mode = 'list', length = 2)
	names(grid_size) <- c('ncol_grid', 'nrow_grid')
	# single plot:
	if (length(plot_list) == 1) {
		grid_size$ncol_grid <- 1
		grid_size$nrow_grid <- length(plot_list) # which should be 1
	} else {# multi-plots but max per page
		# prefer cols over rows, fix as two and then vary rows:
		grid_size$ncol_grid <- max_cols
		grid_size$nrow_grid <- min(floor(length(plot_list) / grid_size$ncol_grid),
															 floor(max_rows / max_cols))
		# # Give a warning if many plots in list:
		# if (length(plot_list) > max_rows) {
		# 	# print('Could not determine sizes')
		# 	print(sprintf('You may have more than %s plots in your list.', max_rows))
		# 	print(sprintf('Probably best to create one grid page per %s plots.', max_rows))
		# 	print(sprintf('%s is hard coded into this function.', max_rows))
		# 	print(sprintf('Only returning %s x %s.', grid_size$ncol_grid,
		# 								                           grid_size$nrow_grid
		# 								)
		# )
		# }
	}
	return(grid_size)
}
