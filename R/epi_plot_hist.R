#' @title Histogram wrapper function using ggplot2
#'
#' @description epi_plot_hist()
#'
#' @param df data.frame with x var to plot
#' @param var_x Variable to plot on x-axis, pass as a string.
#' @param ... passes arguments to ggplot2::geom_histogram()
#'
#' @return Prints a ggplot2 histogram
#'
#' @note For other options, save as object and build on the layers.
#' var_x is passed to ggplot2::aes_string
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plots_to_grid}},
#' \code{\link[ggplot2]{ggplot}},
#' \code{\link[ggplot2]{geom_histogram}},
#' \code{\link{epi_plot_cow_save}}.
#'
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#' set.seed(12345)
#' n <- 20
#' df <- data.frame(var_id = rep(1:(n / 2), each = 2),
#' 								 var_to_rep = rep(c("Pre", "Post"), n / 2),
#' 								 x = rnorm(n),
#' 								 y = rbinom(n, 1, 0.50),
#' 								 z = rpois(n, 2)
#' 								 )
#' df
#' df$x # continuous variable
#' my_hist_plot <- epi_plot_hist(df, 'x') # pass with quotes as using ggplot2::aes_string()
#' my_hist_plot
#' # Change the bins:
#' my_hist_plot <- epi_plot_hist(df, 'x', breaks = seq(-3, 3, by = 1))
#' my_hist_plot
#' # Add titles and axis names:
#' my_hist_plot <- my_hist_plot +
#' 	labs(title = "Histogram for X") +
#' 	labs(x = "X", y = "Count")
#' my_hist_plot
#' # Add axis limits:
#' my_hist_plot <- my_hist_plot +
#' 	xlim(c(-4, 4)) +
#' 	ylim(c(0, 10))
#' my_hist_plot
#' # Histogram with density curve:
#' my_hist_plot <- my_hist_plot + geom_density(col = 2)
#' my_hist_plot
#' # Histogram overlaid with kernel density curve:
#' # http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
#' my_hist_plot <- my_hist_plot +
#' # Density instead of count on y-axis:
#' 	geom_histogram(aes( y = ..density..),
#' 							   binwidth = 0.5,
#' 							   colour = "black",
#' 								 fill = "white") +
#' 	geom_density(alpha = 0.2, fill = "#FF6666") + # Overlay with transparent density plot
#' 	ylab('Density')
#' my_hist_plot
#' }
#'
#' @export
#'



epi_plot_hist <- function(df = NULL,
													var_x = '',
													...
													) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	# var_x <- enquo(var_x) # enquosure required for non-standard R object evaluation
	hist_plot <- ggplot2::ggplot(data = df,
															 ggplot2::aes_string(var_x)
	) + # aes_string() is soft-deprecated
		ggplot2::geom_histogram(...) +
		epi_plot_theme_2() + # Needs ggtheme.R functions in this package
		ggplot2::labs(y = 'Count')

	return(hist_plot)
}
