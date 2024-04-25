#' @title Barplot wrapper function using ggplot2
#'
#' @description epi_plot_bar() wraps ggplot2 barplot for one or two variables with
#' a number of set preferences, see below.
#'
#' @param df data.frame with x var to plot
#' @param var_x Variable to plot on x-axis, pass as a string.
#' @param var_y Variable to plot on y-axis, pass as a string.
#' @param fill Interior colour used to fill. If only passing var_x it defaults to 'black'.
#' If passing both var_y and var_x, it uses var_x.
#' @param bar_colour Aesthetics for ggplot2. Default is 'black' if only var_x.
#' @param guides_fill Fill for ggplot2 guides. Default is 'none'.
#' @param y_lab y-axis label. Default is 'Count'.
#' @param x_lab x-axis label. Default is x_var.
#' @param ... pass further arguments to ggplot2::geom_bar()
#'
#' @return Prints a ggplot2 barplot
#'
#' @note For other options, save as object and build on the layers.
#' var_x and var_y are passed to ggplot2::aes_string.
#' For colour and fill see ggplot2::fill for further information.
#' epi_plot_bar() for one variable uses stat = 'count' and is coloured according
#' to the x variable passed, black borders for bars and no legend by default.
#' For two variables it assumes you want to colour according to the x variable
#' and that stat = 'identity' is what's needed. No legend by default.
#' stat = 'identity' uses the height of the bar to represent the value of the
#' passed column.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plots_to_grid}},
#' \code{\link{epi_plot_box}},
#' \code{\link{epi_plot_hist}},
#' \code{\link[ggplot2]{ggplot}},
#' \code{\link[ggplot2]{geom_bar}}.
#'
#' @examples
#'
#' \dontrun{
#' # Set up an example:
#' set.seed(12345)
#' n <- 20
#' df <- data.frame(var_id = rep(1:(n / 2), each = 2),
#'                  var_to_rep = rep(c("Pre", "Post"), n / 2),
#'                               x = rnorm(n),
#'                               y = rbinom(n, 1, 0.50),
#'                               z = rpois(n, 2),
#' 	                            w = sample(1:20, 20)
#'                  )
#' df$id_unique <- paste0(df[['var_id']], '_', df[['var_to_rep']])
#' df[, 'var_id'] <- as.character(df[, 'var_id'])
#' df[, 'y'] <- as.factor(df[, 'y'])
#' str(df)
#'
#' # Barplot for single variable:
#' summary(df$var_to_rep)
#' plot_bar <- epi_plot_bar(df, 'var_to_rep')
#' plot_bar
#'
#' # Barplot for two variables side by side:
#' df_bar <- reshape2::melt(df[, c('w', 'z', 'id_unique')], id.vars = 'id_unique')
#' epi_head_and_tail(df, cols = 7)
#' epi_head_and_tail(df_bar, cols = 3)
#' plot_bar <- epi_plot_bar(df_bar,
#'                          var_x = 'id_unique',
#'                          var_y = 'value',
#'                          fill = 'variable') +
#' 	theme(axis.text.x = element_text(angle = 90, hjust = 1))
#' plot_bar
#' # Which should be the same as:
#' ggplot(df_bar, aes(x = id_unique, y = value, fill = variable)) +
#'        geom_bar(stat = 'identity', position = 'dodge') +
#'  	theme(axis.text.x = element_text(angle = 90, hjust = 1))
#' }
#'
#' @export
#'

epi_plot_bar <- function(df = NULL,
                         var_x = NULL,
                         var_y = '',
                         fill = NULL,
                         bar_colour = 'black',
                         guides_fill = 'none',
                         y_lab = 'Count',
                         x_lab = var_x,
                         ...
) {
  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # If only y is passed, plot of one variable:
  if (var_y == '') {
    bar_plot_one <- ggplot2::ggplot(df,
                                    ggplot2::aes(x = !!sym(var_x), #.data[[var_y]]),
                                                 fill = var_x)
    ) +
      ggplot2::geom_bar(stat = 'count',
                        colour = bar_colour,
                        ...
      ) +
      ggplot2::guides(fill = guides_fill) +
      ggplot2::labs(y = y_lab, x = x_lab)
    return(bar_plot_one)
  }
  # If both x and y are passed, plot of two variables:
  else if (!is.null(var_y)) {
    bar_plot <- ggplot2::ggplot(df,
                                ggplot2::aes(x = !!sym(var_x),
                                             y = !!sym(var_y),
                                             fill = fill)
    ) +
      ggplot2::geom_bar(stat = 'identity',
                        position = 'dodge',
                        ...
      ) +
      ggplot2::labs(y = y_lab, x = x_lab)
    return(bar_plot)
  }
}
