#' @title Boxplot wrapper function using ggplot2
#'
#' @description epi_plot_box() wraps ggplot2 boxplot for one or two variables with
#' a number of set preferences, see below.
#'
#' @param df data.frame with x var to plot
#' @param var_y Variable to plot on y-axis, pass as a string.
#' @param var_x Variable to plot on x-axis, pass as a string.
#' @param outlier_alpha Outlier transparency. Default is 0.7.
#' @param fill Interior colour used to fill. If only passing var_y it defaults to 'grey80'.
#' If passing both var_y and var_x, it uses var_x.
#' @param colour Aesthetics for ggplot2. Default is 'grey20' if only var_y.
#' @param stat_geom ggplot2::stat_boxplot parameter used for 2 variable boxplot only.
#' Default is 'errorbar'.
#' @param stat_width ggplot2::stat_boxplot parameter used for 2 variable boxplot only.
#' Default is 0.5.
#' @param jitter_shape ggplot2::geom_jitter parameter used for 2 variable boxplot only.
#' Default is 16.
#' @param jitter_position ggplot2::geom_jitter parameter used for 2 variable boxplot only.
#' Default is 0.2.
#' @param jitter_alpha ggplot2::geom_jitter parameter used for 2 variable boxplot only.
#' Default is 0.5.
#' @param sum_fun ggplot2::stat_summary parameter used for 2 variable boxplot only.
#' Default is mean.
#' @param sum_geom ggplot2::stat_summary parameter used for 2 variable boxplot only.
#' Default is "point".
#' @param sum_shape ggplot2::stat_summary parameter used for 2 variable boxplot only.
#' Default is 23.
#' @param sum_size ggplot2::stat_summary parameter used for 2 variable boxplot only.
#' Default is 4.
#' @param ... passes any further arguments to ggplot2::geom_boxplot() for both one and two
#' column plots.
#'
#' @return Prints a ggplot2 boxplot
#'
#' @note For other options, save as object and build on the layers.
#' var_x and var_y are passed to ggplot2::aes.
#' For colour and fill see ggplot2::fill for further information.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plots_to_grid}},
#' \code{\link{epi_plot_hist}},
#' \code{\link[ggplot2]{ggplot}},
#' \code{\link[ggplot2]{geom_boxplot}}.
#'
#' @examples
#'
#' \dontrun{
#' Boxplot of one variable:
#' library(ggplot2)
#' library(ggthemes)
#' set.seed(12345)
#' n <- 20
#' df <- data.frame(var_id = rep(1:(n / 2), each = 2),
#'                  var_to_rep = rep(c("Pre", "Post"), n / 2),
#'                  x = rnorm(n),
#'                  y = rbinom(n, 1, 0.50),
#'                  z = rpois(n, 2)
#'                  )
#' df
#' df$x # continuous variable
#' epi_plot_box(df, var_y = 'x')
#' # Add notch:
#' epi_plot_box(df, var_y = 'x', notch = TRUE)
#'
#' # Boxplot for x and y variables:
#' df$x # continuous variable
#' df$var_to_rep # factor
#' epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x')
#' # Change colours, remove legend, etc.:
#' my_boxplot <- epi_plot_box(df, var_x = 'var_to_rep', var_y = 'x')
#' my_boxplot +
#'   # scale_fill_grey() +
#' scale_fill_brewer(palette = "Blues") +
#'   # scale_fill_brewer(palette = "Dark2") +
#'   theme(legend.position = "none") # Remove legend
#' # dev.off()
#' }
#'
#' @export
#'

epi_plot_box <- function(df = NULL,
                         var_y = NULL,
                         var_x = '',
                         outlier_alpha = 0.7,
                         fill = 'grey80',
                         colour = 'grey20',#'black',
                         stat_geom = 'errorbar',
                         stat_width = 0.5,
                         jitter_shape = 16,
                         jitter_position = 0.2,
                         jitter_alpha = 0.5,
                         sum_fun = mean,
                         sum_geom = "point",
                         sum_shape = 23,
                         sum_size = 4,
                         ...
                         ) {
  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('ggthemes', quietly = TRUE)) {
    stop("Package ggthemes needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('rlang', quietly = TRUE)) {
    stop("Package rlang needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # If only y is passed, boxplot of one variable:
  if (var_x == '') {
  box_plot_one <- ggplot2::ggplot(data = df,
                                  ggplot2::aes(y = !!sym(var_y))
                                  ) +
    ggplot2::geom_boxplot(outlier.alpha = outlier_alpha,
                          fill = fill,
                          colour = colour,
                          ...) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank()
    )

  return(box_plot_one)
  }
  # If both x and y are passed, boxplot of two variables:
  else if (!is.null(var_x)) {
      box_plot <- ggplot2::ggplot(data = df,
                                  ggplot2::aes(y = !!sym(var_y), #.data[[var_y]]),
                                               x = !!sym(var_x), #.data[[var_x]],
                                               fill = .data[[var_x]])
                                               ) +
        ggplot2::stat_boxplot(geom = stat_geom, width = stat_width) +
        ggplot2::geom_boxplot(outlier.alpha = outlier_alpha,
                              ...) +
        ggplot2::geom_jitter(#ggplot2::aes(x = .data[[var_x]], y = .data[[var_y]]),
                             shape = jitter_shape,
                             position = ggplot2::position_jitter(jitter_position),
                             alpha = jitter_alpha
                             ) +
        ggplot2::stat_summary(#ggplot2::aes(x = .data[[var_x]], y = .data[[var_y]]),
                              fun = sum_fun,
                              geom = sum_geom,
                              shape = sum_shape,
                              size = sum_size
                              ) +
        epi_plot_theme_2()
      return(box_plot)
    }
}
