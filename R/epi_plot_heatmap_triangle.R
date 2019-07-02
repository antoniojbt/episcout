#' @title Plot a triangle heatmap using ggplot2
#'
#' @description Plot correlations between numeric variables as a triangle heatmap
#' using ggplot2. Prettier plot than a simple heatmap with only the lower
#' triangle of a correlation matrix.
#' Pass the correlation values from a melted (long) correlation matrix as
#' input. Requires a data.frame with correlations and a data.frame with matching p-values.
#' Column headers in both must be called Var1 and Var2. Use the output from
#' epi_stats_corr_triangle() for example.
#' cor_method is a string passed to the legend title It expects the name of the
#' method used for correlation (eg 'Spearman')
#'
#' @param cormat_melted_triangle_r a matrix object with correlation values
#' Usually the output of episcout::epi_stats_corr_triangle().
#'
#' @param cormat_melted_triangle_pval a matrix object with correlation p-values
#' Usually the output of episcout::epi_stats_corr_triangle().
#'
#' @param cor_method Correlation method used, will be printed in plot.
#' Default is 'Spearman'
#'
#' @param show_values Values to show in plot, 'pval' or 'corr'. Default is 'pval'.
#'
#' @return Returns a heatmap as a ggplot2 object
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_corr}}, \code{\link{epi_stats_corr_triangle}},
#' \code{\link{epi_stats_corr_rename}}, \code{\link{epi_plot_heatmap}}
#'
#' @examples
#'
#' \dontrun{
#' df <- data.frame(var_id = rep(1:(n / 2), each = 2),
#' var_to_rep = rep(c("Pre", "Post"), n / 2),
#' x = rnorm(n),
#' y = rbinom(n, 1, 0.50),
#' z = rpois(n, 2)
#' )
#' df_corr <- df %>% select_if(~ epi_clean_cond_numeric(.))
#' df_corr <- df_corr[, -1] # exclude var_id
#' cormat_all <- epi_stats_corr(df_corr, method = 'pearson')
#' melted_triangles <- epi_stats_corr_triangle(cormat = cormat_all$cormat)
#' vars_list <- c('x', 'y', 'z')
#' var_labels <- c('numeric', 'binomial', 'poisson')
#' renamed_triangles <- epi_stats_corr_rename(melted_triangles$cormat_melted_triangle_r,
#'                                            melted_triangles$cormat_melted_triangle_pval,
#'                                            vars_list = vars_list,
#'                                            var_labels = var_labels
#'                                            )
#' library(ggplot2)
#' library(ggthemes)
#' epi_plot_heatmap(cormat_all$cormat_melted_r)
#' epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_r)
#' epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_pval)
#'
#' epi_plot_heatmap_triangle(renamed_triangles$cormat_melted_triangle_r,
#'                           renamed_triangles$cormat_melted_triangle_pval,
#'                           show_values = 'pval'#'corr'
#'                           )
#' ggsave('epi_heatmap_triangle.svg',
#'        height = 12,
#'        width = 12,
#'        units = 'in'
#'        )
#'
#'}
#' @export
#'

epi_plot_heatmap_triangle <- function(cormat_melted_triangle_r = NULL,
                                      cormat_melted_triangle_pval = NULL,
                                      cor_method = 'Spearman',
                                      show_values = 'pval' # or 'corr'
                                      ) {
    if (!requireNamespace('ggplot2', quietly = TRUE)) {
        stop('Package ggplot2 needed for this function to work. Please install it.',
            call. = FALSE)
    }
    if (show_values == 'pval') {
        show_data <- cormat_melted_triangle_pval
        legend_title <- sprintf('%s correlation (colour scale)\nand unadjusted P-values',
            cor_method)
    } else {
        show_data <- cormat_melted_triangle_r
        legend_title <- sprintf('%s correlation (colour scale \nand numbers)',
            cor_method)
    }
    heatmap_triangle <- ggplot2::ggplot(data = cormat_melted_triangle_r,
                                        ggplot2::aes_string(x = 'Var1',
                                                            y = 'Var2',
                                                            fill = 'value')
                                        ) +
      ggplot2::geom_tile(color = 'light grey') +
      ggplot2::scale_fill_gradient2(low = 'blue',
                                    high = 'red',
                                    mid = 'white',
                                    midpoint = 0,
                                    limit = c(-1, 1),
                                    space = 'Lab',
                                    name = legend_title
                                    ) +
      epi_plot_theme_2() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5),
                     plot.title = ggplot2::element_text(hjust = 0.5)
                     ) +
      ggplot2::coord_fixed() + # Write values can be 'pval' or 'corr':
      ggplot2::geom_text(data = show_data,
                         ggplot2::aes_string(x = 'Var1',
                                             y = 'Var2',
                                             label = 'value'),
                         color = 'black',
                         size = 3) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 90),
                     axis.title.y = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.justification = c(1, 0),
                     legend.position = c(0.5, 0.8),
                     legend.direction = 'horizontal') +
      ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 12,
                                                     barheight = 2,
                                                     title.position = 'top',
                                                     title.hjust = 0.5)
                      )
    return(heatmap_triangle)
    }
