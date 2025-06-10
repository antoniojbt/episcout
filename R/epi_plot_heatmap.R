#' @title Plot a simple heatmap using ggplot2
#'
#' @description Plot correlations between numeric variables as a heatmap using
#' ggplot2. Pass the correlation values from a melted (long) correlation matrix as
#' input.
#'
#' @param cormat_melted a matrix object (usually the output of Hmisc::rcorr()
#' r (or P) or episcout::epi_stats_corr()). Default is 'cormat_all$cormat_melted_r'
#'
#' @param title Main title for the plot. Default is blank ('')
#'
#' @return Returns a heatmap as a ggplot2 object
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_corr}},
#' \code{\link{epi_stats_corr_triangle}},
#' \code{\link{epi_stats_corr_rename}},
#' \code{\link{epi_plot_heatmap_triangle}}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c("Pre", "Post"), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' df_corr <- df %>% select_if(~ epi_clean_cond_numeric(.))
#' df_corr <- df_corr[, -1] # exclude var_id
#' cormat_all <- epi_stats_corr(df_corr, method = "pearson")
#' melted_triangles <- epi_stats_corr_triangle(cormat = cormat_all$cormat)
#' vars_list <- c("x", "y", "z")
#' var_labels <- c("numeric", "binomial", "poisson")
#' renamed_triangles <- epi_stats_corr_rename(melted_triangles$cormat_melted_triangle_r,
#'   melted_triangles$cormat_melted_triangle_pval,
#'   vars_list = vars_list,
#'   var_labels = var_labels
#' )
#' library(ggplot2)
#' library(ggthemes)
#' epi_plot_heatmap(cormat_all$cormat_melted_r)
#' epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_r)
#' epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_pval)
#' ggsave("my_heatmap.svg")
#' }
#' @export
#'

epi_plot_heatmap <- function(cormat_melted = "cormat_all$cormat_melted_r",
                             title = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  df <- as.data.frame(cormat_melted)
  if (!"value" %in% colnames(df)) {
    if ("correlation" %in% colnames(df)) {
      df$value <- df$correlation
    } else if ("pvalue" %in% colnames(df)) {
      df$value <- df$pvalue
    } else {
      stop("Column `value`, `correlation` or `pvalue` not found in cormat_melted")
    }
  }

  heat_map <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = .data$Var1,
      y = .data$Var2,
      fill = .data$value
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::labs(
      title = title,
      y = "",
      x = ""
    ) +
    epi_plot_theme_2() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5
      ),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  return(heat_map)
}
