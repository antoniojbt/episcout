#' @title Get the lower triangle from a correlation matrix
#'
#' @description Keep only the lower triangle of the correlation matrix
#' Useful to create a nicer heatmap. Requires the original, unmelted correlation
#' matrix with both correlation (r) and p-values (P).
#'
#' @param cormat a matrix object (usually the output of Hmisc::rcorr()
#' or episcout::epi_stats_corr()). Default 'cormat_all$cormat'
#'
#' @return Returns a list object melted_triangles containing the lower triangle of the
#' correlation matrix (cormat) with the correlation (cormat_melted_triangle_r) and
#' p-values (cormat_melted_triangle_pval).
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_corr}}, \code{\link{epi_stats_corr_rename}},
#' \code{\link{epi_plot_heatmap}}, \code{\link{epi_plot_heatmap_triangle}},
#' \code{\link[Hmisc]{rcorr}}, \code{\link[data.table]{melt}}
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
#' epi_clean_count_classes(df)
#' df_corr <- df %>%select_if(~ epi_clean_cond_numeric(.))
#' df_corr <- df_corr[, -1] # exclude var_id
#' cormat_all <- epi_stats_corr(df_corr, method = 'pearson')
#' melted_triangles <- epi_stats_corr_triangle(cormat = cormat_all$cormat)
#' melted_triangles$cormat_melted_triangle_r
#' melted_triangles$cormat_melted_triangle_pval
#' class(melted_triangles)
#' }
#' @export
#'

epi_stats_corr_triangle <- function(cormat = 'cormat_all$cormat') {

  if (!requireNamespace('data.table', quietly = TRUE)) {
    stop('Package data.table needed for this function to work. Please install it.',
         call. = FALSE)
  }
  cormat_tri_r <- as.matrix(cormat$r)
  cormat_tri_P <- as.matrix(cormat$P)
  # TO DO: fix warning message: "The melt generic in data.table has been passed a matrix and will attempt to redirect to the relevant reshape2 method"
  # reshape2 is deprecated, switching to data.table has knock-on errors though in
  # epi_stats_corr_rename()
  # epi_plot_heatmap()
  # epi_plot_heatmap_triangle()
  #
  # cormat_tri_r <- data.table::as.data.table(cormat$r)
  # cormat_tri_r <- cormat$r
  # cormat_tri_P <- data.table::as.data.table(cormat$P)
  # cormat_tri_P <- cormat$P

  # Turn all upper triangle values to NA:
  cormat_tri_r[upper.tri(cormat_tri_r)] <- NA
  # Melt and remove NAs:
  # TO DO: Fix warning message, "Consider providing at least one of 'id' or 'measure' vars"
  cormat_melted_triangle_r <- data.table::melt(cormat_tri_r,
                                               na.rm = TRUE)
  # Usually no NAs for correlation values though, indices will not match for cormat_tri_r and cormat_tri_P files
  # And the same for p-values:
  cormat_tri_P[upper.tri(cormat_tri_P)] <- NA
  # Melt:
  cormat_melted_triangle_pval <- data.table::melt(cormat_tri_P,
                                                  na.rm = TRUE)
  # Return melted triangles:
  melted_triangles <- list(cormat_melted_triangle_r = cormat_melted_triangle_r,
                           cormat_melted_triangle_pval = cormat_melted_triangle_pval
                           )
  return(melted_triangles)
}
