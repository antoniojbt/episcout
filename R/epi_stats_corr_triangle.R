#' @title Get the lower triangle from a correlation matrix
#'
#' @description Keep only the lower triangle of the correlation matrix
#' Useful to create a nicer heatmap. Requires the original, unmelted correlation
#' matrix with both correlation (r) and p-values (P). The lower triangles are
#' converted to \code{data.table} before melting.
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
#' \dontrun{
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c("Pre", "Post"), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' epi_clean_count_classes(df)
#' df_corr <- df %>% select_if(~ epi_clean_cond_numeric(.))
#' df_corr <- df_corr[, -1] # exclude var_id
#' cormat_all <- epi_stats_corr(df_corr, method = "pearson")
#' melted_triangles <- epi_stats_corr_triangle(cormat = cormat_all$cormat)
#' melted_triangles$cormat_melted_triangle_r
#' melted_triangles$cormat_melted_triangle_pval
#' class(melted_triangles)
#' }
#' @export
#'

epi_stats_corr_triangle <- function(cormat = "cormat_all$cormat") {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package data.table needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  cormat_tri_r <- as.matrix(cormat$r)
  cormat_tri_P <- as.matrix(cormat$P)

  # Turn all upper triangle values to NA:
  cormat_tri_r[upper.tri(cormat_tri_r)] <- NA
  dt_r <- data.table::data.table(Var1 = rownames(cormat_tri_r), cormat_tri_r)
  cormat_melted_triangle_r <- data.table::melt(
    dt_r,
    id.vars = "Var1",
    variable.name = "Var2",
    value.name = "value",
    na.rm = TRUE
  )

  cormat_tri_P[upper.tri(cormat_tri_P)] <- NA
  dt_p <- data.table::data.table(Var1 = rownames(cormat_tri_P), cormat_tri_P)
  cormat_melted_triangle_pval <- data.table::melt(
    dt_p,
    id.vars = "Var1",
    variable.name = "Var2",
    value.name = "value",
    na.rm = TRUE
  )
  # Return melted triangles:
  melted_triangles <- list(
    cormat_melted_triangle_r = cormat_melted_triangle_r,
    cormat_melted_triangle_pval = cormat_melted_triangle_pval
  )
  melted_triangles
}
