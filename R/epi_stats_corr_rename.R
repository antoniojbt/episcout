#' @title Rename variables from melted triangles and convert to factors
#'
#' @description Rename variables from melted triangles and convert to factors
#' for plotting. Requires correlation matrices with r and p-values with columns
#' named 'Var1' and 'Var2'. The output of epi_stats_corr_triangle() can be
#' used as input. Variables are converted to factors and re-labelled
#' Values are rounded to digits (default is 2 so that they fit in the heatmap).
#' The output can be used for prettier plotting.
#'
#' @param r_vals Correlation values in melted (long) format.
#' Default is melted_triangles$cormat_melted_triangle_r
#'
#' @param p_vals p-values from correlations in melted (long) format.
#' Default is melted_triangles$cormat_melted_triangle_pval
#'
#' @param vars_list List of variables to rename from 'Var1' and 'Var2'.
#' Default is vars_list
#'
#' @param var_labels List of labels to use for renaming values from 'Var1' and 'Var2'.
#' vars_list and var_labels must match by position. Default is var_labels
#'
#' @param digits Rounding integer for correlation and p-values. Default is 2
#'
#' @return Returns a list object melted_triangles containing the lower triangle of the
#' correlation matrix (cormat) with the correlation (cormat_melted_triangle_r) and
#' p-values (cormat_melted_triangle_pval) with the variables 'Var1' and 'Var2'
#' converted to factors and re-labelled.
#'
#' @note vars_list and var_labels must match by position.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_corr}}, \code{\link{epi_stats_corr_triangle}},
#' \code{\link{epi_plot_heatmap}}, \code{\link{epi_plot_heatmap_triangle}},
#' \code{\link[Hmisc]{rcorr}}.
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
#' melted_triangles
#' vars_list <- c('x', 'y', 'z')
#' var_labels <- c('numeric', 'binomial', 'poisson')
#' renamed_triangles <- epi_stats_corr_rename(melted_triangles$cormat_melted_triangle_r,
#'                                            melted_triangles$cormat_melted_triangle_pval,
#'                                            vars_list = vars_list,
#'                                            var_labels = var_labels
#' 																					 )
#' renamed_triangles
#' }
#' @export
#'

epi_stats_corr_rename <- function(r_vals = 'melted_triangles$cormat_melted_triangle_r',
																	p_vals = 'melted_triangles$cormat_melted_triangle_pval',
																	vars_list = vars_list,
																	var_labels = var_labels,
																	digits = 2
																	) {
	r_vals$Var1 <- factor(r_vals$Var1,
												levels = vars_list,
												labels = var_labels
												)
	r_vals$Var2 <- factor(r_vals$Var2,
												levels = vars_list,
												labels = var_labels
												)
	# head_and_tail(cormat_melted_triangle_r, cols = 3)
	# summary(cormat_melted_triangle_r) Rename p-values:
	p_vals$Var1 <- factor(p_vals$Var1,
												levels = vars_list,
												labels = var_labels
												)
	p_vals$Var2 <- factor(p_vals$Var2,
												levels = vars_list,
												labels = var_labels
												)
	# head_and_tail(cormat_melted_triangle_pval,
	#               cols = 3)
	# summary(cormat_melted_triangle_pval)
	# Add rounded correlation coefficients to plot:
	r_vals$value <- round(r_vals$value, 2)
	p_vals$value <- round(p_vals$value, 2)
	# Return the renamed and as factor melted triangles:
	melted_triangles <- list(cormat_melted_triangle_r = r_vals,
													 cormat_melted_triangle_pval = p_vals)
	return(melted_triangles)
	}
