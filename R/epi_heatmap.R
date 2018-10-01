######################
# Helper functions for plotting a heatmap
# Antonio Berlanga-Taylor
######################

######################
# Plot correlations between numeric variables
# Set up correlation matrix with p-values:

#####
# Get a correlation matrix using Hmisc for many numerical variables
# method should be a string that can be accepted by hmisc::rcorr()
# df should be a data frame class object
epi_heatmap_corr <- function(df, method = 'spearman') {
	if (!requireNamespace('Hmisc', quietly = TRUE)) {
		stop("Package Hmisc needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('data.table', quietly = TRUE)) {
		stop("Package data.table needed for this function to work. Please install it.",
				 call. = FALSE)
	}
  cormat <- Hmisc::rcorr(as.matrix(df),
  								type = method)
  # Correlation values:
  cormat_melted_r <- data.table::melt(cormat$r)
  # P-values separately:
  cormat_melted_pval <- melt(cormat$P)
  # Sanity:
  # identical(rownames(cormat_melted_r), rownames(cormat_melted_pval))
  cormat_all <- list('cormat' = cormat,
  									 'cormat_melted_r' = cormat_melted_r,
  									 'cormat_melted_pval' = cormat_melted_pval)
  return(cormat_all)
  }
# Test data:
# TO DO: add actual test data
# df <- input_data[, vars_list]
# cormat_all <- epi_heatmap_corr(df)
# names(cormat_all)
# cormat_all$cormat$n
# cormat_all$cormat_melted_r
# class(cormat_all)
#####

#####
# Keep only the lower triangle of the correlation matrix for a nicer heatmap
# Requires the original, unmelted correlation matrix with both correlation (r)
# and p-values (P)
# Designed to take the output of epi_heatmap_corr() as input
# Leaves the original result of epi_heatmap_corr() unchanged.
epi_heatmap_corr_triangle <- function(cormat_all) {
	if (!requireNamespace('data.table', quietly = TRUE)) {
		stop("Package data.table needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	cormat_tri <- cormat_all
	# Turn all lower triangle values to NA:
	cormat_tri$cormat$r[lower.tri(cormat_tri$cormat$r)] <- NA
	# Melt and remove NAs:
	cormat_melted_triangle_r <- melt(cormat_tri$cormat$r, na.rm = TRUE)
	# And the same for p-values:
	cormat_tri$cormat$P[lower.tri(cormat_tri$cormat$P)] <- NA
	# Melt:
	cormat_melted_triangle_pval <- melt(cormat_tri$cormat$P, na.rm = TRUE)
  # Return melted triangles:
  melted_triangles <- list('cormat_melted_triangle_r' = cormat_melted_triangle_r,
  												 'cormat_melted_triangle_pval' = cormat_melted_triangle_pval)
	return(melted_triangles)
	}
# Test:
# melted_triangles <- epi_heatmap_corr_triangle(cormat_all)
# melted_triangles$cormat_melted_triangle_r
# melted_triangles$cormat_melted_triangle_pval
# class(melted_triangles)
#####

#####
# Rename variables from melted triangles and convert to factors for plotting:
# Requires correlation matrices with r and p-values with columns named
# Var1 and Var2
# The outputs of epi_heatmap_corr_triangle() can be passed here
# vars_list and var_labels must match by position
# Variables are converted to factors and re-labelled
# Values are rounded to digits (default is 2 so that they fit in the heatmap)
epi_heatmap_rename <- function(r_vals = 'melted_triangles$cormat_melted_triangle_r',
															 p_vals = 'melted_triangles$cormat_melted_triangle_pval',
															 vars_list = vars_list,
															 var_labels = var_labels,
															 digits = 2) {
	r_vals$Var1 <- factor(r_vals$Var1,
												levels = vars_list,
												labels = var_labels)
	r_vals$Var2 <- factor(r_vals$Var2,
												levels = vars_list,
												labels = var_labels)
	# head_and_tail(cormat_melted_triangle_r, cols = 3)
  # summary(cormat_melted_triangle_r)
  # Rename p-values:
  p_vals$Var1 <- factor(p_vals$Var1,
  											levels = vars_list,
  											labels = var_labels)
  p_vals$Var2 <- factor(p_vals$Var2,
  											levels = vars_list,
  											labels = var_labels)
  # head_and_tail(cormat_melted_triangle_pval, cols = 3)
  # summary(cormat_melted_triangle_pval)
  # # Add rounded correlation coefficients to plot:
  r_vals$value <- round(r_vals$value, 2)
  p_vals$value <- round(p_vals$value, 2)
  # Return the renamed and as factor melted triangles:
  melted_triangles <- list('cormat_melted_triangle_r' = r_vals,
  												 'cormat_melted_triangle_pval' = p_vals)
  return(melted_triangles)
  }
# Test:
# TO DO: add vars_list and var_labels
# renamed_triangles <- epi_heatmap_rename(melted_triangles$cormat_melted_triangle_r,
# 																				melted_triangles$cormat_melted_triangle_pval,
# 																				vars_list = vars_list,
# 																				var_labels = var_labels
# 																				)
# renamed_triangles$cormat_melted_triangle_r
# renamed_triangles$cormat_melted_triangle_pval
#####
######################

######################
# Simple heatmap:
# Pass the correlation values from a melted (long) correlation matrix
epi_heatmap_simple <- function(cormat_melted_r, title = '', ...) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	heat_map <- ggplot(data = as.data.frame(cormat_melted_r),
										 aes(x = Var1, y = Var2,
										 		fill = value)) +
		geom_tile() +
	  labs(title = title, y = '', x = '') +
	  theme_Publication() +
	  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
				plot.title = element_text(hjust = 0.5)
	  )
	return(heat_map)
	}
# Test:
# epi_heatmap_simple(cormat_all$cormat_melted_r)
# epi_heatmap_simple(renamed_triangles$cormat_melted_triangle_r)
# epi_heatmap_simple(renamed_triangles$cormat_melted_triangle_pval)
# ggsave('my_heatmap.svg')
######################

######################
# Heatmap ggplot2 wrapper function for a triangle heatmap
# Requires a data.frame with correlations and a data.frame with
# matching p-values. Columns headers in both must be called Var1 and Var2.
# Use hmisc::rcorr(), data.table::melt(), cormat[lower.tri(cormat)] <- NA
#  to get these outputs for example.
# cor_method is a string passed to the legend title
# It expects the name of the method used for correlation (eg 'Spearman')
# ... are passed
epi_heatmap <- function(cormat_melted_triangle_r,
											 cormat_melted_triangle_pval,
											 cor_method = 'Spearman',
											 show_values = 'pval' # or 'corr'
											 ) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) {
		stop("Package ggplot2 needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (show_values == 'pval') {
		show_data <- cormat_melted_triangle_pval
		legend_title <- sprintf("%s correlation (colour scale)\nand unadjusted P-values",
														cor_method)
		} else {
			show_data <- cormat_melted_triangle_r
			legend_title <- sprintf("%s correlation (colour scale \nand numbers)",
															cor_method)
		}
	heatmap_triangle <- ggplot(data = cormat_melted_triangle_r,
				 aes(x = Var2, y = Var1, fill = value)) +
		geom_tile(color = "light grey") +
		scale_fill_gradient2(low = "blue", high = "red", mid = "white",
												 midpoint = 0, limit = c(-1, 1), space = "Lab",
												 name = legend_title
												 ) +
		theme_Publication() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
					plot.title = element_text(hjust = 0.5)
		) +
		coord_fixed() +
		# Write values can be 'pval' or 'corr':
		geom_text(data = show_data,
							aes(x = Var2, y = Var1, label = value),
							color = "black", size = 3) +
		theme(axis.title.x = element_blank(),
					axis.text.x = element_text(angle = 90),
					axis.title.y = element_blank(),
					panel.grid.major = element_blank(),
					panel.border = element_blank(),
					panel.background = element_rect(),
					axis.ticks = element_blank(),
					legend.justification = c(1, 0),
					legend.position = c(0.5, 0.8),
					legend.direction = 'horizontal') +
		guides(fill = guide_colorbar(barwidth = 12, barheight = 2,
																 title.position = 'top', title.hjust = 0.5)
					 )
	return(heatmap_triangle)
	}
# Test:
# epi_heatmap(renamed_triangles$cormat_melted_triangle_r,
# 						renamed_triangles$cormat_melted_triangle_pval,
# 					  show_values = 'pval'#'corr'
# 						)
# ggsave('epi_heatmap_triangle.svg',
# 			 height = 12,
# 			 width = 12,
# 			 units = 'in')
######################