#' @title Get a correlation matrix with p-values
#'
#' @description Get a correlation matrix using Hmisc for many numerical variables.
#' epi_stats_corr() wraps Hmisc::rcorr(), melts the matrix and
#' returns correlation and p-values.
#'
#' @param df a data frame class object coerced to matrix and passed to Hmisc::rcorr()
#'
#' @param method should be a string that can be accepted by type parameter in
#' Hmisc::rcorr(). 'Spearman' (default here) or 'pearson'
#'
#' @return Returns a list object cormat_all containing the correlation matrix (cormat),
#' the melted correlation values (cormat_melted_r) (ie long format for plotting for example)
#' and the melted p-values (cormat_melted_pval).
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_corr_triangle}},
#' \code{\link{epi_stats_corr_rename}},
#' \code{\link{epi_plot_heatmap}},
#' \code{\link{epi_plot_heatmap_triangle}},
#' \code{\link[Hmisc]{rcorr}},
#' \code{\link[data.table]{melt}}.
#'
#' @examples
#'
#' \dontrun{
#' library(Hmisc)
#' library(data.table)
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
#' names(cormat_all)
#' names(cormat_all$cormat)
#' cormat_all$cormat$r
#' cormat_all$cormat_melted_r
#' class(cormat_all)
#' }
#'
#' @export
#'

epi_stats_corr <- function(df = NULL,
                           method = 'spearman'
                           ) {
  if (!requireNamespace('Hmisc', quietly = TRUE)) {
    stop('Package Hmisc needed for this function to work. Please install it.',
         call. = FALSE)
    }
  if (!requireNamespace('dplyr', quietly = TRUE)) {
    stop('Package dplyr needed for this function to work. Please install it.',
         call. = FALSE)
  }
  if (!requireNamespace('tidyr', quietly = TRUE)) {
    stop('Package tidyr needed for this function to work. Please install it.',
         call. = FALSE)
  }
  cormat <- Hmisc::rcorr(as.matrix(df), type = method)

  # Correlation values:
  cormat_melted_r <- as.data.frame(cormat$r) %>%
    tibble::rownames_to_column("Var1") %>%
    tidyr::pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")

  # P-values separately:
  cormat_melted_pval <- as.data.frame(cormat$P) %>%
    tibble::rownames_to_column("Var1") %>%
    tidyr::pivot_longer(-Var1, names_to = "Var2", values_to = "pvalue")

  # Sanity: identical(rownames(cormat_melted_r),
  # rownames(cormat_melted_pval))
  cormat_all <- list(cormat = cormat,
                     cormat_melted_r = cormat_melted_r,
                     cormat_melted_pval = cormat_melted_pval
  )
  return(cormat_all)
}

