#' @title Volcano plot
#'
#' @description epi_plot_volcano()
#' Pass a column with log2 fold changes and raw p-values to get a volcano plot.
#'
#' @param logFC limma's estimate of the log2-fold-change.
#' @param p_val raw p-values. This is converted to -log10(p_val)
#' @param main base plot() main parameter. String to use as main title.
#' Default is an empty string.
#' @param pch plotting character, base plot() pch parameter. Default is 20.
#' @param h_abline add straight lines, base plot() abline parameter. Default is 2.
#' @param v_abline add straight lines, base plot() abline parameter. Default is c(0.8, 1.2).
#' @param ... Pass any other arguments to base plot()
#'
#' @return Prints a volcano plot to screen of the fold changes
#'
#' @note Assumes small effect sizes with vertical line cuts at 0.8 and 1.2 logFC.
#' The horizontal line cuts at a p-value of 0.05.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plots_to_grid}},
#' \code{\link{epi_plot_cow_save}},
#' \code{\link[limma]{topTable}},
#' \code{\link{plot}}.
#'
# @examples
#' \dontrun{
#' # # Simple example of fold changes and log fold changes:
#' a <- 1
#' b <- 10
#' # Fold change:
#' fc <- b / a
#' fc
#' # log2 fold change, where 0 is no effect and it is easier to visualise negative
#' # and positive values
#' log2FC <- log2(b) - log2(a)
#' log2FC
#' # Get the fold change from the log2 fold change:
#' 2^(log2(fc))
#'
#' # Directly from:
#' # https://www.rdocumentation.org/packages/limma/versions/3.28.14/topics/lmFit
#' # See also:
#' # https://www.nature.com/articles/ng1032z.pdf
#' # https://genomebiology.biomedcentral.com/articles/10.1186/s13059-014-0550-8
#' library(limma)
#' sd <- 0.3 * sqrt(4 / rchisq(100, df = 4))
#' y <- matrix(rnorm(100 * 6, sd = sd), 100, 6)
#' rownames(y) <- paste("Gene", 1:100)
#' y[1:2, 4:6] <- y[1:2, 4:6] + 2
#' design <- cbind(Grp1 = 1,Grp2vs1 = c(0, 0, 0, 1, 1, 1))
#' options(digits = 3)

#' # Ordinary fit
#' fit <- lmFit(y,design)
#' fit <- eBayes(fit)
#' top_table <- topTable(fit, coef = 2, number = Inf)
#' colnames(top_table)

#' # Volcano plot from limma with log2fold change, use this instead:
#' volcanoplot(fit ,coef = 2, highlight = 2)
#' epi_plot_volcano(logFC = top_table$logFC, p_val = top_table$P.Value)

#' # Other plots:
#' Mean-difference plot
#' plotMD(fit,column=2)

#' # Q-Q plot of moderated t-statistics
#' qqt(fit$t[,2],df=fit$df.residual+fit$df.prior)
#' abline(0,1)
#'
#' }
#'
# @export
#'
#' @importFrom graphics abline plot
#'

epi_plot_volcano <- function(logFC = NULL,
                             p_val = NULL,
                             main = '',
                             pch = 20,
                             h_abline = 2,
                             v_abline = c(0.8, 1.2),
                             ...
                             ) {
  volcano <- plot(2^(logFC),
                  -log10(p_val),
                  pch = pch,
                  main = main,
                  abline(h = h_abline,
                         v = v_abline),
                  ...
                  )
}

