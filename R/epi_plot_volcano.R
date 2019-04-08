#' @title Volcano plot
#'
#' @description epi_plot_volcano()
#' Designed to take limma's topTable output as input.
#'
#' @param logFC limma's estimate of the log2-fold-change.
#' @param adj.P.Val adjusted p-value or q-value. This is converted to -log10(adj.P.Val)
#' @param main base plot() main parameter. String to use as main title.
#' Default is an empty string.
#' @param pch plotting character, base plot() pch parameter. Default is 20.
#' @param h_abline add straight lines, base plot() abline parameter. Default is 2.
#' @param v_abline add straight lines, base plot() abline parameter. Default is c(0.8, 1.2).
#' @param ... Pass any other arguments to base plot()
#'
#' @return Prints a volcano plot to screen
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
# # TO DO
# epi_plot_volcano()
#
# \dontrun{
#
# }
#
#' @export
#'
#' @importFrom graphics abline plot
#'

epi_plot_volcano <- function(logFC = NULL,
                             adj.P.Val = NULL,
                             main = '',
                             pch = 20,
                             h_abline = 2,
                             v_abline = c(0.8, 1.2),
                             ...
                             ) {
  volcano <- plot(2^(logFC), # TO DO: check this is correct as limma gives
                             # estimate of the log2-fold-change 'logFC'
                  -log10(adj.P.Val),
                  pch = pch,
                  main = main,
                  abline(h = h_abline,
                         v = v_abline),
                  ...
                  )
  return(volcano)
}

