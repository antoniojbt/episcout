#' Volcano plot
#'
#' Draw a simple volcano plot given log2 fold changes and raw p-values.
#'
#' @param logFC Numeric vector of log2 fold changes.
#' @param p_val Numeric vector of raw p-values (will be transformed to
#'   `-log10(p_val)`).
#' @param main Main title passed to [graphics::plot()].
#' @param pch Plotting character passed to [graphics::plot()].
#' @param h_abline Horizontal line position passed to [graphics::abline()].
#' @param v_abline Vertical line positions passed to [graphics::abline()].
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return Invisibly returns `NULL` and draws the plot to the active device.
#'
#' @note Assumes small effect sizes with vertical cut-offs at 0.8 and 1.2
#'   log2 fold change and a horizontal cut-off corresponding to a p-value of 0.05.
#'
#' @seealso [epi_plot_list()], [epi_plots_to_grid()],
#'   [epi_plot_cow_save()], [graphics::plot()]
#'
#' @author Antonio Berlanga-Taylor <https://github.com/AntonioJBT/episcout>
#'
#' @examples
#' \dontrun{
#' log2fc <- rnorm(100)
#' pvals <- runif(100)
#' epi_plot_volcano(log2fc, pvals)
#' }
#'
#' @export
#' @importFrom graphics abline plot
epi_plot_volcano <- function(logFC = NULL,
                             p_val = NULL,
                             main = "",
                             pch = 20,
                             h_abline = 2,
                             v_abline = c(0.8, 1.2),
                             ...) {
  plot(logFC,
    -log10(p_val),
    pch = pch,
    main = main,
    ...
  )
  abline(h = h_abline, v = v_abline)
  invisible(NULL)
}
