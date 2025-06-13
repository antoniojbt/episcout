#' IMSS ggplot theme
#'
#' Provides a ggplot2 theme with IMSS corporate colours. Package dependencies are
#' checked at run time.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @return A ggplot2 theme object.
#' @examples
#' if (requireNamespace('ggplot2', quietly = TRUE) &&
#'     requireNamespace('ggthemes', quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
#'     ggplot2::geom_point() +
#'     epi_plot_theme_imss()
#' }
#' @export
epi_plot_theme_imss <- function(base_size = 13, base_family = "Times") {
  pkgs <- c("scales", "grid", "ggplot2", "ggthemes")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Packages missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.2),
        hjust = 0.5
      ),
      panel.background = ggplot2::element_rect(colour = NA),
      plot.background = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_rect(colour = NA),
      axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
      axis.title.x = ggplot2::element_text(vjust = -0.2),
      axis.text = ggplot2::element_text(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}
