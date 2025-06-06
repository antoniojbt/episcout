#' @title ggthemes for episcout
#'
#' @description Themes for ggplot2 plots for episcout epi_plot_XXX functions.
#' Most options are hard-coded, if you don't like the themes it is easier to create
#' one with your preferences or modify directly in a ggplot2 call.
#'
#' @param base_size passed to element_text() for the font size. Default is 11 for
#' epi_plot_theme_1 and 13 for epi_plot_theme_2
#' @param base_family passed to theme() for font family. Default is Times.
#' @param font_size_x Font size for x axis tick labels. Default NULL uses
#'   \code{base_size} - 1.
#' @param font_size_y Font size for y axis tick labels. Default NULL uses
#'   \code{base_size} - 1.
#' @param ... additional arguments passed to ggplot2::discrete_scale() for
#' scale_colour_epi_plot_theme_1 and scale_colour_epi_plot_theme_2
#'
#' @return None, these functions are called from plotting functions.
#'
#' @note For examples see eg \code{\link{epi_plot_heatmap_triangle}} and others.
#' If creating your own themes see for instance:
#' \href{https://github.com/jrnold/ggthemes}{ggthemes},
#' \href{http://sape.inf.usi.ch/quick-reference/ggplot2/themes}{ggplot2 themes}.
#' Some colour codes:
#' \href{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/}{Colors ggplot2},
#' \href{http://sape.inf.usi.ch/quick-reference/ggplot2/colour}{ggplot2 Quick Reference}
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
# @keywords internal
#' @export

# Note that A4 paper measures 210x297 millimeters or 8.27x11.69 inches

epi_plot_theme_1 <- function(base_size = 11,
                             base_family = "Times"
                             ) {
  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop('Package ggplot2 needed for this function to work. Please install it.',
         call. = FALSE)
  }
  # The following is modified from:
  # https://stackoverflow.com/questions/31404433/is-there-an-elegant-way-of-having-uniform-font-size-for-the-whole-plot-in-ggplot
  normal_text <- ggplot2::element_text(size = as.numeric(base_size),
                              colour = "black",
                              face = "plain"
                              )
  large_text <- ggplot2::element_text(size = as.numeric(base_size + 1),
                             colour = "black",
                             face = "plain"
                             )
  bold_text <- ggplot2::element_text(size = as.numeric(base_size + 1),
                            colour = "black",
                            face = "bold"
                            )
  axis_text <- ggplot2::element_text(size = as.numeric(base_size - 1),
                            colour = "black",
                            face = "plain"
                            )
  ggplot2::theme_classic(base_size = base_size,
                base_family = base_family
                ) +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          text = normal_text,
          plot.title = bold_text,
          axis.title = large_text,
          axis.text = axis_text,
          legend.title = bold_text,
          legend.text = normal_text,
          plot.margin = ggplot2::unit(c(3, 5, 2, 2),
                             "mm") #t, r, b, l
          # plot.margin = grid::unit(c(1,1,1,1), "mm")
    )
  }

#' @rdname epi_plot_theme_1

epi_plot_theme_2 <- function(base_size = 13,
                             base_family = 'Times',
                             font_size_x = NULL,
                             font_size_y = NULL
                             ) {
# Use this instead or library or require inside functions:
 if (!requireNamespace('scales', quietly = TRUE)) {
   stop('Package scales needed for this function to work. Please install it.',
        call. = FALSE)
 }
 if (!requireNamespace('grid', quietly = TRUE)) {
   stop('Package grid needed for this function to work. Please install it.',
          call. = FALSE)
 }
 if (!requireNamespace('ggplot2', quietly = TRUE)) {
       stop('Package ggplot2 needed for this function to work. Please install it.',
            call. = FALSE)
   }
  if (!requireNamespace('ggthemes', quietly = TRUE)) {
        stop('Package ggthemes needed for this function to work. Please install it.',
             call. = FALSE)
  }
  # The following is modified from:
  # https://rpubs.com/Koundy/71792
  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                      size = ggplot2::rel(1.2),
                                      hjust = 0.5),
            text = ggplot2::element_text(),
            panel.background = ggplot2::element_rect(colour = NA),
            plot.background = ggplot2::element_rect(colour = NA),
            panel.border = ggplot2::element_rect(colour = NA),
            axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
            axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
            axis.title.x = ggplot2::element_text(vjust = -0.2),
            axis.text = ggplot2::element_text(),
            axis.line = ggplot2::element_line(colour = "black"),
            axis.ticks = ggplot2::element_line(),
            axis.text.x = ggplot2::element_text(size = font_size_x),
            axis.text.y = ggplot2::element_text(size = font_size_y),
            # panel.grid.major = element_line(colour = "#f0f0f0"),
                                              # this is like grey90 roughly
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            # legend.key = element_rect(fill = "white", colour = " light grey"),
            legend.key = ggplot2::element_rect(colour = NA),
            # legend.position = "bottom",
            # legend.direction = "horizontal",
            legend.key.size = ggplot2::unit(0.5, "cm"),
            # legend.key.width = unit(0.2, "cm"),
            # legend.margin = margin(0, 0, 0, 0),
            # legend.title = element_text(face = "italic"),
            # plot.margin = unit(c(10, 5, 5, 5),"mm"),
            # plot.margin = unit(c(3, 5, 2, 2), "mm"), #t, r, b, l
            strip.background = ggplot2::element_rect(colour = "#f0f0f0",
           	                                        fill = "#f0f0f0"),
            strip.text = ggplot2::element_text(face = "bold")
            )
    )
  }

#' @rdname epi_plot_theme_1

scale_fill_epi_plot_theme_2 <- function(...) {
  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop('Package ggplot2 needed for this function to work. Please install it.',
         call. = FALSE)
    }
  if (!requireNamespace('scales', quietly = TRUE)) {
    stop('Package scales needed for this function to work. Please install it.',
         call. = FALSE)
  }
  ggplot2::discrete_scale("fill", "epi_plot_theme_2",
                 scales::manual_pal(values = c("#386cb0",
                                       "#fdb462",
                                       "#7fc97f",
                                       "#ef3b2c",
                                       "#662506",
                                       "#a6cee3",
                                       "#fb9a99",
                                       "#984ea3",
                                       "#ffff33")),
                 ...)

}

#' @rdname epi_plot_theme_1

scale_colour_epi_plot_theme_2 <- function(...) {
  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop('Package ggplot2 needed for this function to work. Please install it.',
         call. = FALSE)
    }
  if (!requireNamespace('scales', quietly = TRUE)) {
    stop('Package scales needed for this function to work. Please install it.',
         call. = FALSE)
  }
  ggplot2::discrete_scale("colour", "epi_plot_theme_2",
                 scales::manual_pal(values = c("#386cb0",
                                       "#fdb462",
                                       "#7fc97f",
                                       "#ef3b2c",
                                       "#662506",
                                       "#a6cee3",
                                       "#fb9a99",
                                       "#984ea3",
                                       "#ffff33")),
                 ...)

}
