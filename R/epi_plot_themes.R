#' @title ggthemes for episcout
#'
#' @description Themes for ggplot2 plots for episcout epi_plot_XXX functions.
#' Most options are hard-coded, if you don't like the themes it is easier to create
#' one with your preferences or modify directly in a ggplot2 call.
#'
#' @return None, these functions are called from plotting functions.
#'
#' @note For examples see eg \code{\link{epi_plot_heatmap_triangle}} and others.
#' If creating your own themes see for instance:
#' \url{https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html}{ggthemes},
#' \url{http://sape.inf.usi.ch/quick-reference/ggplot2/themes}{ggplot2 themes}.
#' Some colour codes:
#' \url{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/}{Colors (ggplot2)},
#' \url{http://sape.inf.usi.ch/quick-reference/ggplot2/colour}{ggplot2 Quick Reference}
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @keywords internal

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
  normal_text <- element_text(size = as.numeric(base_size),
                              colour = "black",
                              face = "plain"
                              )
  large_text <- element_text(size = as.numeric(base_size + 1),
                             colour = "black",
                             face = "plain"
                             )
  bold_text <- element_text(size = as.numeric(base_size + 1),
                            colour = "black",
                            face = "bold"
                            )
  axis_text <- element_text(size = as.numeric(base_size - 1),
                            colour = "black",
                            face = "plain"
                            )
  theme_classic(base_size = base_size,
                base_family = base_family
                ) +
    theme(legend.key = element_blank(),
          strip.background = element_blank(),
          text = normal_text,
          plot.title = bold_text,
          axis.title = large_text,
          axis.text = axis_text,
          legend.title = bold_text,
          legend.text = normal_text,
          plot.margin = unit(c(3, 5, 2, 2),
                             "mm") #t, r, b, l
          # plot.margin = grid::unit(c(1,1,1,1), "mm")
    )
  }

epi_plot_theme_2 <- function(base_size = 13,
                             base_family = 'Times',
                             ...
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
  if (!requireNamespace('ggthemes', quietly = TRUE)) {
        stop('Package ggthemes needed for this function to work. Please install it.',
             call. = FALSE)
  }
  # The following is modified from:
  # https://rpubs.com/Koundy/71792
  (theme_foundation(base_size = base_size, base_family = base_family) +
      theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2),
                                      hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold", size = rel(1)),
             axis.title.y = element_text(angle = 90, vjust = 2),
             axis.title.x = element_text(vjust = -0.2),
             axis.text = element_text(),
             axis.line = element_line(colour = "black"),
             axis.ticks = element_line(),
            # panel.grid.major = element_line(colour = "#f0f0f0"), # this is like grey90 roughly
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             # legend.key = element_rect(fill = "white", colour = " light grey"),
             legend.key = element_rect(colour = NA),
             # legend.position = "bottom",
             # legend.direction = "horizontal",
             legend.key.size = unit(0.5, "cm"),
             # legend.key.width = unit(0.2, "cm"),
             # legend.margin = margin(0, 0, 0, 0),
             # legend.title = element_text(face = "italic"),
             # plot.margin = unit(c(10, 5, 5, 5),"mm"),
             # plot.margin = unit(c(3, 5, 2, 2), "mm"), #t, r, b, l
             strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
             strip.text = element_text(face = "bold")
            )
    )
  }

scale_fill_epi_plot_theme_2 <- function(...) {
  if (!requireNamespace('scales', quietly = TRUE)) {
    stop('Package scales needed for this function to work. Please install it.',
         call. = FALSE)
  }
  discrete_scale("fill", "epi_plot_theme_2",
                 manual_pal(values = c("#386cb0",
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

scale_colour_epi_plot_theme_2 <- function(...) {
  if (!requireNamespace('scales', quietly = TRUE)) {
    stop('Package scales needed for this function to work. Please install it.',
         call. = FALSE)
  }
  discrete_scale("colour", "epi_plot_theme_2",
                 manual_pal(values = c("#386cb0",
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
