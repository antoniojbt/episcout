% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi_plot_themes.R
\name{epi_plot_theme_1}
\alias{epi_plot_theme_1}
\alias{epi_plot_theme_2}
\alias{scale_fill_epi_plot_theme_2}
\alias{scale_colour_epi_plot_theme_2}
\title{ggthemes for episcout}
\usage{
epi_plot_theme_1(base_size = 11, base_family = "Times")

epi_plot_theme_2(
  base_size = 13,
  base_family = "Times",
  font_size_x = NULL,
  font_size_y = NULL
)

scale_fill_epi_plot_theme_2(...)

scale_colour_epi_plot_theme_2(...)
}
\arguments{
\item{base_size}{passed to element_text() for the font size. Default is 11 for
epi_plot_theme_1 and 13 for epi_plot_theme_2}

\item{base_family}{passed to theme() for font family. Default is Times.}

\item{font_size_x}{Font size for x axis tick labels. Default NULL uses
\code{base_size} - 1.}

\item{font_size_y}{Font size for y axis tick labels. Default NULL uses
\code{base_size} - 1.}

\item{...}{additional arguments passed to ggplot2::discrete_scale() for
scale_colour_epi_plot_theme_1 and scale_colour_epi_plot_theme_2}
}
\value{
None, these functions are called from plotting functions.
}
\description{
Themes for ggplot2 plots for episcout epi_plot_XXX functions.
Most options are hard-coded, if you don't like the themes it is easier to create
one with your preferences or modify directly in a ggplot2 call.
}
\note{
For examples see eg \code{\link{epi_plot_heatmap_triangle}} and others.
If creating your own themes see for instance:
\href{https://github.com/jrnold/ggthemes}{ggthemes},
\href{http://sape.inf.usi.ch/quick-reference/ggplot2/themes}{ggplot2 themes}.
Some colour codes:
\href{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/}{Colors ggplot2},
\href{http://sape.inf.usi.ch/quick-reference/ggplot2/colour}{ggplot2 Quick Reference}
}
\author{
Antonio Berlanga-Taylor \if{html}{\out{<\url{https://github.com/AntonioJBT/episcout}>}}
}
