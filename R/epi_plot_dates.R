#' Plot distributions of date variables
#'
#' @description
#' `epi_plot_dates()` creates a histogram, boxplot or line plot for a vector of
#' dates. It acts as a small wrapper around `ggplot2` with the package's default
#' theme.
#'
#' @param x A vector containing dates or values coercible to `Date`.
#' @param type Type of plot to produce. One of `"hist"`, `"box"`, or `"line"`.
#' @param ... Additional arguments passed to the underlying `ggplot2` geom.
#'
#' @return A `ggplot` object representing the requested plot.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' set.seed(123)
#' sample_dates <- as.Date("2020-01-01") + sample(0:100, 50, replace = TRUE)
#' epi_plot_dates(sample_dates, type = "hist")
#' epi_plot_dates(sample_dates, type = "box")
#' epi_plot_dates(sample_dates, type = "line")
#' }
#'
#' @export
epi_plot_dates <- function(x, type = c("hist", "box", "line"), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package ggplot2 needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (!requireNamespace("ggthemes", quietly = TRUE)) {
    stop(
      "Package ggthemes needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  type <- match.arg(type)
  if (any(is.na(x))) {
    stop("Input contains missing (NA) values. Please remove or impute them before plotting.")
  }
  date_vec <- as.Date(x)
  if (any(is.na(date_vec))) {
    stop("Input contains values that cannot be converted to valid dates.")
  }
  df <- data.frame(date = date_vec)
  if (type == "hist") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$date)) +
      ggplot2::geom_histogram(...) +
      epi_plot_theme_2() +
      ggplot2::labs(x = NULL, y = "Count")
  } else if (type == "box") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(1), y = .data$date)) +
      ggplot2::geom_boxplot(...) +
      epi_plot_theme_2() +
      ggplot2::labs(x = NULL, y = "Date")
  } else {
    df <- df[order(df$date), , drop = FALSE]
    df$value <- seq_along(df$date)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$value)) +
      ggplot2::geom_point(...) +
      epi_plot_theme_2() +
      ggplot2::labs(x = NULL, y = "Index")
  }
  p
}
