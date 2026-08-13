# Internal validation and application of caller-owned EDA plot styling.

eda_plot_style_options <- function(plot_style = NULL, plot_style_id = NULL) {
  if (!is.null(plot_style) && !is.function(plot_style)) {
    stop("plot_style must be NULL or a function accepting plot and context.", call. = FALSE)
  }
  if (!is.null(plot_style_id) && (!is.character(plot_style_id) || length(plot_style_id) != 1L || is.na(plot_style_id) || !nzchar(trimws(plot_style_id)))) {
    stop("plot_style_id must be NULL or one non-empty character identifier.", call. = FALSE)
  }
  if (is.null(plot_style) && !is.null(plot_style_id)) {
    stop("plot_style_id requires plot_style.", call. = FALSE)
  }
  list(plot_style = plot_style, plot_style_id = plot_style_id)
}

eda_plot_style_context <- function(entry) {
  list(
    name = entry$name,
    label = entry$label,
    type = entry$type,
    plot_type = entry$plot_type,
    n_total = entry$n_total,
    n_missing = entry$n_missing,
    n_plotted = entry$n_plotted,
    n_excluded_non_finite = entry$n_excluded_non_finite
  )
}

eda_apply_plot_style <- function(plot, entry, plot_style = NULL) {
  if (is.null(plot_style)) {
    return(plot)
  }
  styled <- tryCatch(
    plot_style(plot, eda_plot_style_context(entry)),
    error = function(error) {
      stop("plot_style failed while styling an EDA plot.", call. = FALSE)
    }
  )
  if (!inherits(styled, "ggplot")) {
    stop("plot_style must return one ggplot object.", call. = FALSE)
  }
  styled
}
