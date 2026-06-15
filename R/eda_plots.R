#' Profile plots using an EDA specification
#'
#' Create one basic ggplot object for each variable listed in a
#' specification-first EDA data dictionary. Plot objects are returned without
#' being printed.
#'
#' @param data A data frame containing observed data.
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A named list of ggplot objects, one per specified variable.
#'
#' @export
profile_plots <- function(data, spec) {
  if (!is.data.frame(data)) {
    stop("EDA data must be a data frame.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required for profile_plots().", call. = FALSE)
  }

  spec <- eda_spec(spec)
  missing_vars <- setdiff(spec$name, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "EDA data is missing specified variables: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  plots <- lapply(seq_len(nrow(spec)), function(i) {
    profile_plot_variable(data, spec$name[[i]], spec$type[[i]], spec$label[[i]])
  })
  names(plots) <- spec$name
  plots
}

profile_plot_variable <- function(data, name, type, label) {
  plot_data <- data.frame(value = data[[name]])
  axis_label <- if (!is.na(label) && nzchar(label)) label else name

  if (type %in% c("numeric", "integer")) {
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$value)) +
      ggplot2::geom_histogram(bins = 30, na.rm = TRUE) +
      ggplot2::labs(x = axis_label, y = "Count", title = axis_label) +
      ggplot2::theme_minimal()
  } else if (type %in% c("categorical", "binary", "text")) {
    plot_data$value <- as.factor(plot_data$value)
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$value)) +
      ggplot2::geom_bar(na.rm = TRUE) +
      ggplot2::labs(x = axis_label, y = "Count", title = axis_label) +
      ggplot2::theme_minimal()
  } else if (type %in% c("date", "datetime")) {
    plot_data$value <- as.POSIXct(plot_data$value)
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$value)) +
      ggplot2::geom_histogram(bins = 30, na.rm = TRUE) +
      ggplot2::labs(x = axis_label, y = "Count", title = axis_label) +
      ggplot2::theme_minimal()
  } else {
    stop("Unsupported EDA plot type: ", type, call. = FALSE)
  }
}
