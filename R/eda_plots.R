#' Profile plots using an EDA specification
#'
#' Create one basic ggplot object for each variable listed in a specification-first EDA data dictionary. Standard missing values and specification `missing_codes` are excluded before plotting or temporal conversion. Plot objects are returned without being printed.
#'
#' @param data A data frame containing observed data.
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A named list of ggplot objects, one per specified variable.
#'
#' @export
epi_eda_profile_plots <- function(data, spec) {
  if (!is.data.frame(data)) {
    stop("EDA data must be a data frame.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required for epi_eda_profile_plots().", call. = FALSE)
  }

  spec <- epi_eda_spec(spec)
  missing_vars <- setdiff(spec$name, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "EDA data is missing specified variables: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  plots <- lapply(seq_len(nrow(spec)), function(i) {
    profile_plot_variable(
      data,
      spec$name[[i]],
      spec$type[[i]],
      spec$label[[i]],
      eda_missing_codes(spec, spec$name[[i]])
    )
  })
  names(plots) <- spec$name
  plots
}

profile_plot_variable <- function(data, name, type, label, missing_codes = character()) {
  values <- data[[name]]
  missing <- eda_missing_mask(values, missing_codes)
  plot_data <- data.frame(value = values[!missing])
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
    plot_data$value <- profile_plot_temporal_values(plot_data$value, type)
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$value)) +
      ggplot2::geom_histogram(bins = 30, na.rm = TRUE) +
      ggplot2::labs(x = axis_label, y = "Count", title = axis_label) +
      ggplot2::theme_minimal()
  } else {
    stop("Unsupported EDA plot type: ", type, call. = FALSE)
  }
}

profile_plot_temporal_values <- function(values, type) {
  if (type == "date") {
    if (inherits(values, "IDate")) {
      parsed <- as.Date(values)
    } else if (inherits(values, "Date")) {
      parsed <- values
    } else if (is.character(values)) {
      parsed <- suppressWarnings(as.Date(values, format = "%Y-%m-%d"))
    } else {
      stop("Observed class is incompatible with declared date type.", call. = FALSE)
    }
    if (anyNA(parsed)) {
      stop("Temporal variable contains invalid non-missing values.", call. = FALSE)
    }
    return(as.POSIXct(parsed))
  }

  if (inherits(values, c("POSIXct", "POSIXlt"))) {
    parsed <- as.POSIXct(values)
  } else if (is.character(values)) {
    parsed <- summary_parse_datetime_chr(values)
  } else {
    stop("Observed class is incompatible with declared datetime type.", call. = FALSE)
  }
  if (anyNA(parsed)) {
    stop("Temporal variable contains invalid non-missing values.", call. = FALSE)
  }
  parsed
}
