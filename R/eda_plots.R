#' Profile plots using an EDA specification
#'
#' Create one compact aggregate plot for each variable listed in a
#' specification-first EDA data dictionary. Numeric and temporal values use
#' fixed 30-bin distributions, categorical values use complete frequencies,
#' and text uses character lengths rather than observed strings.
#'
#' @param data A data frame or an [epi_eda_postgres_source()] containing
#'   observed data.
#' @param spec An EDA specification data frame or CSV path.
#' @param plot_style Optional function receiving a completed ggplot object and
#'   compact non-row-level context. It must return one ggplot object.
#'
#' @return A named list containing one ggplot object per specification row, in
#'   specification order.
#'
#' @export
epi_eda_profile_plots <- function(data, spec, plot_style = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required for epi_eda_profile_plots().", call. = FALSE)
  }
  style_options <- eda_plot_style_options(plot_style)
  spec <- epi_eda_spec(spec)
  if (inherits(data, "epi_eda_postgres_source")) {
    prepared <- eda_postgres_transaction(data, {
      missing_vars <- setdiff(spec$name, data$columns$name)
      if (length(missing_vars) > 0L) {
        stop("EDA data is missing specified variables: ", paste(missing_vars, collapse = ", "), call. = FALSE)
      }
      summaries <- eda_postgres_summaries_inside(data, spec)
      eda_postgres_plot_data_inside(data, spec, summaries, 20L)
    })
    return(eda_render_plot_entries(prepared$entries, style_options$plot_style))
  }
  if (!is.data.frame(data)) {
    stop("EDA data must be a data frame or an epi_eda_postgres_source.", call. = FALSE)
  }
  missing_vars <- setdiff(spec$name, names(data))
  if (length(missing_vars) > 0L) {
    stop("EDA data is missing specified variables: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
  prepared <- eda_data_frame_plot_data(data, spec, max_plot_levels = 20L)
  eda_render_plot_entries(prepared$entries, style_options$plot_style)
}

eda_data_frame_plot_data <- function(data, spec, max_plot_levels = 20L) {
  entries <- lapply(seq_len(nrow(spec)), function(index) {
    name <- spec$name[[index]]
    type <- spec$type[[index]]
    label <- if (!is.na(spec$label[[index]]) && nzchar(spec$label[[index]])) spec$label[[index]] else name
    values <- data[[name]]
    missing <- eda_missing_mask(values, eda_missing_codes(spec, name))
    observed <- values[!missing]
    if (type %in% c("numeric", "integer")) {
      if (!is.numeric(observed)) stop("Observed class is incompatible with declared numeric type.", call. = FALSE)
      finite <- as.numeric(observed[is.finite(observed)])
      compact <- eda_compact_histogram(finite)
      return(eda_plot_entry(name, label, type, "histogram", compact, length(values), sum(missing), length(finite), sum(!is.finite(observed))))
    }
    if (type %in% c("categorical", "binary")) {
      levels <- if ("levels" %in% names(spec)) eda_spec_levels(spec$levels[[index]]) else character()
      frequencies <- summary_categorical_core(values, eda_missing_codes(spec, name), if (length(levels) > 0L) levels else NULL)
      display <- eda_cat_display_frequency(
        frequencies, name, label, type, length(values), sum(missing)
      )
      compact <- eda_collapse_frequencies(display, max_plot_levels)
      entry <- eda_plot_entry(name, label, type, "frequency", compact, length(values), sum(missing), length(observed), 0L)
      entry$n_displayed_levels <- nrow(compact)
      entry$n_collapsed_levels <- max(0L, nrow(frequencies) - min(nrow(frequencies), max_plot_levels))
      return(entry)
    }
    if (type == "text") {
      if (!is.character(observed) && !is.factor(observed)) stop("Observed class is incompatible with declared text type.", call. = FALSE)
      lengths <- nchar(as.character(observed), type = "chars")
      return(eda_plot_entry(name, label, type, "text_length", eda_compact_histogram(lengths), length(values), sum(missing), length(lengths), 0L))
    }
    if (type %in% c("date", "datetime")) {
      parsed <- profile_plot_temporal_values(observed, type)
      numeric_values <- as.numeric(parsed)
      return(eda_plot_entry(name, label, type, "temporal", eda_compact_histogram(numeric_values), length(values), sum(missing), length(numeric_values), 0L))
    }
    stop("Unsupported EDA plot type: ", type, call. = FALSE)
  })
  names(entries) <- spec$name
  list(entries = entries, inventory = eda_plot_inventory(entries))
}

eda_plot_entry <- function(name,
                           label,
                           type,
                           plot_type,
                           data,
                           n_total,
                           n_missing,
                           n_plotted,
                           n_excluded_non_finite,
                           status = "created",
                           reason = NA_character_) {
  list(
    name = name, label = label, type = type, plot_type = plot_type, data = data,
    n_total = as.integer(n_total), n_missing = as.integer(n_missing),
    n_plotted = as.integer(n_plotted),
    n_excluded_non_finite = as.integer(n_excluded_non_finite),
    n_displayed_levels = NA_integer_, n_collapsed_levels = NA_integer_,
    status = status, reason = reason
  )
}

eda_compact_histogram <- function(values, bins = 30L) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    breaks <- seq(0, 1, length.out = bins + 1L)
    counts <- integer(bins)
  } else if (min(values) == max(values)) {
    delta <- max(0.5, abs(values[[1]]) * 0.01)
    breaks <- seq(values[[1]] - delta, values[[1]] + delta, length.out = bins + 1L)
    counts <- integer(bins)
    counts[[as.integer(ceiling(bins / 2))]] <- length(values)
  } else {
    breaks <- seq(min(values), max(values), length.out = bins + 1L)
    bin <- findInterval(values, breaks, all.inside = TRUE)
    counts <- tabulate(bin, nbins = bins)
  }
  data.frame(
    bin = seq_len(bins), lower = breaks[-length(breaks)], upper = breaks[-1L],
    midpoint = (breaks[-length(breaks)] + breaks[-1L]) / 2,
    count = as.integer(counts), stringsAsFactors = FALSE
  )
}

eda_histogram_from_counts <- function(minimum, maximum, counts, bins = 30L) {
  if (is.na(minimum) || is.na(maximum)) {
    return(eda_compact_histogram(numeric(), bins))
  }
  base <- if (minimum == maximum) {
    eda_compact_histogram(minimum, bins)
  } else {
    breaks <- seq(minimum, maximum, length.out = bins + 1L)
    data.frame(
      bin = seq_len(bins), lower = breaks[-length(breaks)], upper = breaks[-1L],
      midpoint = (breaks[-length(breaks)] + breaks[-1L]) / 2,
      count = integer(bins), stringsAsFactors = FALSE
    )
  }
  if (nrow(counts) > 0L) base$count[match(counts$bin, base$bin)] <- counts$count
  base
}

eda_collapse_frequencies <- function(frequencies, max_plot_levels) {
  if (nrow(frequencies) == 0L) {
    empty <- eda_empty_categorical_display()
    return(cbind(
      data.frame(
        level = character(), count = integer(), display_order = integer(),
        remainder = logical(), stringsAsFactors = FALSE
      ),
      empty[setdiff(names(empty), "level")]
    ))
  }
  required <- eda_categorical_display_names()
  if (!all(required %in% names(frequencies)) ||
    any(frequencies$is_missing_level) ||
    !eda_cat_counts_valid(frequencies$numerator)) {
    stop("Categorical plot display rows are incompatible.", call. = FALSE)
  }
  frequencies$canonical_order <- seq_len(nrow(frequencies))
  ordered <- frequencies[order(-frequencies$numerator, frequencies$canonical_order), , drop = FALSE]
  keep_n <- min(nrow(ordered), max_plot_levels)
  kept <- ordered[seq_len(keep_n), , drop = FALSE]
  out <- data.frame(
    level = kept$level, count = as.integer(kept$numerator), display_order = seq_len(keep_n),
    remainder = FALSE, stringsAsFactors = FALSE
  )
  out <- cbind(
    out,
    kept[, setdiff(required, "level"), drop = FALSE]
  )
  if (nrow(ordered) > keep_n) {
    collapsed <- nrow(ordered) - keep_n
    remainder_rows <- ordered[(keep_n + 1L):nrow(ordered), , drop = FALSE]
    if (length(unique(remainder_rows$denominator)) != 1L ||
      length(unique(remainder_rows$percentage_basis)) != 1L) {
      stop("Collapsed categorical plot denominators did not reconcile.", call. = FALSE)
    }
    remainder <- remainder_rows[1L, required, drop = FALSE]
    remainder$level <- paste0("Other (", collapsed, " levels)")
    remainder$level_order <- NA_integer_
    remainder$numerator <- as.integer(sum(as.numeric(remainder_rows$numerator)))
    remainder$proportion <- summary_safe_proportion(
      remainder$numerator[[1]], remainder$denominator[[1]]
    )
    remainder$is_missing_level <- FALSE
    out <- rbind(out, cbind(
      data.frame(
        level = remainder$level,
        count = remainder$numerator,
        display_order = nrow(out) + 1L,
        remainder = TRUE,
        stringsAsFactors = FALSE
      ),
      remainder[, setdiff(required, "level"), drop = FALSE]
    ))
  }
  row.names(out) <- NULL
  out
}

eda_frequency_companion_names <- function() {
  c(
    "level", "count", "display_order", "remainder",
    setdiff(eda_categorical_display_names(), "level")
  )
}

eda_render_plot_entries <- function(entries, plot_style = NULL) {
  plots <- lapply(entries, function(entry) {
    if (is.null(entry$data)) {
      return(NULL)
    }
    eda_apply_plot_style(eda_render_compact_plot(entry), entry, plot_style)
  })
  names(plots) <- names(entries)
  plots
}

eda_render_compact_plot <- function(entry) {
  axis_label <- entry$label
  if (entry$plot_type == "frequency") {
    data <- entry$data
    data$level <- factor(data$level, levels = data$level)
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$level, y = .data$count))
    return(
      plot +
        ggplot2::geom_col() +
        ggplot2::labs(x = axis_label, y = "Count", title = axis_label) +
        ggplot2::theme_minimal()
    )
  }
  data <- entry$data
  if (entry$type == "date") {
    data$midpoint <- as.Date(data$midpoint, origin = "1970-01-01")
  } else if (entry$type == "datetime") {
    data$midpoint <- as.POSIXct(data$midpoint, origin = "1970-01-01", tz = "UTC")
  }
  ggplot2::ggplot(data, ggplot2::aes(x = .data$midpoint, y = .data$count)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = if (entry$plot_type == "text_length") paste0(axis_label, " character length") else axis_label, y = "Count", title = axis_label) +
    ggplot2::theme_minimal()
}

eda_plot_inventory <- function(entries) {
  rows <- lapply(seq_along(entries), function(index) {
    entry <- entries[[index]]
    data.frame(
      variable_index = as.integer(index), name = entry$name, type = entry$type,
      plot_type = entry$plot_type, n_total = entry$n_total,
      n_missing = entry$n_missing, n_plotted = entry$n_plotted,
      n_excluded_non_finite = entry$n_excluded_non_finite,
      n_displayed_levels = entry$n_displayed_levels,
      n_collapsed_levels = entry$n_collapsed_levels,
      status = entry$status, reason = entry$reason, path = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    return(data.frame(
      variable_index = integer(), name = character(), type = character(),
      plot_type = character(), n_total = integer(), n_missing = integer(),
      n_plotted = integer(), n_excluded_non_finite = integer(),
      n_displayed_levels = integer(), n_collapsed_levels = integer(),
      status = character(), reason = character(), path = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

profile_plot_temporal_values <- function(values, type) {
  if (type == "date") {
    if (inherits(values, "IDate")) parsed <- as.Date(values) else if (inherits(values, "Date")) parsed <- values else if (is.character(values)) parsed <- suppressWarnings(as.Date(values, format = "%Y-%m-%d")) else stop("Observed class is incompatible with declared date type.", call. = FALSE)
    if (anyNA(parsed)) stop("Temporal variable contains invalid non-missing values.", call. = FALSE)
    return(parsed)
  }
  if (inherits(values, c("POSIXct", "POSIXlt"))) parsed <- as.POSIXct(values) else if (is.character(values)) parsed <- summary_parse_datetime_chr(values) else stop("Observed class is incompatible with declared datetime type.", call. = FALSE)
  if (anyNA(parsed)) stop("Temporal variable contains invalid non-missing values.", call. = FALSE)
  parsed
}
