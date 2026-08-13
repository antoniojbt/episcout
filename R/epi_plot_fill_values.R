# Internal helpers for validating discrete categorical fill mappings.

epi_plot_fill_levels <- function(x) {
  if (is.factor(x)) {
    levels(x)
  } else {
    unique(as.character(x[!is.na(x)]))
  }
}

epi_plot_fill_mapping <- function(fill_values, levels, argument = "fill_values") {
  if (is.null(fill_values)) {
    return(NULL)
  }
  if (!is.character(fill_values) || !is.atomic(fill_values) || length(fill_values) == 0L || anyNA(fill_values)) {
    stop(sprintf("%s must be a non-missing character vector of colours.", argument), call. = FALSE)
  }

  colour_is_valid <- vapply(fill_values, function(value) {
    !inherits(try(grDevices::col2rgb(value), silent = TRUE), "try-error")
  }, logical(1L))
  if (any(!colour_is_valid)) {
    stop(sprintf("%s contains an invalid colour.", argument), call. = FALSE)
  }

  mapping_names <- names(fill_values)
  if (is.null(mapping_names)) {
    if (length(fill_values) < length(levels)) {
      stop(sprintf("%s needs one colour for each displayed category.", argument), call. = FALSE)
    }
    return(stats::setNames(fill_values[seq_along(levels)], levels))
  }

  if (any(!nzchar(mapping_names)) || anyDuplicated(mapping_names)) {
    stop(sprintf("%s names must be non-empty and unique.", argument), call. = FALSE)
  }
  missing_levels <- setdiff(levels, mapping_names)
  extra_levels <- setdiff(mapping_names, levels)
  if (length(missing_levels) > 0L || length(extra_levels) > 0L) {
    stop(sprintf("%s names must exactly match the displayed categories.", argument), call. = FALSE)
  }
  fill_values[levels]
}
