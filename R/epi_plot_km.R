#' @title Convert survfit object to data frame
#'
#' @description
#' `epi_survfit_to_df()` converts a `survival::survfit` object into a data frame
#' suitable for plotting. Strata information is expanded into individual
#' columns when present.
#'
#' @param survfit_obj A `survival::survfit` object.
#'
#' @return A data frame containing survival estimates and any strata variables.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#' epi_survfit_to_df(fit)
#' }
#'
#' @export
epi_survfit_to_df <- function(survfit_obj) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop(
      "Package survival needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  res <- data.frame(
    time = survfit_obj$time,
    n_risk = survfit_obj$n.risk,
    n_event = survfit_obj$n.event,
    n_censor = survfit_obj$n.censor,
    surv = survfit_obj$surv,
    upper = survfit_obj$upper,
    lower = survfit_obj$lower
  )
  if (!is.null(survfit_obj$strata)) {
    strata_vec <- rep(names(survfit_obj$strata), times = survfit_obj$strata)
    res$strata <- strata_vec
    strata_mat <- do.call(rbind, lapply(strata_vec, function(s) {
      parts <- strsplit(s, ",")[[1]]
      vals <- sapply(parts, function(p) sub(".*=", "", p))
      names(vals) <- sapply(parts, function(p) sub("=.*", "", p))
      vals
    }))
    res <- data.frame(res, strata_mat, stringsAsFactors = FALSE)
  }
  res
}

#' @title Plot Kaplan-Meier survival curves
#'
#' @description
#' `epi_plot_km()` creates Kaplan-Meier plots using ggplot2 from a
#' `survival::survfit` object.
#'
#' @param survfit_obj A `survival::survfit` object.
#' @param group_var Optional name of the column to use for colour.
#' Defaults to the first strata variable if present.
#' @param facet_var Optional name of the column to facet by.
#' Defaults to the second strata variable if present.
#' @param save_path Optional file path to save the plot via `ggplot2::ggsave`.
#' If `NULL`, the plot is not saved.
#' @param ... Further arguments passed to `ggplot2::ggsave` when saving.
#'
#' @return A ggplot object of the Kaplan-Meier curves.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#' epi_plot_km(fit, save_path = "km.png")
#' }
#'
#' @export
epi_plot_km <- function(survfit_obj,
                        group_var = NULL,
                        facet_var = NULL,
                        save_path = NULL,
                        ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package ggplot2 needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  df <- epi_survfit_to_df(survfit_obj)
  default_cols <- c(
    "time",
    "n_risk",
    "n_event",
    "n_censor",
    "surv",
    "upper",
    "lower",
    "strata"
  )
  other_cols <- setdiff(names(df), default_cols)
  if (is.null(group_var) && length(other_cols) >= 1) {
    group_var <- other_cols[1]
  }
  if (is.null(facet_var) && length(other_cols) >= 2) {
    facet_var <- other_cols[2]
  }
  if (is.null(group_var)) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$surv))
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = .data$time,
      y = .data$surv,
      colour = .data[[group_var]]
    ))
  }
  p <- p +
    ggplot2::geom_step() +
    ggplot2::labs(x = "Time", y = "Survival Probability") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), scales = "free_y")
  }
  if (!is.null(save_path)) {
    ggplot2::ggsave(filename = save_path, plot = p, ...)
  }
  p
}
