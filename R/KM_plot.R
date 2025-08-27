#' Plot Kaplan-Meier survival curves
#'
#' Convert a `survival::survfit` object into a tidy data frame and plot the
#' survival curves with `ggplot2`.
#'
#' @param survfit_obj A `survival::survfit` object.
#'
#' @return A `ggplot2` object. The data used to build the plot is attached as
#'   an attribute named `surv_data`.
#' @export
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE) &&
#'   requireNamespace("ggplot2", quietly = TRUE)) {
#'   fit <- survival::survfit(survival::Surv(time, status) ~ sex,
#'     data = survival::lung
#'   )
#'   epi_plot_km(fit)
#' }
epi_plot_km <- function(survfit_obj) {
  if (!inherits(survfit_obj, "survfit")) stop("Input must be a survfit object")
  summary_surv <- summary(survfit_obj)
  surv_df <- tibble::tibble(
    time = summary_surv$time,
    surv = summary_surv$surv,
    n_risk = summary_surv$n.risk,
    n_event = summary_surv$n.event,
    n_censor = summary_surv$n.censor,
    strata = if (is.null(summary_surv$strata)) {
      "All"
    } else {
      as.character(summary_surv$strata)
    }
  )

  p <- ggplot2::ggplot(
    surv_df,
    ggplot2::aes(
      x = .data$time,
      y = .data$surv,
      colour = .data$strata
    )
  ) +
    ggplot2::geom_step() +
    ggplot2::labs(
      x = "Time",
      y = "Survival probability",
      colour = "Strata"
    )

  attr(p, "surv_data") <- surv_df
  p
}
