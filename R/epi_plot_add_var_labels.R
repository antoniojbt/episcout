#' Add variable labels to ggplot axes
#'
#' This function wraps \code{ggplot2::labs()} to dynamically set
#' axis labels based on metadata provided in a lookup table. It is
#' designed for use as a ggplot layer and can be added with \code{+}.
#'
#' @param var_y Character string. The name of the variable to use for the y-axis.
#' @param var_x Character string. The name of the variable to use for the x-axis (optional).
#' @param var_lookup Named character vector. A lookup table where names are
#'   variable codes (e.g., column names) and values are human-readable
#'   descriptions (e.g., "Number of usable beds (x920935)").
#'
#' @return A \code{ggplot2} layer that sets axis labels using metadata.
#'
#' @details
#' This function checks whether \code{var_y} and \code{var_x} are present
#' in \code{var_lookup}. If so, it uses their corresponding descriptions as axis labels.
#' Otherwise, no labels are added for missing variables.
#'
#' @examples
#' library(ggplot2)
#'
#' # Example metadata
#' var_labels <- c(
#'   beds = "Number of usable beds (beds)",
#'   patients = "Total patients treated (patients)"
#' )
#'
#' # Basic ggplot with dynamic labels
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   epi_plot_add_var_labels(var_y = "mpg", var_x = "wt", var_lookup = var_labels)
#'
#' @export
epi_plot_add_var_labels <- function(var_y = NULL, var_x = NULL, var_lookup = NULL) {
    ggplot2::labs(
        y = if (!is.null(var_y) && var_y %in% names(var_lookup)) var_lookup[[var_y]] else NULL,
        x = if (!is.null(var_x) && var_x %in% names(var_lookup)) var_lookup[[var_x]] else NULL
    )
}
