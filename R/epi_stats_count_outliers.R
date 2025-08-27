#' @title Count univariate outliers
#'
#' @description epi_stat_count_outliers() counts how many outliers a vector has
#' using a univariate approach. Returns the number of observations that are greater than
#' the cutoff. Outliers are detected with the Tukey method (above and below coef * IQR).
#'
#' @param num_vec Numeric vector to test.
#' @param coef Coefficient for outlier detection, default is 1.5
#' @param ... Other parameters that can be passed to boxplot.stats().
#'
#' @return Returns the number of observations above the cut-off specified.
#'
#' @note coef = 0 returns no outliers, see ?boxplot.stats
#' An alternative, not implemented, is to consider those eg > 5 * SD
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[grDevices]{boxplot.stats}}
#'
#' @examples
#' \dontrun{
#' n <- 1000
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c("Pre", "Post"), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' epi_head_and_tail(df)
#' epi_stat_count_outliers(num_vec = df$x, coef = 0)
#' epi_stat_count_outliers(num_vec = df$x)
#' summary(df$x)
#' }
#'
#' @export
#'
#' @importFrom grDevices boxplot.stats
#'

#' @details
#' Returns 0 for empty or all-`NA` vectors and raises an error when `coef`
#' is negative. Setting `coef` to `0` disables outlier detection.
epi_stats_count_outliers <- function(num_vec = NULL,
                                     coef = 1.5,
                                     ...) {
  if (is.null(num_vec) || !is.numeric(num_vec)) {
    stop("num_vec must be a numeric vector")
  }
  if (!is.numeric(coef) || length(coef) != 1) {
    stop("coef must be a single numeric value")
  }
  if (coef < 0) {
    stop("coef must be non-negative")
  }
  if (coef == 0 || length(num_vec) == 0 || all(is.na(num_vec))) {
    return(0L)
  }
  q1 <- stats::quantile(num_vec, 0.25, na.rm = TRUE, type = 7)
  q3 <- stats::quantile(num_vec, 0.75, na.rm = TRUE, type = 7)
  iqr_val <- q3 - q1
  lower <- q1 - coef * iqr_val
  upper <- q3 + coef * iqr_val
  outliers <- sum(num_vec < lower | num_vec > upper, na.rm = TRUE)
  outliers
}

# Alternative thresholds such as multiples of the standard deviation or
# inner/outer fences (e.g. 3 * IQR) are not currently implemented but may
# be explored in future iterations.

# Using the Qn estimator to avoid assuming that the underlying distribution
# is symmetric:
# https://stats.stackexchange.com/questions/1519/on-univariate-outlier-tests-or-dixon-q-versus-grubbs?rq=1
# median+/-delta* Qn
# get_qn <- robustbase::Qn(num_vec)
# median <- median(num_vec, na.rm = na.rm)
# ?
