#' @title Get summary descriptive statistics for numeric columns.
#'
#' @description epi_stats_numeric() generates several descriptive summary statistics for numeric
#' variables.
#'
#' @param num_vec Numeric vector to test.
#' @param na.rm Remove NA values, default is TRUE
#' @param coef Coefficient for outlier detection, default is 1.5
#' @param ... is passed to skewness() and kurtosis()
#'
#' @return Data.frame with results from tests performed
#'
#' @note Normality is tested with Shapiro-Wilk (small values indicate
#' non-normality). Testing normality can be contentious, likely uninformative
#' and with Shapiro-Wilk can only be done for sample size between 3-5000.
#' The package e1071 is used for skewness and kurtosis.
#' For skewness: negative/longer left tail, positive/longer right tail,
#' values above 1 usually means non-normality. For kurtosis consider lower values,
#' broader shape and longer tails (platy ~<3), normal (meso ~3)
#' and slender/no tails (lepto ~<3). Outliers are detected with the Tukey
#' method (above and below 1.5 * IQR). na.rm is TRUE by default for all tests.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_summary}},
#' \code{\link{epi_stats_format}},
#' \code{\link{epi_stats_tidy}},
#' \code{\link[e1071]{skewness}},
#' \code{\link[e1071]{kurtosis}}.
#'
#' @example vignettes/summary_funcs_examples.R
#'
#' @export
#'
#' @importFrom stats median na.omit quantile sd shapiro.test var
#'

epi_stats_numeric <- function(num_vec = NULL,
                              na.rm = TRUE,
                              coef = 1.5,
                              ...
                              ) {
  if (!requireNamespace('e1071', quietly = TRUE)) {
    stop("Package e1071 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  cond <- length(num_vec) > 3 & length(num_vec) < 5000
  if (cond) {
    normality <- shapiro.test(num_vec)
    normality <- normality$p.value
    } else {
    normality <- NA
    }
  desc_stats <- data.frame('min' = min(num_vec, na.rm = na.rm),
                           'quantile_25' = quantile(num_vec, probs = 0.25, names = FALSE, na.rm = na.rm),
                           'mean' = mean(num_vec, na.rm = na.rm),
                           'median' = median(num_vec, na.rm = na.rm),
                           'quantile_75' = quantile(num_vec, probs = 0.75, names = FALSE, na.rm = na.rm),
                           'max' = max(num_vec, na.rm = na.rm),
                           'SD' = sd(num_vec, na.rm = na.rm),
                           'variance' = var(num_vec, na.rm = na.rm),
                           'sem' = sd(num_vec, na.rm = na.rm) / sqrt(length(na.omit(num_vec))),
                           'skewness' = e1071::skewness(num_vec, na.rm = na.rm, ...),
                           'kurtosis' = e1071::kurtosis(num_vec, na.rm = na.rm, ...),
                           'Shapiro_Wilk_p_value' = normality,
                           'outlier_count' = epi_stats_count_outliers(num_vec, coef = coef),
                           'NA_count' = length(which(is.na(num_vec))),
                           'NA_percentage' = (length(which(is.na(num_vec))) / length(num_vec)) * 100
                           )
  return(desc_stats)
  }

# TO DO:
# add outlier_counts perc of total for column excluding NAs (?)
# add total n - NA (?) as column
