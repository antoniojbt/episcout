#' @title Check whether column type is integer or numeric
#'
#' epi_clean_cond_numeric()
#'
#' @description Check if column is integer or numeric (TRUE). Useful when
#' extracting columns from a large data frame using dplyr
#' (ie get all integer and numeric columns)
#'
#' @param col column header as string or integer. This will be passed to
#' is.integer(col) and is.numeric(col)
#'
#' @return boolean TRUE/FALSE indicating whether column passed is type integer
#' or numeric
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_cond_chr_fct}}, \code{\link{epi_clean_cond_date}},
#' \code{\link{epi_clean_class_to_factor}}, \code{\link{epi_clean_count_classes}},
#' \code{\link[base]{is.integer}}, \code{\link[base]{is.numeric}}
#'
#' @examples
#'
#' \dontrun{
#' set.seed(12345)
#' n <- 20
#' df <- data.frame(
#' 	var_id = rep(1:(n / 2), each = 2),
#' 	var_to_rep = rep(c('Pre', 'Post'), n / 2),
#' 	x = rnorm(n),
#' 	y = rbinom(n, 1, 0.50),
#' 	z = rpois(n, 2)
#' )
#' df %>% select_if(~ epi_clean_cond_numeric(.))
#' epi_clean_cond_numeric(df[[2]]) # should be FALSE
#' epi_clean_cond_numeric(df[, 'x']) # should be TRUE
#' }
#'

epi_clean_cond_numeric <- function(col = NULL) {
	is.integer(col) == TRUE | is.numeric(col) == TRUE
}
