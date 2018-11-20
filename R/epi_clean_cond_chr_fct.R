#' @title Check if column is character or factor
#'
#' @description Check if column is character or factor (TRUE). Useful when
#' extracting columns from a large data frame using dplyr
#' (ie get all integer and numeric columns)
#'
#' @param col column header as string or integer. This will be passed to
#' is.character(col) and is.factor(col)
#'
#' @return boolean TRUE/FALSE indicating whether column passed is type character
#' or factor
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_cond_numeric}}, \code{\link{epi_clean_cond_date}},
#' \code{\link{epi_clean_class_to_factor}}, \code{\link{epi_clean_count_classes}},
#' \code{\link[base]{is.character}}, \code{\link[base]{is.factor}}.
#'
#' @examples
#'
#' \dontrun{
#' col_chr <- data.frame('chr1' = rep(c('A', 'B')),
#' 											'chr2' = rep(c('C', 'D'))
#' 											)
#' df_cont_chr <- as.tibble(cbind(df, col_chr))
#' df_cont_chr %>% select_if(~ epi_clean_cond_chr_fct(.))
#' epi_clean_cond_chr_fct(df_cont_chr[[2]]) # should be 'TRUE'
#' epi_clean_cond_chr_fct(df_cont_chr[, 'x']) # should be 'FALSE'
#' }
#'
#' @export
#'

epi_clean_cond_chr_fct <- function(col = NULL) {
	is.character(col) == TRUE | is.factor(col) == TRUE
}
