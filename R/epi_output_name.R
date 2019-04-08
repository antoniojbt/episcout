#' @title Create a name for plots and files to save
#'
#' @description Generate a simple output name, given a string (filename)
#' splits at the last '.', drops the current suffix and adds the one provided.
#'
#' @param input_name string to split, usually an input file name
#' previously saved as an object
#'
#' @param suffix any string you wish to add, default is '.tsv'
#'
#' @return string value
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @examples
#'
#' \dontrun{
#' input_name <- 'baseline_clean.RData'
#' svg(epi_output_name(input_name, '_histogram_obesity_1.svg'))
#' plot(x, y)
#' dev.off()
#' }
#'
#' @export
#'

epi_output_name <- function(input_name = '',
                            suffix = '.tsv') {
  # Split infile name at the last '.':
  output_name <- strsplit(input_name, "[.]\\s*(?=[^.]+$)", perl = TRUE)[[1]][1]
  output_name <- sprintf('%s%s', output_name, suffix)
  return(output_name)
}
