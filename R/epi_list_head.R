#' @title Print first few rows of each element of a list
#'
#' @description epi_list_tail() Prettier print of lists for head and tail
#'
#' @param list a list object to print
#' @param rows Number of rows to print, default is 3
#' @param max Maximum number of elements in list to print, default is the
#' length of the list passed
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @examples
#'
#' \dontrun{
#' epi_list_head(as.list(df), 5, 4)
#' }
#'
#' @export
#'

epi_list_head <- function(list = NULL,
                          rows = 3,
                          max = length(list)
                          ) {
  cat(sprintf('List has %s elements in total.\n', length(list)))
  cat(sprintf('First %s rows of first %s elements in list: \n', rows, max))
  for (item in 1:max) {
    print(head(list[[item]], rows))
  }
}
