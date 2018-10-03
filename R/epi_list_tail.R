#' @title Print the last few rows of each element of a list

#' epi_list_tail() Prettier print of lists
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
#' epi_list_tail(as.list(df), 5, 4)
#' }
#'
#' @export
#'

epi_list_tail <- function(list = list,
													rows = 3,
													max = length(list)
													) {
	cat(sprintf('List has %s elements in total.\n', length(list)))
	cat(sprintf('Last %s rows of first %s elements in list: \n', rows, max))
	for (item in 1:max) {
		print(tail(list[[item]], rows))
	}
}
