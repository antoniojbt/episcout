#' @title Write files with a consistent convenience function
#'
#' @description epi_write() writes files with a consistent convenience function.
#' Wraps data.table's fwrite with several assumptions as defaults.
#'
#' @param file_object Object to write
#'
#' @param file_name file name as a string
#'
#' @param row.names as for data.table::fwrite(), default is also FALSE
#'
#' @param quote as for data.table::fwrite(), default is FALSE
#'
#' @param sep as for data.table::fwrite(), default is tab separated
#'
#' @param na as for data.table::fwrite(), default is 'NA'
#'
#' @param col.names as for data.table::fwrite(), default is also TRUE
#'
#' @param ... pass any other data.table::fwrite() parameters
#'
#' @note Other parameters as specified by data.table::fwrite()
#' You are probably better off using the standard write.csv(),
#' data.table::fwrite() directly or other functions unless you are
#' writing out several similarly constructed objects.
#' Files are written with data.table. Column names are written,
#' row names are not, quotes are not used. Columns are tab separated.
#' NAs are written as 'NA'. Assumes the first row in the object is a header.
#'
#' @return None
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[data.table]{fwrite}}
#'
#' @examples
#'
#' \dontrun{
#' epi_write(some_dataframe, 'some_dataframe.tsv')
#' }
#'
#' @export
#'

epi_write <- function(file_object = NULL,
											file_name = '',
											row.names = FALSE,
											quote = FALSE,
											sep = '\t',
											na = 'NA',
											col.names = TRUE,
											...) {
	if (!requireNamespace('data.table', quietly = TRUE)) {
		stop("Package data.table needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	data.table::fwrite(x = file_object,
										 file = file_name,
										 row.names = row.names,
										 quote = quote,
										 sep = sep,
										 na = na,
										 col.names = col.names,
										 ...
										 )
	}
