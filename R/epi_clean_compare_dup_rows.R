#' @title Compare two rows which may be duplicated
#'
#' @description Compare two rows which may have duplicated information.
#' epi_clean_compare_dup_rows() uses compare::compare() for possibly duplicated rows.
#' compare::compare allows all transformations, sorting, etc. so can be loose.
#' This function is intended to make manual inspection easier,
#' compare::compare can miss differences though so care is needed.
#'
#' @param df_dups a data frame with duplicated entries to compare
#'
#' @param val_id is a value that is thought to be duplicated (eg a repeating row ID),
#' passed as a string. Grep is used to search for duplicates without regex with
#' fixed = TRUE
#'
#' @param col_id is a string to indicate an ID column
#'
#' @param sub_index_1 default = 1
#'
#' @param sub_index_2 default = 2
#'
#' @param ... pass any other options from compare::compare()
#'
#' @param allowAll compare::compare option
#'
#' @return returns a list object with the differing columns ('differing_cols'),
#' their names ('col_names') and the duplicated indices
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[compare]{compare}}, \code{\link[base]{grepl}}
#'
#' @examples
#'
#' \dontrun{
#' # Data frame object with rows thought to have duplicated entries:
#' check_dups
#' # Specify the row ID to grep where duplicate values are expected:
#' val_id <- '2'
#' comp <- epi_clean_compare_dup_rows(check_dups, val_id, 'var_id', 1, 2)
#' comp
#' View(t(check_dups[comp$duplicate_indices, ]))
#' View(t(check_dups[comp$duplicate_indices, comp$differing_cols]))}
#'
#' @export
#'

epi_clean_compare_dup_rows <- function(df_dups = NULL,
                                       val_id = '1',
                                       col_id = '',
                                       sub_index_1 = 1,
                                       sub_index_2 = 2,
                                       allowAll = TRUE,
                                       ...
                                         ) {
  if (!requireNamespace('compare', quietly = TRUE)) {
    stop("Package compare needed for this function to work. Please install it.",
         call. = FALSE)
  }
  val_id <- as.character(val_id)
  dup_indices <- which(grepl(val_id,
                             df_dups[[col_id]],
                             fixed = TRUE) # match as string, not regex
  )
  # check_dups[dup_indices, 1:2]
  comp <- compare::compare(df_dups[dup_indices[sub_index_1], , drop = FALSE],
                           df_dups[dup_indices[sub_index_2], , drop = FALSE],
                           allowAll = allowAll,
                           ...
                           )
  comp_diff <- which(comp$detailedResult == FALSE)
  names_diff_cols <- names(which(comp$detailedResult == FALSE))
  comp_results <- list('differing_cols' = comp_diff,
                       'col_names' = names_diff_cols,
                       'duplicate_indices' = dup_indices
                       )
  return(comp_results)
}
