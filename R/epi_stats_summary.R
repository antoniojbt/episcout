#' @title Get summary statistics from a data frame with multiple columns
#'
#' @description epi_stats_summary() provides summary descriptive statistics
#' for columns belonging to either character and factor (class_type = 'chr_fct')
#' or integer and numeric (class_type = 'int_num') while discarding values
#' provided (codes). This is useful if data frame has contingency codes.
#' Columns are ordered according to order in contingency codes option.
#' Rows are then ordered in decreasing order according to column provided.
#'
#' @param df Data frame
#' @param codes Specify codes to summarise or exclude as string. Default is NULL.
#' @param class_type Class of variables to summarise, 'chr_fct' or 'int_num'.
#' Default is character and factor.
#' @param action Values to summarise, 'codes_only' or 'exclude'. Default is 'exclude'.
#'
#' @return A data.frame as tibble with summaries.
#'
#' @note Desgined with data frames that require pre-processing and likely
#' have contingency and database codes. Action 'exclude' excludes the string values
#' provided from the summary. Useful to quickly assess what a data.frame contains,
#' types of values in each column and summary statitics if excluding codes.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_numeric}},
#' \code{\link{epi_stats_format}},
#' \code{\link{epi_stats_tidy}},
#' \code{\link{epi_clean_cond_chr_fct}},
#' \code{\link{epi_clean_cond_numeric}}.
#'
#' @example vignettes/summary_funcs_examples.R
#'
#' @export
#'

epi_stats_summary <- function(df = NULL,
                              codes = NULL,
                              class_type = 'chr_fct', # 'int_num'
                              action = 'exclude' # 'codes_only'
                              ) {
  if (!requireNamespace('dplyr', quietly = TRUE)) {
    stop("Package dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('purrr', quietly = TRUE)) {
    stop("Package purrr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  df <- tibble::as.tibble(df)
  # Determine which group of columns to use:
  if (class_type == 'chr_fct') {
    cond <- expression(epi_clean_cond_chr_fct(.))
    } else if (class_type == 'int_num') {
    cond <- expression(epi_clean_cond_numeric(.))
    } else {
      stop('class_type parameter not specified correctly?')
    }
  # Determine what to do with the codes provided (count only codes or
  # exclude codes from counting):
  if (action == 'codes_only') {
    map_func <- expression(purrr::keep(., .p = (. %in% codes)))
    } else if (action == 'exclude') {
    map_func <- expression(purrr::discard(., .p = (. %in% codes)))
    } else {
      stop('action parameter not specified correctly?')
      }
  # Determine if to count or sum depending on class cond and action asked for
  # codes are expected to be summarised as factors (so count()) as they are
  # assumed to represent database codes for NA explanations
  # chr and factor columns would be counted regardless of codes only or codes excluded
  # so summary() should only be needed for num/int columns where codes are excluded
  if (class_type == 'int_num' & action == 'exclude') {
    sum_func <- expression(epi_stats_numeric(.))
    } else {
    # count is designed for data frames, not vectors, so pass as:
    sum_func <- expression(dplyr::count(data.frame(x = .x), x))
    }
  df <- df %>%
    dplyr::select_if(~ eval(cond)) %>%
    purrr::map(~ eval(map_func)) %>%
    purrr::map(~ eval(sum_func)) # Returns a list
  # Convert to dataframe with the same names for the var of interest:
  df <- as.data.frame(purrr::map_df(df,
                                    tibble::rownames_to_column,
                                    'var',
                                    .id = 'id')
                      )
  # Returns a list if sum_func is summary()
  df <- tibble::as.tibble(as.data.frame(df))
  # Drop 'var' col as not needed:
  df$var <- NULL
  # Make the rownames a column and order columns:
  # df$id <- rownames(df)
  # df <- df %>%
  #   select(id,
  #          everything()
  #   )
  return(df)
  }
