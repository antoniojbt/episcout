#' @title Get summary statistics from a data frame with multiple columns
#'
#' @description Summarise character/factor or integer/numeric columns while counting or excluding reviewed code values. Set `output = "typed"` to return the canonical six-component descriptive contract for every supported column.
#'
#' @param df Data frame
#' @param codes Character vector of values to count or exclude. With typed output, these values are treated as global sentinel-missing codes.
#' @param class_type Current-output variable class: `"chr_fct"` or `"int_num"`. Leave the default when requesting typed output.
#' @param action Current-output handling of `codes`: `"codes_only"` or `"exclude"`. Typed output requires `"exclude"`.
#' @param output Output contract. `"current"` preserves the existing class/action-specific tibble. `"typed"` returns complete typed summary components for every supported column and treats `codes` as global sentinel-missing values.
#'
#' @return With `output = "current"`, a tibble using the historical mode-specific schema. With `output = "typed"`, a list containing `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped` data frames.
#'
#' @note The current output is a historical convenience interface for data frames containing contingency or database codes. Prefer typed output for complete variable coverage and explicit missingness semantics.
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_numeric}}, \code{\link{epi_stats_format}}, \code{\link{epi_stats_tidy}}, \code{\link{epi_clean_cond_chr_fct}}, \code{\link{epi_clean_cond_numeric}}.
#'
#' @example inst/examples/summary-functions.R
#'
#' @export
#'
# @importFrom magrittr "%>%"

epi_stats_summary <- function(df = NULL,
                              codes = NULL,
                              class_type = "chr_fct", # 'int_num'
                              action = "exclude", # 'codes_only'
                              output = c("current", "typed")) {
  output <- match.arg(output)
  df <- tibble::as_tibble(df)
  if (output == "typed") {
    if (action != "exclude") {
      stop("Typed output requires action = \"exclude\".", call. = FALSE)
    }
    if (class_type != "chr_fct") {
      stop("class_type applies only to current output; leave it at the default for typed output.", call. = FALSE)
    }
    types <- vapply(df, summary_infer_type, character(1))
    levels <- vapply(df, function(values) {
      if (is.factor(values)) {
        return(paste(levels(values), collapse = ";"))
      }
      if (is.logical(values)) {
        return("FALSE;TRUE")
      }
      ""
    }, character(1))
    spec <- data.frame(
      name = names(df),
      label = names(df),
      type = unname(types),
      role = rep(NA_character_, ncol(df)),
      levels = unname(levels),
      stringsAsFactors = FALSE
    )
    return(build_typed_summaries(as.data.frame(df), spec, global_missing_codes = codes))
  }
  # Determine which group of columns to use:
  if (class_type == "chr_fct") {
    selected <- dplyr::select_if(df, epi_clean_cond_chr_fct)
  } else if (class_type == "int_num") {
    selected <- dplyr::select_if(df, epi_clean_cond_numeric)
  } else {
    stop("class_type parameter not specified correctly?")
  }
  # Determine what to do with the codes provided (count only codes or exclude codes from counting):
  if (action == "codes_only") {
    filter_values <- function(.x) purrr::keep(.x, .p = .x %in% codes)
  } else if (action == "exclude") {
    filter_values <- function(.x) purrr::discard(.x, .p = .x %in% codes)
  } else {
    stop("action parameter not specified correctly?")
  }
  # Determine if to count or sum depending on class cond and action asked for codes are expected to be summarised as factors (so count()) as they are assumed to represent database codes for NA explanations chr and factor columns would be counted regardless of codes only or codes excluded so summary() should only be needed for num/int columns where codes are excluded
  if (class_type == "int_num" && action == "exclude") {
    sum_func <- function(.x) epi_stats_numeric(.x)
  } else {
    # count is designed for data frames, not vectors, so pass as:
    sum_func <- function(.x) dplyr::count(data.frame(x = .x), x)
  }

  df <- selected %>%
    purrr::map(filter_values) %>%
    purrr::map(sum_func) # Returns a list

  # Convert to dataframe with the same names for the var of interest:
  df <- as.data.frame(purrr::map_df(
    df,
    tibble::rownames_to_column,
    "var",
    .id = "id"
  ))
  # Returns a list if sum_func is summary()
  df <- tibble::as_tibble(as.data.frame(df))
  # Drop 'var' col as not needed:
  df$var <- NULL
  # Make the rownames a column and order columns:
  # df$id <- rownames(df)
  # df <- df %>%
  #   select(id,
  #          everything()
  #   )
  df
}
