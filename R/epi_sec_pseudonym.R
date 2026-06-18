#' Generate a Secure Pseudonymisation Bridge Table
#'
#' Generate random, non-derivable token identifiers for participant
#' pseudonymisation. The returned bridge table maps each original participant
#' identifier to a cryptographically random token.
#'
#' @param participant_id Character, numeric or factor vector of participant
#'   identifiers. Values must be unique and non-missing.
#' @param n_bytes Number of random bytes per token. Must be at least 16. The
#'   default of 24 gives 192-bit tokens.
#' @param prefix Character prefix added before each token.
#' @param bridge_path Optional file path for writing the bridge table as CSV.
#'   If `NULL`, no file is written.
#' @param overwrite Logical. If `FALSE`, an existing `bridge_path` is not
#'   overwritten.
#'
#' @return A tibble with columns `participant_id` and `token_id`.
#'
#' @details
#' Tokens are generated from cryptographic random bytes using
#' `openssl::rand_bytes()`. The function does not accept a seed and does not
#' provide deterministic output.
#'
#' The bridge table remains re-identifying information. Store it separately
#' from pseudonymised analysis datasets and protect it with appropriate access
#' controls. Pseudonymisation reduces risk but does not by itself anonymise a
#' dataset.
#'
#' @examples
#' participant_ids <- sprintf("study_%04d", 1:10)
#' bridge <- epi_sec_pseudonym(participant_ids, prefix = "MXSTUDY")
#' head(bridge)
#'
#' @importFrom openssl rand_bytes
#' @export
epi_sec_pseudonym <- function(participant_id,
                              n_bytes = 24,
                              prefix = "P",
                              bridge_path = NULL,
                              overwrite = FALSE) {
  if (!is.character(participant_id) &&
      !is.numeric(participant_id) &&
      !is.factor(participant_id)) {
    stop(
      "participant_id must be a character, numeric or factor vector.",
      call. = FALSE
    )
  }

  if (length(participant_id) == 0) {
    stop("participant_id must contain at least one value.", call. = FALSE)
  }

  if (anyNA(participant_id)) {
    stop("participant_id must not contain missing values.", call. = FALSE)
  }

  if (anyDuplicated(participant_id)) {
    stop("participant_id values must be unique.", call. = FALSE)
  }

  if (!is.numeric(n_bytes) ||
      length(n_bytes) != 1 ||
      is.na(n_bytes) ||
      n_bytes != as.integer(n_bytes) ||
      n_bytes < 16) {
    stop("n_bytes must be a whole number greater than or equal to 16.",
      call. = FALSE
    )
  }

  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix)) {
    stop("prefix must be a single non-missing character value.", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(bridge_path)) {
    if (!is.character(bridge_path) ||
        length(bridge_path) != 1 ||
        is.na(bridge_path)) {
      stop("bridge_path must be NULL or a single file path.", call. = FALSE)
    }

    if (file.exists(bridge_path) && !overwrite) {
      stop(
        "bridge_path already exists. Set overwrite = TRUE to replace it.",
        call. = FALSE
      )
    }
  }

  n_bytes <- as.integer(n_bytes)
  n <- length(participant_id)
  random_bytes <- openssl::rand_bytes(n * n_bytes)
  random_hex <- format(random_bytes)
  token_hex <- apply(
    matrix(random_hex, nrow = n, ncol = n_bytes, byrow = TRUE),
    1,
    paste0,
    collapse = ""
  )
  token_id <- paste0(prefix, "_", token_hex)

  if (anyDuplicated(token_id)) {
    stop("Duplicate token generated. Re-run with more bytes.", call. = FALSE)
  }

  bridge <- tibble::tibble(
    participant_id = participant_id,
    token_id = token_id
  )

  if (!is.null(bridge_path)) {
    utils::write.csv(bridge, file = bridge_path, row.names = FALSE)
  }

  bridge
}
