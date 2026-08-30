.curp_birthplace_codes <- c(
  "AS", "BC", "BS", "CC", "CL", "CM", "CS", "CH", "DF", "DG", "GT",
  "GR", "HG", "JC", "MC", "MN", "MS", "NT", "NL", "OC", "PL", "QT",
  "QR", "SP", "SL", "SR", "TC", "TS", "TL", "VZ", "YN", "ZS", "NE"
)

# Days from 1970-01-01 to 1900-01-01, stored directly to avoid
# platform-specific warnings from parsing pre-1902 character dates.
.curp_birth_date_lower_bound <- structure(-25567, class = "Date")

.curp_empty_legacy <- function() {
  tibble::tibble(
    CURP = character(),
    PrimeraLetraApellidoPaterno = character(),
    PrimeraVocalApellidoPaterno = character(),
    PrimeraLetraApellidoMaterno = character(),
    PrimeraLetraNombre = character(),
    AnoNacimiento = character(),
    MesNacimiento = character(),
    DiaNacimiento = character(),
    Sexo = character(),
    EntidadFederativa = character(),
    PrimerasConsonantes = character(),
    Homoclave = character(),
    DigitoVerificador = character()
  )
}

#' Extract CURP components using the legacy schema
#'
#' `epi_clean_curp()` is a compatibility extractor, not a CURP validator. It
#' preserves the historical 13-column Spanish schema and includes the supplied
#' CURP in its result. Use [epi_clean_curp_audit()] for privacy-aware structural
#' validation and optional reconciliation.
#'
#' @param curp A character vector of CURPs. Non-missing values must contain
#'   exactly 18 characters.
#'
#' @return A tibble with one row per input and the historical columns `CURP`,
#'   `PrimeraLetraApellidoPaterno`, `PrimeraVocalApellidoPaterno`,
#'   `PrimeraLetraApellidoMaterno`, `PrimeraLetraNombre`, `AnoNacimiento`,
#'   `MesNacimiento`, `DiaNacimiento`, `Sexo`, `EntidadFederativa`,
#'   `PrimerasConsonantes`, `Homoclave` and `DigitoVerificador`.
#'
#' @details This function performs positional extraction after a length check.
#' It does not establish structural validity, checksum validity, registry
#' assignment, certification or identity. Its returned CURP and derived fields
#' are personal data; callers are responsible for access, storage and retention.
#' The historical year split is retained for one compatibility cycle.
#'
#' @importFrom tibble tibble
#' @export
epi_clean_curp <- function(curp) {
  if (!is.character(curp)) {
    stop("`curp` debe ser un vector de texto.", call. = FALSE)
  }

  lengths <- nchar(curp, type = "chars", allowNA = TRUE, keepNA = TRUE)
  if (any(!is.na(lengths) & lengths != 18L)) {
    stop("Cada CURP no faltante debe tener exactamente 18 caracteres.", call. = FALSE)
  }
  if (length(curp) == 0L) {
    return(.curp_empty_legacy())
  }

  rows <- lapply(curp, function(value) {
    year <- suppressWarnings(as.integer(substr(value, 5L, 6L)))
    tibble::tibble(
      CURP = value,
      PrimeraLetraApellidoPaterno = substr(value, 1L, 1L),
      PrimeraVocalApellidoPaterno = substr(value, 2L, 2L),
      PrimeraLetraApellidoMaterno = substr(value, 3L, 3L),
      PrimeraLetraNombre = substr(value, 4L, 4L),
      AnoNacimiento = ifelse(
        is.na(year),
        NA_character_,
        ifelse(year <= 22L, paste0("20", substr(value, 5L, 6L)), paste0("19", substr(value, 5L, 6L)))
      ),
      MesNacimiento = substr(value, 7L, 8L),
      DiaNacimiento = substr(value, 9L, 10L),
      Sexo = substr(value, 11L, 11L),
      EntidadFederativa = substr(value, 12L, 13L),
      PrimerasConsonantes = substr(value, 14L, 16L),
      Homoclave = substr(value, 17L, 17L),
      DigitoVerificador = substr(value, 18L, 18L)
    )
  })

  dplyr::bind_rows(rows)
}

.curp_empty_issues <- function() {
  tibble::tibble(
    input_index = integer(),
    issue_code = character(),
    stage = character(),
    severity = character()
  )
}

.curp_empty_summary <- function() {
  tibble::tibble(type = character(), value = character(), n = integer())
}

.curp_issue_rows <- function(checks) {
  rows <- lapply(checks, function(check) {
    indices <- which(check$mask)
    if (length(indices) == 0L) {
      return(NULL)
    }
    tibble::tibble(
      input_index = as.integer(indices),
      issue_code = rep(check$code, length(indices)),
      stage = rep(check$stage, length(indices)),
      severity = rep("error", length(indices))
    )
  })
  result <- dplyr::bind_rows(rows)
  if (nrow(result) == 0L) {
    return(.curp_empty_issues())
  }
  result
}

.curp_reference <- function(value, n, argument, type, valid) {
  if (is.null(value)) {
    return(list(requested = FALSE, value = rep(NA, n)))
  }
  if (!(length(value) %in% c(1L, n))) {
    stop(
      sprintf("`%s` must have length one or match `curp`.", argument),
      call. = FALSE
    )
  }
  if (!type(value)) {
    stop(sprintf("`%s` must be %s.", argument, valid$type_message), call. = FALSE)
  }
  if (!valid$check(value)) {
    stop(sprintf("`%s` must contain only %s or missing values.", argument, valid$value_message), call. = FALSE)
  }
  list(requested = TRUE, value = rep(value, length.out = n))
}

.curp_comparison_state <- function(reference, derived, valid_record) {
  n <- length(valid_record)
  if (!reference$requested) {
    return(rep("not_requested", n))
  }
  state <- rep("curp_unavailable", n)
  reference_missing <- valid_record & is.na(reference$value)
  comparable <- valid_record & !is.na(reference$value)
  state[reference_missing] <- "reference_missing"
  state[comparable] <- ifelse(
    reference$value[comparable] == derived[comparable],
    "match",
    "mismatch"
  )
  state
}

.curp_summary <- function(records, issues) {
  if (nrow(records) == 0L) {
    return(.curp_empty_summary())
  }
  status_order <- c("valid", "invalid", "missing")
  status_n <- tabulate(match(records$status, status_order), nbins = length(status_order))
  status_rows <- tibble::tibble(
    type = rep("status", sum(status_n > 0L)),
    value = status_order[status_n > 0L],
    n = as.integer(status_n[status_n > 0L])
  )
  if (nrow(issues) == 0L) {
    return(status_rows)
  }
  issue_values <- unique(issues$issue_code)
  issue_rows <- tibble::tibble(
    type = rep("issue", length(issue_values)),
    value = issue_values,
    n = vapply(issue_values, function(value) sum(issues$issue_code == value), integer(1L))
  )
  dplyr::bind_rows(status_rows, issue_rows)
}

#' Audit CURP structure and reconcile reviewed reference fields
#'
#' Performs local, vector-safe structural checks for CURP values and optionally
#' compares successfully derived fields with separately reviewed reference
#' vectors. It makes no network calls and does not return the supplied CURP.
#'
#' @param curp A character vector. Values are checked exactly as supplied;
#'   lowercase, whitespace and Unicode are not normalised.
#' @param birth_date An optional `Date` vector of length one or `length(curp)`.
#' @param sex_code An optional character vector containing CURP recorded-sex
#'   codes `H`, `M` or missing values.
#' @param birthplace_code An optional character vector containing reviewed CURP
#'   birthplace codes or missing values.
#' @param initials An optional character vector containing reviewed four-letter
#'   CURP initials segments or missing values. Names are not accepted or derived.
#'
#' @return An `epi_curp_audit` list containing `records`, `issues`, `comparison`
#'   and aggregate `summary` tibbles. `records` omits the original CURP but its
#'   derived date, recorded-sex code, birthplace and initials remain restricted
#'   personal data. `valid` means only that the documented local structural
#'   checks passed. `checksum_status` is `not_verified` because the authoritative
#'   position-18 algorithm is not publicly evidenced for this implementation.
#'
#' @details Numeric position-17 markers are interpreted within the supported
#' 1900--1999 domain and `A`--`J` markers within 2000--2099. A possible pre-1900
#' key cannot be distinguished from the corresponding 1900s key locally. Future
#' dates are rejected. Local structure does not prove registry assignment,
#' certification, authenticity or identity. Callers control access, storage,
#' retention and linkage of the restricted row-level result.
#'
#' @examples
#' epi_clean_curp_audit(c(NA_character_, "NOT-A-CURP"))
#'
#' @export
epi_clean_curp_audit <- function(curp,
                                 birth_date = NULL,
                                 sex_code = NULL,
                                 birthplace_code = NULL,
                                 initials = NULL) {
  if (!is.character(curp)) {
    stop("`curp` must be a character vector.", call. = FALSE)
  }
  n <- length(curp)
  missing <- is.na(curp)
  lengths <- nchar(curp, type = "chars", allowNA = TRUE, keepNA = TRUE)
  length_ok <- !missing & lengths == 18L
  segment <- function(start, stop = start) substr(curp, start, stop)

  initials_ok <- length_ok & grepl("^[A-Z][AEIOUX][A-Z]{2}$", segment(1L, 4L))
  date_format_ok <- length_ok & grepl("^[0-9]{6}$", segment(5L, 10L))
  sex_ok <- length_ok & segment(11L) %in% c("H", "M")
  birthplace_ok <- length_ok & segment(12L, 13L) %in% .curp_birthplace_codes
  consonants_ok <- length_ok & grepl(
    "^[BCDFGHJKLMNPQRSTVWXYZ]{3}$",
    segment(14L, 16L)
  )
  marker_ok <- length_ok & grepl("^[0-9A-J]$", segment(17L))
  check_digit_format_ok <- length_ok & grepl("^[0-9]$", segment(18L))

  marker <- segment(17L)
  year <- rep(NA_integer_, n)
  date_candidates <- date_format_ok & marker_ok
  year[date_candidates & grepl("^[0-9]$", marker)] <-
    1900L + as.integer(segment(5L, 6L)[date_candidates & grepl("^[0-9]$", marker)])
  year[date_candidates & grepl("^[A-J]$", marker)] <-
    2000L + as.integer(segment(5L, 6L)[date_candidates & grepl("^[A-J]$", marker)])
  date_text <- rep(NA_character_, n)
  date_text[date_candidates] <- sprintf(
    "%04d-%s-%s",
    year[date_candidates],
    segment(7L, 8L)[date_candidates],
    segment(9L, 10L)[date_candidates]
  )
  derived_date <- suppressWarnings(as.Date(date_text, format = "%Y-%m-%d"))
  calendar_ok <- date_candidates & !is.na(derived_date)
  future_date <- calendar_ok & derived_date > Sys.Date()

  issues <- .curp_issue_rows(list(
    list(mask = missing, code = "missing_curp", stage = "input"),
    list(mask = !missing & !length_ok, code = "invalid_length", stage = "input"),
    list(mask = length_ok & !initials_ok, code = "invalid_initials", stage = "lexical"),
    list(mask = length_ok & !date_format_ok, code = "invalid_birth_date_format", stage = "lexical"),
    list(mask = length_ok & !sex_ok, code = "invalid_sex_code", stage = "lexical"),
    list(mask = length_ok & !birthplace_ok, code = "invalid_birthplace_code", stage = "catalogue"),
    list(mask = length_ok & !consonants_ok, code = "invalid_internal_consonants", stage = "lexical"),
    list(mask = length_ok & !marker_ok, code = "invalid_century_marker", stage = "date"),
    list(mask = length_ok & !check_digit_format_ok, code = "invalid_check_digit_format", stage = "lexical"),
    list(mask = date_candidates & !calendar_ok, code = "invalid_calendar_date", stage = "date"),
    list(mask = future_date, code = "future_birth_date", stage = "date")
  ))

  invalid <- !missing & (
    !length_ok | !initials_ok | !date_format_ok | !sex_ok | !birthplace_ok |
      !consonants_ok | !marker_ok | !check_digit_format_ok | !calendar_ok |
      future_date
  )
  valid <- !missing & !invalid
  status <- ifelse(missing, "missing", ifelse(valid, "valid", "invalid"))

  record_date <- derived_date
  record_date[!valid] <- as.Date(NA_character_)
  records <- tibble::tibble(
    input_index = seq_len(n),
    status = status,
    birth_date = record_date,
    sex_code = ifelse(valid, segment(11L), NA_character_),
    birthplace_code = ifelse(valid, segment(12L, 13L), NA_character_),
    initials = ifelse(valid, segment(1L, 4L), NA_character_),
    century_marker_class = ifelse(
      valid,
      ifelse(grepl("^[0-9]$", marker), "1900-1999", "2000-2099"),
      NA_character_
    ),
    checksum_status = ifelse(valid, "not_verified", NA_character_)
  )

  date_reference <- .curp_reference(
    birth_date,
    n,
    "birth_date",
    function(value) inherits(value, "Date"),
    list(
      type_message = "a Date vector",
      value_message = "dates from 1900 through the current date",
      check = function(value) {
        all(
          is.na(value) |
            (value >= .curp_birth_date_lower_bound & value <= Sys.Date())
        )
      }
    )
  )
  sex_reference <- .curp_reference(
    sex_code,
    n,
    "sex_code",
    is.character,
    list(
      type_message = "a character vector",
      value_message = "the CURP recorded-sex codes `H` and `M`",
      check = function(value) all(is.na(value) | value %in% c("H", "M"))
    )
  )
  birthplace_reference <- .curp_reference(
    birthplace_code,
    n,
    "birthplace_code",
    is.character,
    list(
      type_message = "a character vector",
      value_message = "recognised CURP birthplace codes",
      check = function(value) all(is.na(value) | value %in% .curp_birthplace_codes)
    )
  )
  initials_reference <- .curp_reference(
    initials,
    n,
    "initials",
    is.character,
    list(
      type_message = "a character vector",
      value_message = "four uppercase letters in the reviewed CURP initials form",
      check = function(value) all(is.na(value) | grepl("^[A-Z][AEIOUX][A-Z]{2}$", value))
    )
  )

  comparison <- tibble::tibble(
    input_index = seq_len(n),
    birth_date = .curp_comparison_state(date_reference, records$birth_date, valid),
    sex_code = .curp_comparison_state(sex_reference, records$sex_code, valid),
    birthplace_code = .curp_comparison_state(
      birthplace_reference,
      records$birthplace_code,
      valid
    ),
    initials = .curp_comparison_state(initials_reference, records$initials, valid)
  )
  result <- list(
    records = records,
    issues = issues,
    comparison = comparison,
    summary = .curp_summary(records, issues)
  )
  structure(result, class = c("epi_curp_audit", "list"))
}

#' Print a CURP structural audit safely
#'
#' @param x An `epi_curp_audit` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.epi_curp_audit <- function(x, ...) {
  n <- nrow(x$records)
  label <- if (n == 1L) "record" else "records"
  cat("<epi_curp_audit>\n")
  cat(sprintf("%d %s\n", n, label))
  status_rows <- x$summary[x$summary$type == "status", , drop = FALSE]
  if (nrow(status_rows) > 0L) {
    cat(paste0(status_rows$value, ": ", status_rows$n, collapse = "; "), "\n")
  }
  cat("Checksum: not verified (authoritative algorithm deferred).\n")
  cat("Restricted row-level fields are not printed.\n")
  invisible(x)
}

#' Display a safe structure summary for a CURP audit
#'
#' @param object An `epi_curp_audit` object.
#' @param ... Unused.
#'
#' @return `object`, invisibly.
#' @export
str.epi_curp_audit <- function(object, ...) {
  print.epi_curp_audit(object)
}
