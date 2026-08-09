context("episcout CURP structural audit")

curp_2000_leap <- "XEXX000229MASXXXA0"
curp_1999_end <- "XEXX991231HNEYYY00"

test_that("epi_clean_curp_audit has a fixed privacy-aware schema", {
  expect_equal(
    names(formals(epi_clean_curp_audit)),
    c("curp", "birth_date", "sex_code", "birthplace_code", "initials")
  )

  audit <- epi_clean_curp_audit(c(curp_2000_leap, NA_character_))

  expect_s3_class(audit, "epi_curp_audit")
  expect_named(audit, c("records", "issues", "comparison", "summary"))
  expect_named(
    audit$records,
    c(
      "input_index", "status", "birth_date", "sex_code",
      "birthplace_code", "initials", "century_marker_class",
      "checksum_status"
    )
  )
  expect_named(audit$issues, c("input_index", "issue_code", "stage", "severity"))
  expect_named(
    audit$comparison,
    c("input_index", "birth_date", "sex_code", "birthplace_code", "initials")
  )
  expect_named(audit$summary, c("type", "value", "n"))
  expect_equal(audit$records$input_index, 1:2)
  expect_equal(audit$records$status, c("valid", "missing"))
  expect_equal(audit$records$checksum_status, c("not_verified", NA_character_))
  expect_false("curp" %in% names(audit$records))
})

test_that("structural audit handles empty, scalar and vector inputs", {
  expect_silent(empty <- epi_clean_curp_audit(character()))
  expect_equal(nrow(empty$records), 0L)
  expect_equal(nrow(empty$issues), 0L)
  expect_equal(nrow(empty$comparison), 0L)

  audit <- epi_clean_curp_audit(c(curp_1999_end, curp_2000_leap))
  expect_equal(audit$records$status, c("valid", "valid"))
  expect_equal(audit$records$birth_date, as.Date(c("1999-12-31", "2000-02-29")))
  expect_equal(audit$records$century_marker_class, c("1900-1999", "2000-2099"))
  expect_equal(audit$records$sex_code, c("H", "M"))
  expect_equal(audit$records$birthplace_code, c("NE", "AS"))
  expect_equal(audit$records$initials, c("XEXX", "XEXX"))
  expect_equal(audit$records$checksum_status, rep("not_verified", 2L))
})

test_that("missing input is distinct from malformed input", {
  audit <- epi_clean_curp_audit(c(NA_character_, "SHORT"))

  expect_equal(audit$records$status, c("missing", "invalid"))
  expect_equal(
    audit$issues$issue_code[audit$issues$input_index == 1L],
    "missing_curp"
  )
  expect_equal(
    audit$issues$issue_code[audit$issues$input_index == 2L],
    "invalid_length"
  )
})

test_that("input shape failures are value-free", {
  expect_error(epi_clean_curp_audit(1:2), "character vector", fixed = TRUE)
  expect_error(
    epi_clean_curp_audit(curp_2000_leap, sex_code = c("H", "M")),
    "length one or match",
    fixed = TRUE
  )
  expect_error(
    epi_clean_curp_audit(character(), sex_code = "X"),
    "recorded-sex codes",
    fixed = TRUE
  )

  secret_reference <- "DO-NOT-ECHO"
  condition <- tryCatch(
    epi_clean_curp_audit(curp_2000_leap, sex_code = secret_reference),
    error = identity
  )
  expect_s3_class(condition, "error")
  expect_false(grepl(secret_reference, conditionMessage(condition), fixed = TRUE))
})

test_that("lowercase, whitespace, punctuation and Unicode are not normalised", {
  inputs <- c(
    tolower(curp_2000_leap),
    paste0(" ", curp_2000_leap),
    sub("X", "-", curp_2000_leap, fixed = TRUE),
    sub("X", "Ñ", curp_2000_leap, fixed = TRUE)
  )
  audit <- epi_clean_curp_audit(inputs)

  expect_equal(audit$records$status, rep("invalid", 4L))
  expect_true(all(audit$issues$input_index %in% 1:4))
  expect_false(any(inputs %in% unlist(audit, use.names = FALSE)))
})

test_that("each official position class is checked independently", {
  mutate_position <- function(value, position, replacement) {
    substr(value, position, position) <- replacement
    value
  }
  inputs <- c(
    mutate_position(curp_2000_leap, 2L, "B"),
    mutate_position(curp_2000_leap, 5L, "A"),
    mutate_position(curp_2000_leap, 11L, "X"),
    mutate_position(curp_2000_leap, 12L, "X"),
    mutate_position(curp_2000_leap, 14L, "A"),
    mutate_position(curp_2000_leap, 17L, "K"),
    mutate_position(curp_2000_leap, 18L, "A")
  )
  expected_codes <- c(
    "invalid_initials",
    "invalid_birth_date_format",
    "invalid_sex_code",
    "invalid_birthplace_code",
    "invalid_internal_consonants",
    "invalid_century_marker",
    "invalid_check_digit_format"
  )
  audit <- epi_clean_curp_audit(inputs)

  expect_equal(audit$records$status, rep("invalid", length(inputs)))
  for (i in seq_along(expected_codes)) {
    expect_true(expected_codes[[i]] %in% audit$issues$issue_code[audit$issues$input_index == i])
  }
})

test_that("calendar, leap-day and future-date semantics are explicit", {
  invalid_non_leap <- "XEXX010229MASXXXA0"
  future_date <- "XEXX991231MASXXXA0"
  audit <- epi_clean_curp_audit(c(curp_2000_leap, invalid_non_leap, future_date))

  expect_equal(audit$records$status, c("valid", "invalid", "invalid"))
  expect_true("invalid_calendar_date" %in% audit$issues$issue_code[audit$issues$input_index == 2L])
  expect_true("future_birth_date" %in% audit$issues$issue_code[audit$issues$input_index == 3L])
  expect_true(is.na(audit$records$birth_date[[2L]]))
  expect_true(is.na(audit$records$birth_date[[3L]]))
})

test_that("the pinned birthplace catalogue accepts all and only reviewed codes", {
  codes <- c(
    "AS", "BC", "BS", "CC", "CL", "CM", "CS", "CH", "DF", "DG", "GT",
    "GR", "HG", "JC", "MC", "MN", "MS", "NT", "NL", "OC", "PL", "QT",
    "QR", "SP", "SL", "SR", "TC", "TS", "TL", "VZ", "YN", "ZS", "NE"
  )
  inputs <- vapply(codes, function(code) {
    value <- curp_1999_end
    substr(value, 12L, 13L) <- code
    value
  }, character(1L), USE.NAMES = FALSE)

  catalogue_audit <- epi_clean_curp_audit(inputs)
  expect_equal(catalogue_audit$records$status, rep("valid", length(codes)))
  expect_equal(
    catalogue_audit$records$checksum_status,
    rep("not_verified", length(codes))
  )
  invalid <- epi_clean_curp_audit(sub("NE", "XX", curp_1999_end, fixed = TRUE))
  expect_equal(invalid$records$status, "invalid")
  expect_true("invalid_birthplace_code" %in% invalid$issues$issue_code)

  catalogue_path <- system.file(
    "extdata",
    "curp_birthplace_codes.csv",
    package = "episcout"
  )
  expect_true(nzchar(catalogue_path))
  expect_equal(utils::read.csv(catalogue_path, stringsAsFactors = FALSE)$code, codes)
})

test_that("comparison states follow the accepted truth table", {
  invalid_curp <- sub("M", "X", curp_2000_leap, fixed = TRUE)
  audit <- epi_clean_curp_audit(
    c(curp_2000_leap, curp_2000_leap, curp_2000_leap, invalid_curp),
    birth_date = as.Date(c("2000-02-29", "2000-03-01", NA, "2000-02-29")),
    sex_code = c("M", "H", NA, "M"),
    birthplace_code = c("AS", "NE", NA, "AS")
  )

  expect_equal(audit$comparison$birth_date, c("match", "mismatch", "reference_missing", "curp_unavailable"))
  expect_equal(audit$comparison$sex_code, c("match", "mismatch", "reference_missing", "curp_unavailable"))
  expect_equal(audit$comparison$birthplace_code, c("match", "mismatch", "reference_missing", "curp_unavailable"))
  expect_equal(audit$comparison$initials, rep("not_requested", 4L))

  recycled <- epi_clean_curp_audit(
    c(curp_2000_leap, curp_2000_leap),
    initials = "XEXX"
  )
  expect_equal(recycled$comparison$initials, c("match", "match"))
})

test_that("comparison references are validated without exposing values", {
  expect_error(
    epi_clean_curp_audit(curp_2000_leap, birth_date = "2000-02-29"),
    "Date vector",
    fixed = TRUE
  )
  expect_error(
    epi_clean_curp_audit(curp_2000_leap, birthplace_code = "ZZ"),
    "recognised CURP birthplace codes",
    fixed = TRUE
  )
  expect_error(
    epi_clean_curp_audit(curp_2000_leap, initials = "PRIVATE-VALUE"),
    "four uppercase letters",
    fixed = TRUE
  )
})

test_that("printing and str disclose only aggregate status", {
  audit <- epi_clean_curp_audit(curp_2000_leap)
  printed <- capture.output(print(audit))
  structured <- capture.output(str(audit))
  disclosed <- paste(c(printed, structured), collapse = " ")

  expect_match(disclosed, "1 record", fixed = TRUE)
  expect_match(disclosed, "not verified", fixed = TRUE)
  expect_false(grepl(curp_2000_leap, disclosed, fixed = TRUE))
  expect_false(grepl("2000-02-29", disclosed, fixed = TRUE))
  expect_false(grepl("AS", disclosed, fixed = TRUE))
  expect_false(grepl("XEXX", disclosed, fixed = TRUE))
})

test_that("summary contains aggregate status and issue counts", {
  audit <- epi_clean_curp_audit(c(curp_2000_leap, "SHORT", NA_character_))

  expect_true(all(c("valid", "invalid", "missing") %in% audit$summary$value))
  expect_true("invalid_length" %in% audit$summary$value)
  expect_equal(sum(audit$summary$n[audit$summary$type == "status"]), 3L)
})
