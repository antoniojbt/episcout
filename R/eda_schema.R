#' Check observed data against an EDA specification
#'
#' Compare expected variables in an EDA specification with variables observed in a data frame. Presence and type compatibility are reported separately; this function does not coerce data or stop on incompatible observed types.
#'
#' @param data A data frame or an [epi_eda_postgres_source()] to check.
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A data frame with one row per expected or unexpected variable. The historical `status` column reports presence. `type_status` is one of `compatible`, `coercible`, `incompatible` or `not_applicable`, and `type_reason` explains that classification.
#'
#' @export
epi_eda_check_schema <- function(data, spec) {
  if (inherits(data, "epi_eda_postgres_source")) {
    spec <- epi_eda_spec(spec)
    return(eda_postgres_transaction(
      data,
      eda_postgres_schema_inside(data, spec)
    ))
  }
  if (!is.data.frame(data)) {
    stop("Data must be a data frame or an epi_eda_postgres_source.", call. = FALSE)
  }

  spec <- epi_eda_spec(spec)
  data_names <- names(data)
  expected_names <- spec$name

  expected <- data.frame(
    name = expected_names,
    expected_type = spec$type,
    observed_type = unname(vapply(
      expected_names,
      function(name) {
        if (name %in% data_names) {
          eda_observed_type(data[[name]])
        } else {
          NA_character_
        }
      },
      character(1)
    )),
    expected_present = TRUE,
    observed_present = expected_names %in% data_names,
    stringsAsFactors = FALSE
  )
  expected$status <- ifelse(expected$observed_present, "present", "missing")
  compatibility <- lapply(seq_along(expected_names), function(i) {
    name <- expected_names[[i]]
    if (!name %in% data_names) {
      return(list(
        status = "not_applicable",
        reason = "Observed variable is not present."
      ))
    }
    eda_type_compatibility(
      data[[name]],
      spec$type[[i]],
      if ("levels" %in% names(spec)) eda_spec_levels(spec$levels[[i]]) else character(),
      eda_missing_codes(spec, name)
    )
  })
  expected$type_status <- vapply(compatibility, `[[`, character(1), "status")
  expected$type_reason <- vapply(compatibility, `[[`, character(1), "reason")

  unexpected_names <- setdiff(data_names, expected_names)
  if (length(unexpected_names) == 0) {
    return(expected[, schema_columns()])
  }

  unexpected <- data.frame(
    name = unexpected_names,
    expected_type = NA_character_,
    observed_type = unname(vapply(
      unexpected_names,
      function(name) eda_observed_type(data[[name]]),
      character(1)
    )),
    expected_present = FALSE,
    observed_present = TRUE,
    status = "unexpected",
    type_status = "not_applicable",
    type_reason = "Variable is not declared in the EDA specification.",
    stringsAsFactors = FALSE
  )

  rbind(expected[, schema_columns()], unexpected[, schema_columns()])
}

schema_columns <- function() {
  c(
    "name",
    "expected_type",
    "observed_type",
    "expected_present",
    "observed_present",
    "status",
    "type_status",
    "type_reason"
  )
}

eda_type_compatibility <- function(values, expected_type, declared_levels, missing_codes) {
  observed_class <- paste(class(values), collapse = "/")
  observed <- values[!summary_missing_mask(values, missing_codes)]

  if (expected_type == "numeric") {
    if (is.numeric(values) && !inherits(values, c("Date", "POSIXt", "IDate"))) {
      return(eda_type_result("compatible", "Observed numeric or integer storage is compatible with numeric."))
    }
    return(eda_type_result("incompatible", paste0("Observed class ", observed_class, " is not numeric storage.")))
  }

  if (expected_type == "integer") {
    if (is.integer(values) && !inherits(values, "IDate")) {
      return(eda_type_result("compatible", "Observed integer storage is compatible with integer."))
    }
    if (is.double(values) && !inherits(values, c("Date", "POSIXt"))) {
      if (all(is.finite(observed)) && all(observed == trunc(observed))) {
        return(eda_type_result("coercible", "All observed numeric values are finite whole numbers and can be coerced to integer."))
      }
      return(eda_type_result("incompatible", "Observed numeric values include non-whole or non-finite values."))
    }
    return(eda_type_result("incompatible", paste0("Observed class ", observed_class, " cannot be coerced safely to integer.")))
  }

  if (expected_type == "categorical") {
    if (is.factor(values) || is.character(values)) {
      return(eda_type_result("compatible", "Observed factor or character storage is compatible with categorical."))
    }
    if (length(declared_levels) > 0L && is.atomic(values) && !inherits(values, c("Date", "POSIXt")) && all(as.character(observed) %in% declared_levels)) {
      return(eda_type_result("coercible", "All observed values match the declared categorical levels."))
    }
    return(eda_type_result("incompatible", paste0("Observed class ", observed_class, " is not categorical and its values do not all match declared levels.")))
  }

  if (expected_type == "binary") {
    if (is.logical(values)) {
      return(eda_type_result("compatible", "Observed logical storage is compatible with binary."))
    }
    if (is.factor(values) && length(declared_levels) == 2L &&
          identical(levels(values), declared_levels) &&
          all(as.character(observed) %in% declared_levels)) {
      return(eda_type_result("compatible", "Observed factor storage exactly matches the two declared binary levels."))
    }
    if (length(declared_levels) == 2L && is.atomic(values) && !inherits(values, c("Date", "POSIXt")) && all(as.character(observed) %in% declared_levels)) {
      return(eda_type_result("coercible", "All observed values match the two declared binary levels."))
    }
    return(eda_type_result("incompatible", paste0("Observed class ", observed_class, " does not match two declared binary levels.")))
  }

  if (expected_type == "text") {
    if (is.character(values)) {
      return(eda_type_result("compatible", "Observed character storage is compatible with text."))
    }
    if (is.factor(values)) {
      return(eda_type_result("coercible", "Observed factor values can be coerced to text."))
    }
    return(eda_type_result("incompatible", paste0("Observed class ", observed_class, " is not character or factor storage.")))
  }

  if (expected_type == "date") {
    if (inherits(values, c("Date", "IDate"))) {
      return(eda_type_result("compatible", "Observed Date or IDate storage is compatible with date."))
    }
    if (is.character(values) && eda_all_iso_dates(observed)) {
      return(eda_type_result("coercible", "All observed character values are parseable ISO dates."))
    }
    return(eda_type_result("incompatible", paste0("Observed class ", observed_class, " is not a date and contains non-ISO date values.")))
  }

  if (expected_type == "datetime") {
    if (inherits(values, c("POSIXct", "POSIXlt"))) {
      return(eda_type_result("compatible", "Observed POSIXct or POSIXlt storage is compatible with datetime."))
    }
    if (is.character(values) && eda_all_iso_datetimes(observed)) {
      return(eda_type_result("coercible", "All observed character values are parseable ISO-8601 datetimes."))
    }
    return(eda_type_result("incompatible", paste0("Observed class ", observed_class, " is not a datetime and contains non-ISO-8601 values.")))
  }

  eda_type_result("incompatible", paste0("Observed class ", observed_class, " is unsupported for declared type ", expected_type, "."))
}

eda_type_result <- function(status, reason) {
  list(status = status, reason = reason)
}

eda_all_iso_dates <- function(values) {
  values <- as.character(values)
  if (length(values) == 0L) {
    return(TRUE)
  }
  has_iso_shape <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", values)
  parsed <- suppressWarnings(as.Date(values, format = "%Y-%m-%d"))
  all(has_iso_shape & !is.na(parsed) & format(parsed, "%Y-%m-%d") == values)
}

eda_all_iso_datetimes <- function(values) {
  values <- as.character(values)
  if (length(values) == 0L) {
    return(TRUE)
  }
  has_iso_shape <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?(Z|[+-][0-9]{2}:?[0-9]{2})?$", values)
  parsed <- summary_parse_datetime_chr(values)
  all(has_iso_shape & !is.na(parsed))
}

eda_observed_type <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return("datetime")
  }

  if (inherits(x, "Date")) {
    return("date")
  }

  if (is.factor(x)) {
    return("categorical")
  }

  if (is.numeric(x) || is.integer(x)) {
    return("numeric")
  }

  if (is.character(x)) {
    return("text")
  }

  if (is.logical(x)) {
    return("binary")
  }

  class(x)[1]
}
