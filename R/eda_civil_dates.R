# Approved civil-date derivation is a separate executable contract for caller-reviewed local timestamps. This module validates explicit civil-date semantics, exact midnight across every non-missing source value and new-only publication without assigning a timezone, interpreting an instant or changing the source.

civil_date_operation_columns <- function() {
  c(
    "source_variable_key", "derived_name", "operation_state",
    "declared_semantics", "preserve_source", "require_midnight",
    "approval_id"
  )
}

#' Validate analyst-approved civil-date operations
#'
#' Create the executable operation object consumed by
#' [epi_eda_derive_civil_dates()]. Civil-date meaning must come from external
#' analyst review; episcout never infers it from a column name, source storage
#' or observed midnight values.
#'
#' @param operations A non-empty data frame containing exactly
#'   `source_variable_key`, `derived_name`, `operation_state`,
#'   `declared_semantics`, `preserve_source`, `require_midnight`, and
#'   `approval_id` in that order. See Details.
#'
#' @return An `epi_eda_approved_civil_dates` data frame in canonical opaque-key
#'   order.
#'
#' @details `operation_state` must be `"approved"`, `declared_semantics` must be
#'   `"civil_date"`, and both safeguards must be non-missing `TRUE` values.
#'   Source keys must be unique caller-managed opaque keys matching
#'   `^var_[a-z0-9]{16,64}$`; derived names must be unique non-empty valid UTF-8
#'   text. `approval_id` is a caller-managed opaque reference matching
#'   `^approval_[a-z0-9]{16,64}$`. Episcout validates the declared state and
#'   structure but does not authenticate the approving analyst.
#'
#'   Extra or reordered fields, pending operations, undeclared semantics, false
#'   safeguards, duplicate keys or derived names, and malformed approval
#'   references are rejected. Printing and structure display omit source and
#'   derived identities and approval references.
#'
#' @export
epi_eda_approved_civil_dates <- function(operations) {
  if (!is.data.frame(operations)) {
    stop(
      "operations must be a data frame using the approved civil-date schema.",
      call. = FALSE
    )
  }
  if (!identical(names(operations), civil_date_operation_columns())) {
    stop(
      "operations must contain exactly the approved civil-date fields in the required order.",
      call. = FALSE
    )
  }
  operations <- as.data.frame(operations, stringsAsFactors = FALSE)
  if (nrow(operations) == 0L) {
    stop("operations must contain at least one approved civil-date operation.", call. = FALSE)
  }

  character_fields <- c(
    "source_variable_key", "derived_name", "operation_state",
    "declared_semantics", "approval_id"
  )
  if (!all(vapply(operations[character_fields], is.character, logical(1)))) {
    stop("Civil-date character fields must use character vectors.", call. = FALSE)
  }
  logical_fields <- c("preserve_source", "require_midnight")
  if (!all(vapply(operations[logical_fields], is.logical, logical(1)))) {
    stop("Civil-date safeguards must use logical vectors.", call. = FALSE)
  }
  if (anyNA(operations[c(character_fields, logical_fields)])) {
    stop("Approved civil-date fields must not be missing.", call. = FALSE)
  }

  valid_keys <- validUTF8(operations$source_variable_key)
  valid_keys[valid_keys] <- grepl(
    "^var_[a-z0-9]{16,64}$",
    operations$source_variable_key[valid_keys]
  )
  if (!all(valid_keys) || anyDuplicated(operations$source_variable_key)) {
    stop(
      "Approved civil-date operations require unique caller-managed opaque source keys.",
      call. = FALSE
    )
  }
  valid_names <- validUTF8(operations$derived_name)
  valid_names[valid_names] <- trimws(operations$derived_name[valid_names]) != ""
  if (!all(valid_names) || anyDuplicated(operations$derived_name)) {
    stop(
      "Approved civil-date operations require unique non-empty derived names.",
      call. = FALSE
    )
  }
  if (!all(operations$operation_state == "approved")) {
    stop(
      "Every executable civil-date operation must have operation_state equal to approved.",
      call. = FALSE
    )
  }
  if (!all(operations$declared_semantics == "civil_date")) {
    stop(
      "Every executable civil-date operation must explicitly declare civil_date semantics.",
      call. = FALSE
    )
  }
  if (!all(operations$preserve_source)) {
    stop("Every civil-date operation must preserve its source.", call. = FALSE)
  }
  if (!all(operations$require_midnight)) {
    stop("Every civil-date operation must require exact midnight.", call. = FALSE)
  }

  valid_approvals <- validUTF8(operations$approval_id)
  valid_approvals[valid_approvals] <- grepl(
    "^approval_[a-z0-9]{16,64}$",
    operations$approval_id[valid_approvals]
  )
  if (!all(valid_approvals)) {
    stop(
      "Approved civil-date operations require opaque approval identifiers matching the required pattern.",
      call. = FALSE
    )
  }

  operations <- operations[
    order(operations$source_variable_key, method = "radix"), ,
    drop = FALSE
  ]
  row.names(operations) <- NULL
  class(operations) <- c("epi_eda_approved_civil_dates", "data.frame")
  operations
}

#' @export
print.epi_eda_approved_civil_dates <- function(x, ...) {
  cat("<episcout approved civil-date operations>\n")
  cat("  approved operations: ", nrow(x), "\n", sep = "")
  cat("  source, derived and approval identities: <not displayed>\n")
  invisible(x)
}

#' @export
str.epi_eda_approved_civil_dates <- function(object, ...) {
  cat("<episcout approved civil-date operations>\n")
  cat("  approved operations: ", nrow(object), "\n", sep = "")
  cat("  source, derived and approval identities: <not displayed>\n")
  invisible(object)
}

#' Derive reviewed civil dates from local timestamps
#'
#' Apply separately approved civil-date operations to strict local timestamps
#' in a data frame or PostgreSQL source without modifying or replacing the
#' source. Every non-missing source value must be exact midnight before any
#' derived column or destination is created.
#'
#' @param data A data frame or an [epi_eda_postgres_source()].
#' @param operations An unmodified object returned by
#'   [epi_eda_approved_civil_dates()].
#' @param variable_keys A caller-owned data frame containing exactly `name` and
#'   `variable_key`. Every approved source key must resolve exactly once.
#'   Additional mappings are permitted.
#' @param output_path For a data-frame source, an optional new destination file.
#' @param output_format When `output_path` is supplied, exactly `"csv"` or
#'   `"rds"`. The format is never inferred.
#' @param destination_schema For a PostgreSQL source, the existing schema in
#'   which to create the new destination table.
#' @param destination_table For a PostgreSQL source, the new table name.
#'
#' @return An `epi_eda_civil_date_result` list with `data` and `audit`. For a
#'   data-frame source, `data` is the complete derived data frame, including
#'   when it was also exported. For PostgreSQL, `data` is `NULL` because rows
#'   remain server-side. `audit` contains one aggregate `summary` row and an
#'   opaque-keyed `operations` table with source and derived missing counts,
#'   dimensions, a deterministic operation hash and reconciliation flags.
#'
#' @details Civil-date meaning must be declared in the approved operation;
#'   storage and observed values never establish that meaning. In memory,
#'   supported local timestamps are exact base character vectors using
#'   `YYYY-MM-DD HH:MM:SS` with an optional decimal fraction. Years 0001 through
#'   9999 and valid proleptic Gregorian calendar dates are supported. Offset,
#'   zone and `T` syntax and `POSIXct`/`POSIXlt` vectors are rejected rather than
#'   interpreted. PostgreSQL support is limited to `timestamp without time
#'   zone`; the existing rejection of that storage for instant-oriented
#'   preparation is unchanged.
#'
#'   Exact midnight requires zero hour, minute and second and only zero digits
#'   in an optional fraction. If any non-missing value fails, the complete call
#'   is blocked and the error reports only the aggregate affected-value count.
#'   PostgreSQL non-finite timestamps count as affected values. Missing sources
#'   produce missing `Date` values. Successful derivation appends separate
#'   columns and leaves every source value unchanged.
#'
#'   Existing file destinations are never replaced. PostgreSQL validation,
#'   aggregate midnight counting, creation and reconciliation occur in one
#'   repeatable-read transaction; failure rolls back the new table. Neither
#'   backend assigns a timezone, converts a timezone or treats a local timestamp
#'   as an instant. Returned audits and display methods omit source and derived
#'   names, paths, relation identities, timestamps and approval references.
#'   Aggregate counts and hashes can still be sensitive in context.
#'
#' @export
epi_eda_derive_civil_dates <- function(data,
                                       operations,
                                       variable_keys,
                                       output_path = NULL,
                                       output_format = NULL,
                                       destination_schema = NULL,
                                       destination_table = NULL) {
  operations <- civil_revalidate_operations(operations)
  source_names <- civil_validate_keys(
    variable_keys,
    operations$source_variable_key
  )
  operation_hash <- eda_postgres_fingerprint(
    civil_date_plain_operations(operations)
  )

  if (inherits(data, "epi_eda_postgres_source")) {
    clean_validate_pg_arguments(
      output_path,
      output_format,
      destination_schema,
      destination_table
    )
    return(civil_date_apply_postgres(
      data,
      operations,
      source_names,
      operation_hash,
      destination_schema,
      destination_table
    ))
  }

  clean_validate_data_frame(data)
  publication <- clean_validate_file_arguments(
    data,
    output_path,
    output_format,
    destination_schema,
    destination_table
  )
  civil_date_apply_data_frame(
    data,
    operations,
    source_names,
    operation_hash,
    publication
  )
}

civil_date_plain_operations <- function(operations) {
  out <- operations
  class(out) <- "data.frame"
  out
}

civil_revalidate_operations <- function(operations) {
  valid_class <- inherits(operations, "epi_eda_approved_civil_dates") &&
    identical(
      class(operations),
      c("epi_eda_approved_civil_dates", "data.frame")
    )
  if (!valid_class) {
    stop(
      "operations must be an unmodified object returned by epi_eda_approved_civil_dates().",
      call. = FALSE
    )
  }
  rebuilt <- tryCatch(
    epi_eda_approved_civil_dates(civil_date_plain_operations(operations)),
    error = function(error) NULL
  )
  if (is.null(rebuilt) || !identical(operations, rebuilt)) {
    stop(
      "operations must be an unmodified object returned by epi_eda_approved_civil_dates().",
      call. = FALSE
    )
  }
  operations
}

civil_validate_keys <- function(variable_keys, source_keys) {
  valid_shape <- is.data.frame(variable_keys) &&
    identical(names(variable_keys), c("name", "variable_key")) &&
    is.character(variable_keys$name) &&
    is.character(variable_keys$variable_key)
  if (!valid_shape) {
    stop(
      "variable_keys must contain exactly character name and variable_key fields.",
      call. = FALSE
    )
  }
  valid_names <- !is.na(variable_keys$name) & validUTF8(variable_keys$name)
  valid_names[valid_names] <- trimws(variable_keys$name[valid_names]) != ""
  valid_keys <- !is.na(variable_keys$variable_key) &
    validUTF8(variable_keys$variable_key)
  valid_keys[valid_keys] <- grepl(
    "^var_[a-z0-9]{16,64}$",
    variable_keys$variable_key[valid_keys]
  )
  unique_names <- !anyDuplicated(variable_keys$name)
  unique_keys <- !anyDuplicated(variable_keys$variable_key)
  if (!all(valid_names) || !all(valid_keys) || !unique_names || !unique_keys) {
    stop(
      "variable_keys must contain unique names and opaque variable keys.",
      call. = FALSE
    )
  }
  matched <- match(source_keys, variable_keys$variable_key)
  if (anyNA(matched)) {
    stop(
      "variable_keys must resolve every approved civil-date source exactly once.",
      call. = FALSE
    )
  }
  variable_keys$name[matched]
}

civil_date_parse_local <- function(values) {
  if (!identical(class(values), "character")) {
    stop(
      "A civil-date source is not strict timezone-free character storage.",
      call. = FALSE
    )
  }
  non_missing <- !is.na(values)
  dates <- as.Date(rep(NA_character_, length(values)))
  non_midnight <- rep(FALSE, length(values))
  if (!any(non_missing)) {
    return(list(dates = dates, non_midnight = non_midnight))
  }

  text <- values[non_missing]
  valid_encoding <- validUTF8(text)
  syntax <- rep(FALSE, length(text))
  syntax[valid_encoding] <- grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?$",
    text[valid_encoding]
  )
  if (!all(syntax)) {
    stop(
      "A civil-date source contains malformed or non-local timestamp text.",
      call. = FALSE
    )
  }

  year <- as.integer(substr(text, 1L, 4L))
  month <- as.integer(substr(text, 6L, 7L))
  day <- as.integer(substr(text, 9L, 10L))
  hour <- as.integer(substr(text, 12L, 13L))
  minute <- as.integer(substr(text, 15L, 16L))
  second <- as.integer(substr(text, 18L, 19L))
  valid_month <- month >= 1L & month <= 12L
  max_day <- rep(NA_integer_, length(month))
  month_days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
  max_day[valid_month] <- month_days[month[valid_month]]
  leap <- year %% 4L == 0L & (year %% 100L != 0L | year %% 400L == 0L)
  february <- valid_month & month == 2L & leap
  max_day[february] <- 29L
  valid_calendar <- year >= 1L & valid_month & day >= 1L &
    !is.na(max_day) & day <= max_day & hour >= 0L & hour <= 23L &
    minute >= 0L & minute <= 59L & second >= 0L & second <= 59L
  if (!all(valid_calendar)) {
    stop(
      "A civil-date source contains an invalid local calendar timestamp.",
      call. = FALSE
    )
  }

  fraction <- substring(text, 20L)
  fractional_nonzero <- nzchar(fraction) & grepl("[1-9]", fraction)
  non_midnight[non_missing] <- hour != 0L | minute != 0L | second != 0L |
    fractional_nonzero
  dates[non_missing] <- as.Date(substr(text, 1L, 10L), format = "%Y-%m-%d")
  list(dates = dates, non_midnight = non_midnight)
}

civil_date_memory_plans <- function(data, operations, source_names) {
  if (any(operations$derived_name %in% names(data))) {
    stop(
      "Every civil-date derived name must be new in the source data.",
      call. = FALSE
    )
  }
  plans <- vector("list", nrow(operations))
  for (index in seq_len(nrow(operations))) {
    name <- source_names[[index]]
    if (!name %in% names(data)) {
      stop(
        "Every approved civil-date operation must resolve to a present source variable.",
        call. = FALSE
      )
    }
    parsed <- civil_date_parse_local(data[[name]])
    plans[[index]] <- list(
      source_name = name,
      derived_name = operations$derived_name[[index]],
      source_key = operations$source_variable_key[[index]],
      dates = parsed$dates,
      n_non_midnight = sum(parsed$non_midnight),
      n_missing_source = sum(is.na(data[[name]]))
    )
  }
  plans
}

civil_date_midnight_error <- function(n_affected) {
  stop(
    n_affected,
    " non-missing source values are not exact midnight; no civil dates were derived.",
    call. = FALSE
  )
}

civil_date_apply_data_frame <- function(data,
                                        operations,
                                        source_names,
                                        operation_hash,
                                        publication) {
  plans <- civil_date_memory_plans(data, operations, source_names)
  n_non_midnight <- clean_total_count(vapply(
    plans,
    `[[`,
    numeric(1),
    "n_non_midnight"
  ))
  if (n_non_midnight > 0L) {
    civil_date_midnight_error(n_non_midnight)
  }

  processed <- as.data.frame(data, stringsAsFactors = FALSE)
  operation_rows <- vector("list", length(plans))
  for (index in seq_along(plans)) {
    plan <- plans[[index]]
    processed[[plan$derived_name]] <- plan$dates
    operation_rows[[index]] <- civil_date_operation_audit_row(
      plan$source_key,
      nrow(data),
      plan$n_missing_source,
      sum(is.na(plan$dates))
    )
  }

  operation_audit <- civil_bind_operation_audit(operation_rows)
  civil_require_reconciliation(
    operation_audit,
    nrow(data),
    ncol(data),
    nrow(processed),
    ncol(processed),
    nrow(operations)
  )
  publication_dimensions <- c(nrow(processed), ncol(processed))
  if (!identical(publication$kind, "memory")) {
    publication_dimensions <- clean_publish_file(
      processed,
      publication$path,
      publication$kind
    )
  }
  summary <- civil_date_audit_summary(
    operation_hash,
    publication$kind,
    nrow(data),
    ncol(data),
    publication_dimensions[[1L]],
    publication_dimensions[[2L]],
    operation_audit,
    nrow(operations),
    publication_reconciled = TRUE
  )
  civil_date_result(processed, summary, operation_audit)
}

civil_date_operation_audit_row <- function(key,
                                           n,
                                           missing_source,
                                           missing_derived) {
  n <- clean_checked_count(n)
  missing_source <- clean_checked_count(missing_source)
  missing_derived <- clean_checked_count(missing_derived)
  data.frame(
    source_variable_key = as.character(key),
    n = n,
    n_missing_source = missing_source,
    n_missing_derived = missing_derived,
    reconciled = missing_source == missing_derived && missing_derived <= n,
    stringsAsFactors = FALSE
  )
}

civil_bind_operation_audit <- function(rows) {
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

civil_require_reconciliation <- function(operations,
                                         source_rows,
                                         source_columns,
                                         destination_rows,
                                         destination_columns,
                                         n_operations) {
  dimensions <- source_rows == destination_rows &&
    destination_columns == source_columns + n_operations
  if (!isTRUE(dimensions) || !all(operations$reconciled)) {
    stop(
      "Civil-date dimensions or missingness failed reconciliation.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

civil_date_audit_summary <- function(operation_hash,
                                     publication,
                                     source_rows,
                                     source_columns,
                                     destination_rows,
                                     destination_columns,
                                     operations,
                                     n_operations,
                                     publication_reconciled) {
  data.frame(
    operation_set_sha256 = as.character(operation_hash),
    publication = as.character(publication),
    source_rows = clean_checked_count(source_rows),
    source_columns = clean_checked_count(source_columns),
    destination_rows = clean_checked_count(destination_rows),
    destination_columns = clean_checked_count(destination_columns),
    n_operations = clean_checked_count(n_operations),
    n_missing_source = clean_total_count(operations$n_missing_source),
    n_missing_derived = clean_total_count(operations$n_missing_derived),
    n_non_midnight = 0L,
    dimensions_reconciled = source_rows == destination_rows &&
      destination_columns == source_columns + n_operations,
    missingness_reconciled = all(operations$reconciled),
    publication_reconciled = isTRUE(publication_reconciled),
    stringsAsFactors = FALSE
  )
}

civil_date_result <- function(data, summary, operations) {
  structure(
    list(data = data, audit = list(summary = summary, operations = operations)),
    class = c("epi_eda_civil_date_result", "list")
  )
}

#' @export
print.epi_eda_civil_date_result <- function(x, ...) {
  cat("<episcout approved civil-date result>\n")
  cat("  publication: ", x$audit$summary$publication[[1L]], "\n", sep = "")
  cat("  rows: ", x$audit$summary$destination_rows[[1L]], "\n", sep = "")
  cat("  approved operations: ", nrow(x$audit$operations), "\n", sep = "")
  cat("  source, derived, destination and timestamp values: <not displayed>\n")
  invisible(x)
}

#' @export
str.epi_eda_civil_date_result <- function(object, ...) {
  cat("<episcout approved civil-date result>\n")
  cat("  publication: ", object$audit$summary$publication[[1L]], "\n", sep = "")
  dimensions <- paste0(
    object$audit$summary$destination_rows[[1L]],
    " rows x ",
    object$audit$summary$destination_columns[[1L]],
    " columns"
  )
  cat("  dimensions: ", dimensions, "\n", sep = "")
  cat("  approved operations: ", nrow(object$audit$operations), "\n", sep = "")
  cat("  source, derived, destination, keys and timestamp values: <not displayed>\n")
  invisible(object)
}

civil_date_pg_plan <- function(source, key, source_name, derived_name) {
  column <- eda_postgres_column(source, source_name)
  compatible <- !is.null(column) &&
    eda_postgres_storage_family(column) == "local_datetime"
  if (!compatible) {
    stop(
      "An approved civil-date operation is incompatible with PostgreSQL source storage.",
      call. = FALSE
    )
  }
  source_sql <- eda_postgres_column_sql(source, source_name)
  derived_sql <- as.character(DBI::dbQuoteIdentifier(source$con, derived_name))
  list(
    key = key,
    source_name = source_name,
    derived_name = derived_name,
    source_sql = source_sql,
    derived_sql = derived_sql,
    source_missing = paste0("(", source_sql, " IS NULL)"),
    derived_missing = paste0("(", derived_sql, " IS NULL)"),
    non_midnight = paste0(
      "(CASE WHEN ", source_sql, " IS NULL THEN FALSE ",
      "WHEN NOT isfinite(", source_sql, ") THEN TRUE ELSE ",
      source_sql, "::time <> TIME '00:00:00' END)"
    )
  )
}

civil_date_pg_source_audit <- function(source, plan) {
  observed <- clean_pg_fetch(
    source$con,
    paste0(
      "SELECT count(*) FILTER (WHERE ", plan$source_missing,
      ")::text AS n_missing_source, count(*) FILTER (WHERE ",
      plan$non_midnight, ")::text AS n_non_midnight FROM ",
      eda_postgres_table_sql(source)
    ),
    kind = "civil_date_source_audit"
  )
  if (!identical(
    names(observed),
    c("n_missing_source", "n_non_midnight")
  )) {
    stop(
      "PostgreSQL civil-date audit returned an invalid scalar schema.",
      call. = FALSE
    )
  }
  c(
    n_missing_source = eda_checked_count(
      observed$n_missing_source[[1L]],
      "PostgreSQL civil-date count"
    ),
    n_non_midnight = eda_checked_count(
      observed$n_non_midnight[[1L]],
      "PostgreSQL civil-date count"
    )
  )
}

civil_pg_destination_missing <- function(con, table_sql, plan) {
  observed <- clean_pg_fetch(
    con,
    paste0(
      "SELECT count(*) FILTER (WHERE ", plan$derived_missing,
      ")::text AS n_missing_derived FROM ", table_sql
    ),
    kind = "civil_date_destination_audit"
  )
  if (!identical(names(observed), "n_missing_derived")) {
    stop(
      "PostgreSQL civil-date audit returned an invalid scalar schema.",
      call. = FALSE
    )
  }
  eda_checked_count(
    observed$n_missing_derived[[1L]],
    "PostgreSQL civil-date count"
  )
}

civil_date_pg_create_statement <- function(source, destination_sql, plans) {
  source_fields <- vapply(
    source$columns$name,
    function(name) eda_postgres_column_sql(source, name),
    character(1)
  )
  derived_fields <- vapply(plans, function(plan) {
    paste0(plan$source_sql, "::date AS ", plan$derived_sql)
  }, character(1))
  paste0(
    "CREATE TABLE ", destination_sql, " AS SELECT ",
    paste(c(source_fields, derived_fields), collapse = ", "),
    " FROM ", eda_postgres_table_sql(source)
  )
}

civil_date_apply_postgres <- function(source,
                                      operations,
                                      source_names,
                                      operation_hash,
                                      destination_schema,
                                      destination_table) {
  eda_validate_postgres_source(source, require_idle = TRUE)
  destination_schema <- eda_postgres_identifier(
    destination_schema,
    "destination_schema"
  )
  destination_table <- eda_postgres_identifier(
    destination_table,
    "destination_table"
  )
  system_schema <- destination_schema %in% c("pg_catalog", "information_schema") ||
    grepl("^pg_(temp|toast)", destination_schema)
  if (system_schema) {
    stop(
      "The PostgreSQL destination must use a caller-owned permanent schema.",
      call. = FALSE
    )
  }
  same_destination <- identical(destination_schema, source$schema) &&
    identical(destination_table, source$relation)
  if (same_destination) {
    stop(
      "The PostgreSQL destination must differ from the source relation.",
      call. = FALSE
    )
  }

  con <- source$con
  eda_db_lifecycle_call(
    eda_db_begin(con),
    "PostgreSQL civil-date transaction could not begin; review restricted database logs."
  )
  finished <- FALSE
  on.exit(
    {
      if (!finished && DBI::dbIsValid(con)) {
        try(DBI::dbRollback(con), silent = TRUE)
      }
    },
    add = TRUE
  )
  clean_pg_execute(
    con,
    "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ",
    kind = "civil-date transaction setup"
  )
  eda_validate_postgres_source(source, require_idle = FALSE)
  destination_state <- clean_pg_destination_state(
    con,
    destination_schema,
    destination_table
  )
  if (!isTRUE(destination_state$schema_exists[[1L]])) {
    stop("The PostgreSQL destination schema does not exist.", call. = FALSE)
  }
  if (isTRUE(destination_state$relation_exists[[1L]])) {
    stop(
      "The PostgreSQL destination already exists and will not be replaced.",
      call. = FALSE
    )
  }
  if (any(operations$derived_name %in% source$columns$name)) {
    stop(
      "Every civil-date derived name must be new in the PostgreSQL source.",
      call. = FALSE
    )
  }

  plans <- lapply(seq_len(nrow(operations)), function(index) {
    civil_date_pg_plan(
      source,
      operations$source_variable_key[[index]],
      source_names[[index]],
      operations$derived_name[[index]]
    )
  })
  source_rows <- eda_postgres_row_count(source)
  before <- lapply(
    plans,
    function(plan) civil_date_pg_source_audit(source, plan)
  )
  n_non_midnight <- clean_total_count(vapply(
    before,
    `[[`,
    numeric(1),
    "n_non_midnight"
  ))
  if (n_non_midnight > 0L) {
    civil_date_midnight_error(n_non_midnight)
  }

  destination_sql <- clean_pg_quote_table(
    con,
    destination_schema,
    destination_table
  )
  clean_pg_execute(
    con,
    civil_date_pg_create_statement(source, destination_sql, plans),
    kind = "civil-date destination creation"
  )

  destination_catalogue <- clean_pg_destination_catalogue(
    con,
    destination_schema,
    destination_table
  )
  expected_names <- c(
    as.character(source$columns$name),
    operations$derived_name
  )
  same_names <- identical(
    as.character(destination_catalogue$columns$name),
    expected_names
  )
  derived_indices <- match(
    operations$derived_name,
    destination_catalogue$columns$name
  )
  derived_are_dates <- !anyNA(derived_indices) && all(vapply(
    derived_indices,
    function(index) {
      eda_postgres_storage_family(
        destination_catalogue$columns[index, , drop = FALSE]
      ) == "date"
    },
    logical(1)
  ))
  if (!same_names || !derived_are_dates) {
    stop(
      "PostgreSQL civil-date destination columns failed reconciliation.",
      call. = FALSE
    )
  }

  destination_rows <- clean_pg_fetch(
    con,
    paste0("SELECT count(*)::text AS n FROM ", destination_sql),
    kind = "civil_date_destination_dimensions"
  )
  if (!identical(names(destination_rows), "n")) {
    stop(
      "PostgreSQL civil-date dimensions returned an invalid scalar schema.",
      call. = FALSE
    )
  }
  destination_rows <- eda_checked_count(
    destination_rows$n[[1L]],
    "PostgreSQL civil-date destination row count"
  )
  destination_columns <- nrow(destination_catalogue$columns)

  operation_rows <- vector("list", length(plans))
  for (index in seq_along(plans)) {
    missing_derived <- civil_pg_destination_missing(
      con,
      destination_sql,
      plans[[index]]
    )
    operation_rows[[index]] <- civil_date_operation_audit_row(
      operations$source_variable_key[[index]],
      source_rows,
      before[[index]][["n_missing_source"]],
      missing_derived
    )
  }
  operation_audit <- civil_bind_operation_audit(operation_rows)
  civil_require_reconciliation(
    operation_audit,
    source_rows,
    nrow(source$columns),
    destination_rows,
    destination_columns,
    nrow(operations)
  )
  summary <- civil_date_audit_summary(
    operation_hash,
    "postgresql",
    source_rows,
    nrow(source$columns),
    destination_rows,
    destination_columns,
    operation_audit,
    nrow(operations),
    publication_reconciled = TRUE
  )
  eda_db_lifecycle_call(
    eda_db_commit(con),
    "PostgreSQL civil-date transaction could not commit safely; review restricted database logs."
  )
  finished <- TRUE
  civil_date_result(NULL, summary, operation_audit)
}
