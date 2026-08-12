context("reviewed civil-date derivation")

civil_date_test_keys <- function(name) {
  data.frame(
    name = name,
    variable_key = sprintf("var_%016d", seq_along(name)),
    stringsAsFactors = FALSE
  )
}

civil_date_test_operations <- function(source_variable_key,
                                       derived_name,
                                       operation_state = rep(
                                         "approved",
                                         length(source_variable_key)
                                       ),
                                       declared_semantics = rep(
                                         "civil_date",
                                         length(source_variable_key)
                                       ),
                                       preserve_source = rep(
                                         TRUE,
                                         length(source_variable_key)
                                       ),
                                       require_midnight = rep(
                                         TRUE,
                                         length(source_variable_key)
                                       ),
                                       approval_id = rep(
                                         "approval_0000000000000001",
                                         length(source_variable_key)
                                       )) {
  data.frame(
    source_variable_key = source_variable_key,
    derived_name = derived_name,
    operation_state = operation_state,
    declared_semantics = declared_semantics,
    preserve_source = preserve_source,
    require_midnight = require_midnight,
    approval_id = approval_id,
    stringsAsFactors = FALSE
  )
}

civil_date_fixture <- function() {
  data <- data.frame(
    sequence = 1:4,
    local_stamp = c(
      "2024-02-29 00:00:00",
      "2024-12-31 00:00:00.000",
      NA_character_,
      "2025-01-01 00:00:00"
    ),
    stringsAsFactors = FALSE,
    row.names = paste0("row_", 1:4)
  )
  keys <- civil_date_test_keys(names(data))
  source_key <- keys$variable_key[keys$name == "local_stamp"]
  operations <- epi_eda_approved_civil_dates(
    civil_date_test_operations(source_key, "reviewed_date")
  )
  list(data = data, keys = keys, operations = operations)
}

test_that("approved civil-date operations have an exact explicit schema", {
  expect_identical(names(formals(epi_eda_approved_civil_dates)), "operations")
  expect_identical(
    names(formals(epi_eda_derive_civil_dates)),
    c(
      "data", "operations", "variable_keys", "output_path", "output_format",
      "destination_schema", "destination_table"
    )
  )
  raw <- civil_date_test_operations(
    c("var_0000000000000002", "var_0000000000000001"),
    c("date_b", "date_a")
  )
  observed <- epi_eda_approved_civil_dates(raw)

  expect_identical(
    class(observed),
    c("epi_eda_approved_civil_dates", "data.frame")
  )
  expect_named(
    observed,
    c(
      "source_variable_key", "derived_name", "operation_state",
      "declared_semantics", "preserve_source", "require_midnight",
      "approval_id"
    )
  )
  expect_identical(
    observed$source_variable_key,
    c("var_0000000000000001", "var_0000000000000002")
  )
  expect_identical(observed$derived_name, c("date_a", "date_b"))
  expect_true(all(observed$preserve_source))
  expect_true(all(observed$require_midnight))
})

test_that("implicit pending malformed and unsafe operations are rejected privately", {
  base <- civil_date_test_operations(
    "var_0000000000000001",
    "derived_date"
  )
  invalid <- list(
    transform(base, operation_state = "pending"),
    transform(base, declared_semantics = "SEMANTICS_CANARY"),
    transform(base, preserve_source = FALSE),
    transform(base, require_midnight = FALSE),
    transform(base, preserve_source = NA),
    transform(base, source_variable_key = "KEY_CANARY"),
    transform(base, derived_name = ""),
    transform(base, approval_id = "APPROVAL_CANARY"),
    cbind(base, extra_canary = "EXTRA_CANARY", stringsAsFactors = FALSE)
  )
  for (value in invalid) {
    error <- tryCatch(
      epi_eda_approved_civil_dates(value),
      error = identity
    )
    expect_s3_class(error, "error")
    expect_false(grepl("CANARY|derived_date", conditionMessage(error)))
  }

  duplicated_key <- rbind(base, transform(base, derived_name = "date_b"))
  expect_error(epi_eda_approved_civil_dates(duplicated_key), "unique")
  duplicated_name <- rbind(
    base,
    transform(base, source_variable_key = "var_0000000000000002")
  )
  expect_error(epi_eda_approved_civil_dates(duplicated_name), "unique")
  expect_error(
    epi_eda_approved_civil_dates(base[FALSE, , drop = FALSE]),
    "at least one"
  )
  expect_error(
    epi_eda_approved_civil_dates(list()),
    "must be a data frame"
  )
  expect_error(
    epi_eda_approved_civil_dates(transform(base, source_variable_key = 1)),
    "character fields"
  )
  expect_error(
    epi_eda_approved_civil_dates(transform(base, preserve_source = 1)),
    "logical vectors"
  )
})

test_that("operation objects and source key maps are revalidated before use", {
  fixture <- civil_date_fixture()
  expect_error(
    epi_eda_derive_civil_dates(
      fixture$data,
      civil_date_test_operations(
        "var_0000000000000002",
        "reviewed_date"
      ),
      fixture$keys
    ),
    "unmodified object"
  )
  mutated <- fixture$operations
  mutated$declared_semantics[[1]] <- "calendar_guess"
  expect_error(
    epi_eda_derive_civil_dates(fixture$data, mutated, fixture$keys),
    "unmodified object"
  )

  expect_error(
    epi_eda_derive_civil_dates(
      fixture$data,
      fixture$operations,
      fixture$keys["name"]
    ),
    "exactly character"
  )
  invalid_keys <- fixture$keys
  invalid_keys$variable_key[[1]] <- "INVALID_KEY_CANARY"
  error <- tryCatch(
    epi_eda_derive_civil_dates(
      fixture$data,
      fixture$operations,
      invalid_keys
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_false(grepl("CANARY", conditionMessage(error)))
  unresolved <- fixture$keys[fixture$keys$name == "sequence", , drop = FALSE]
  expect_error(
    epi_eda_derive_civil_dates(
      fixture$data,
      fixture$operations,
      unresolved
    ),
    "resolve every"
  )
  absent <- fixture$keys
  absent$name[absent$name == "local_stamp"] <- "absent_stamp"
  expect_error(
    epi_eda_derive_civil_dates(
      fixture$data,
      fixture$operations,
      absent
    ),
    "present source variable"
  )
})

test_that("in-memory derivation preserves source values and literal dates", {
  fixture <- civil_date_fixture()
  source_before <- fixture$data
  operations_before <- fixture$operations
  keys_before <- fixture$keys
  observed <- epi_eda_derive_civil_dates(
    fixture$data,
    fixture$operations,
    fixture$keys
  )

  expect_identical(class(observed), c("epi_eda_civil_date_result", "list"))
  expect_named(observed, c("data", "audit"))
  expect_named(observed$audit, c("summary", "operations"))
  expect_named(
    observed$audit$summary,
    c(
      "operation_set_sha256", "publication", "source_rows",
      "source_columns", "destination_rows", "destination_columns",
      "n_operations", "n_missing_source", "n_missing_derived",
      "n_non_midnight", "dimensions_reconciled",
      "missingness_reconciled", "publication_reconciled"
    )
  )
  expect_named(
    observed$audit$operations,
    c(
      "source_variable_key", "n", "n_missing_source",
      "n_missing_derived", "reconciled"
    )
  )
  expect_identical(names(observed$data), c(names(fixture$data), "reviewed_date"))
  expect_identical(observed$data$local_stamp, fixture$data$local_stamp)
  expect_identical(
    observed$data$reviewed_date,
    as.Date(c("2024-02-29", "2024-12-31", NA, "2025-01-01"))
  )
  expect_identical(row.names(observed$data), row.names(fixture$data))
  expect_identical(fixture$data, source_before)
  expect_identical(fixture$operations, operations_before)
  expect_identical(fixture$keys, keys_before)

  summary <- observed$audit$summary
  expect_identical(summary$publication, "memory")
  expect_identical(
    unname(as.integer(unlist(summary[c(
      "source_rows", "source_columns", "destination_rows",
      "destination_columns", "n_operations", "n_missing_source",
      "n_missing_derived", "n_non_midnight"
    )], use.names = FALSE))),
    c(4L, 2L, 4L, 3L, 1L, 1L, 1L, 0L)
  )
  expect_true(all(unlist(summary[c(
    "dimensions_reconciled", "missingness_reconciled",
    "publication_reconciled"
  )])))
  expect_identical(observed$audit$operations$n, 4L)
  expect_identical(observed$audit$operations$n_missing_source, 1L)
  expect_identical(observed$audit$operations$n_missing_derived, 1L)
  expect_true(observed$audit$operations$reconciled)
})

test_that("zero rows and all-missing local timestamps preserve missingness", {
  keys <- civil_date_test_keys("local_stamp")
  operations <- epi_eda_approved_civil_dates(civil_date_test_operations(
    keys$variable_key,
    "reviewed_date"
  ))
  zero <- epi_eda_derive_civil_dates(
    data.frame(local_stamp = character()),
    operations,
    keys
  )
  expect_identical(dim(zero$data), c(0L, 2L))
  expect_s3_class(zero$data$reviewed_date, "Date")
  expect_identical(zero$audit$operations$n, 0L)
  expect_identical(zero$audit$operations$n_missing_derived, 0L)

  missing_source <- data.frame(
    local_stamp = rep(NA_character_, 3L),
    stringsAsFactors = FALSE
  )
  missing <- epi_eda_derive_civil_dates(
    missing_source,
    operations,
    keys
  )
  expect_identical(missing$data$local_stamp, missing_source$local_stamp)
  expect_true(all(is.na(missing$data$reviewed_date)))
  expect_s3_class(missing$data$reviewed_date, "Date")
  expect_identical(missing$audit$operations$n_missing_source, 3L)
  expect_identical(missing$audit$operations$n_missing_derived, 3L)
})

test_that("non-midnight values block atomically with one aggregate count", {
  data <- data.frame(
    local_a = c(
      "2024-01-01 00:00:00",
      "2024-01-02 00:00:01",
      "2024-01-03 00:00:00.001"
    ),
    local_b = c(
      "2024-01-01 00:00:00.000",
      "2024-01-02 12:00:00",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  source_before <- data
  keys <- civil_date_test_keys(names(data))
  operations <- epi_eda_approved_civil_dates(civil_date_test_operations(
    keys$variable_key,
    c("date_a", "date_b")
  ))
  directory <- tempfile("civil-date-block-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  target <- file.path(directory, "blocked.rds")

  error <- tryCatch(
    epi_eda_derive_civil_dates(
      data,
      operations,
      keys,
      output_path = target,
      output_format = "rds"
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_identical(
    conditionMessage(error),
    "3 non-missing source values are not exact midnight; no civil dates were derived."
  )
  expect_false(grepl("2024|local_|date_", conditionMessage(error)))
  expect_identical(data, source_before)
  expect_false(any(c("date_a", "date_b") %in% names(data)))
  expect_false(file.exists(target))
  expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("malformed non-local storage and destination collisions fail privately", {
  keys <- civil_date_test_keys("local_stamp")
  operations <- epi_eda_approved_civil_dates(civil_date_test_operations(
    keys$variable_key,
    "reviewed_date"
  ))
  invalid_values <- c(
    "2023-02-29 00:00:00",
    "2024-13-01 00:00:00",
    "2024-01-01 24:00:00",
    "2024-01-01T00:00:00",
    "2024-01-01 00:00:00Z",
    "2024-01-01 00:00:00+01:00"
  )
  for (value in invalid_values) {
    data <- data.frame(local_stamp = value, stringsAsFactors = FALSE)
    error <- tryCatch(
      epi_eda_derive_civil_dates(data, operations, keys),
      error = identity
    )
    expect_s3_class(error, "error")
    expect_false(grepl("202[34]|local_stamp|reviewed_date", conditionMessage(error)))
    expect_identical(names(data), "local_stamp")
  }

  instant <- data.frame(
    local_stamp = as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  )
  expect_error(
    epi_eda_derive_civil_dates(instant, operations, keys),
    "strict timezone-free character storage"
  )
  collision <- data.frame(
    local_stamp = "2024-01-01 00:00:00",
    reviewed_date = "SOURCE_COLUMN_CANARY",
    stringsAsFactors = FALSE
  )
  collision_keys <- civil_date_test_keys(names(collision))
  collision_operations <- epi_eda_approved_civil_dates(
    civil_date_test_operations(
      collision_keys$variable_key[collision_keys$name == "local_stamp"],
      "reviewed_date"
    )
  )
  collision_error <- tryCatch(
    epi_eda_derive_civil_dates(
      collision,
      collision_operations,
      collision_keys
    ),
    error = identity
  )
  expect_s3_class(collision_error, "error")
  expect_false(grepl("CANARY|reviewed_date|local_stamp", conditionMessage(collision_error)))
  expect_identical(collision$reviewed_date, "SOURCE_COLUMN_CANARY")
})

test_that("CSV and RDS publication is complete explicit and no-replace", {
  fixture <- civil_date_fixture()
  directory <- tempfile("civil-date-files-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rds_path <- file.path(directory, "derived.rds")
  csv_path <- file.path(directory, "derived.csv")

  rds <- epi_eda_derive_civil_dates(
    fixture$data,
    fixture$operations,
    fixture$keys,
    output_path = rds_path,
    output_format = "rds"
  )
  csv <- epi_eda_derive_civil_dates(
    fixture$data,
    fixture$operations,
    fixture$keys,
    output_path = csv_path,
    output_format = "csv"
  )
  expect_identical(readRDS(rds_path), rds$data)
  expect_identical(
    dim(utils::read.csv(csv_path, check.names = FALSE)),
    dim(csv$data)
  )
  expect_identical(rds$audit$summary$publication, "rds")
  expect_identical(csv$audit$summary$publication, "csv")

  collision <- file.path(directory, "COLLISION_PATH_CANARY")
  writeLines("ORIGINAL", collision)
  error <- tryCatch(
    epi_eda_derive_civil_dates(
      fixture$data,
      fixture$operations,
      fixture$keys,
      output_path = collision,
      output_format = "rds"
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_identical(readLines(collision), "ORIGINAL")
  expect_false(grepl("CANARY", conditionMessage(error)))
})

test_that("display methods omit source derived values and approval references", {
  fixture <- civil_date_fixture()
  result <- epi_eda_derive_civil_dates(
    fixture$data,
    fixture$operations,
    fixture$keys
  )
  operation_display <- paste(
    capture.output(print(fixture$operations)),
    capture.output(str(fixture$operations)),
    collapse = "\n"
  )
  result_display <- paste(
    capture.output(print(result)),
    capture.output(str(result)),
    collapse = "\n"
  )
  expect_false(grepl("var_|reviewed_date|approval_", operation_display))
  expect_false(grepl(
    "var_|reviewed_date|approval_|local_stamp|2024",
    result_display
  ))
  expect_match(result_display, "approved operations: 1", fixed = TRUE)
})

test_that("PostgreSQL plans use local calendar casts without timezone semantics", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- list(
    con = con,
    schema = "main",
    relation = "source_table",
    columns = data.frame(
      name = c("local_stamp", "untouched"),
      base_udt_name = c("timestamp", "text"),
      typtype = c("b", "b"),
      stringsAsFactors = FALSE
    )
  )
  plan <- getFromNamespace("civil_date_pg_plan", "episcout")(
    source,
    "var_0000000000000001",
    "local_stamp",
    "reviewed_date"
  )
  statement <- getFromNamespace(
    "civil_date_pg_create_statement",
    "episcout"
  )(source, '"destination_table"', list(plan))
  expect_match(plan$non_midnight, "::time", fixed = TRUE)
  expect_match(plan$non_midnight, "isfinite", fixed = TRUE)
  expect_match(plan$non_midnight, "TIME '00:00:00'", fixed = TRUE)
  expect_match(statement, "::date AS", fixed = TRUE)
  expect_match(statement, "untouched", fixed = TRUE)
  expect_false(grepl(
    "AT TIME|time zone|timestamptz|2024|approval_|var_",
    paste(plan$non_midnight, statement),
    ignore.case = TRUE
  ))

  source_plan <- plan
  expect_error(
    with_mocked_bindings(
      getFromNamespace("civil_date_pg_source_audit", "episcout")(
        source,
        source_plan
      ),
      clean_pg_fetch = function(...) data.frame(wrong = "0"),
      .package = "episcout"
    ),
    "invalid scalar schema"
  )
  expect_error(
    with_mocked_bindings(
      getFromNamespace("civil_pg_destination_missing", "episcout")(
        con,
        '"destination_table"',
        source_plan
      ),
      clean_pg_fetch = function(...) data.frame(wrong = "0"),
      .package = "episcout"
    ),
    "invalid scalar schema"
  )

  unreconciled <- data.frame(reconciled = FALSE)
  expect_error(
    getFromNamespace("civil_require_reconciliation", "episcout")(
      unreconciled,
      1L,
      1L,
      1L,
      2L,
      1L
    ),
    "failed reconciliation"
  )
  expect_error(
    getFromNamespace("civil_require_reconciliation", "episcout")(
      transform(unreconciled, reconciled = TRUE),
      1L,
      1L,
      1L,
      1L,
      1L
    ),
    "failed reconciliation"
  )
})

civil_date_postgres_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    testthat::skip(
      "Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL integration tests."
    )
  }
  testthat::skip_if_not_installed("RPostgres")
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    dbname = Sys.getenv("PGDATABASE", "synthetic_records"),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "")
  )
}

test_that("PostgreSQL civil-date derivation is equivalent new-only and transactional", {
  con <- civil_date_postgres_connection()
  schema <- paste0("epi_civil_date_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        if (getFromNamespace("eda_pg_is_transacting", "episcout")(con)) {
          try(DBI::dbRollback(con), silent = TRUE)
        }
        DBI::dbExecute(con, paste0("DROP SCHEMA ", schema_sql, " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))

  source_table <- "source_local_stamps"
  source_sql <- paste0(
    schema_sql,
    ".",
    as.character(DBI::dbQuoteIdentifier(con, source_table))
  )
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", source_sql, " (",
    "sequence integer, local_stamp timestamp without time zone, ",
    "instant_stamp timestamp with time zone)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", source_sql, " VALUES ",
    "(1, TIMESTAMP '2024-02-29 00:00:00', TIMESTAMPTZ '2024-02-29 00:00:00+00'),",
    "(2, TIMESTAMP '2024-12-31 00:00:00.000', TIMESTAMPTZ '2024-12-31 00:00:00+00'),",
    "(3, NULL, NULL),",
    "(4, TIMESTAMP '2025-01-01 00:00:00', TIMESTAMPTZ '2025-01-01 00:00:00+00')"
  ))
  memory_data <- data.frame(
    sequence = 1:4,
    local_stamp = c(
      "2024-02-29 00:00:00",
      "2024-12-31 00:00:00.000",
      NA_character_,
      "2025-01-01 00:00:00"
    ),
    stringsAsFactors = FALSE
  )
  keys <- civil_date_test_keys(names(memory_data))
  operations <- epi_eda_approved_civil_dates(civil_date_test_operations(
    keys$variable_key[keys$name == "local_stamp"],
    "reviewed_date"
  ))
  source <- epi_eda_postgres_source(con, schema, source_table)
  before <- DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", source_sql, " ORDER BY sequence")
  )
  memory <- epi_eda_derive_civil_dates(memory_data, operations, keys)
  database <- epi_eda_derive_civil_dates(
    source,
    operations,
    keys,
    destination_schema = schema,
    destination_table = "derived_dates"
  )
  destination_sql <- paste0(
    schema_sql,
    ".",
    as.character(DBI::dbQuoteIdentifier(con, "derived_dates"))
  )
  after <- DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", source_sql, " ORDER BY sequence")
  )
  processed <- DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", destination_sql, " ORDER BY sequence")
  )

  expect_identical(before, after)
  expect_identical(processed$sequence, memory$data$sequence)
  expect_identical(processed$reviewed_date, memory$data$reviewed_date)
  expect_null(database$data)
  expect_identical(database$audit$operations, memory$audit$operations)
  expect_identical(
    database$audit$summary$operation_set_sha256,
    memory$audit$summary$operation_set_sha256
  )
  expect_identical(database$audit$summary$publication, "postgresql")
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  expect_error(
    epi_eda_derive_civil_dates(
      source,
      operations,
      keys,
      destination_schema = "pg_catalog",
      destination_table = "blocked_system_schema"
    ),
    "caller-owned permanent schema"
  )
  expect_error(
    epi_eda_derive_civil_dates(
      source,
      operations,
      keys,
      destination_schema = schema,
      destination_table = source_table
    ),
    "differ from the source"
  )
  expect_error(
    epi_eda_derive_civil_dates(
      source,
      operations,
      keys,
      destination_schema = "absent_schema",
      destination_table = "absent_destination"
    ),
    "schema does not exist"
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  collision_error <- tryCatch(
    epi_eda_derive_civil_dates(
      source,
      operations,
      keys,
      destination_schema = schema,
      destination_table = "derived_dates"
    ),
    error = identity
  )
  expect_s3_class(collision_error, "error")
  expect_false(grepl(
    "derived|source|epi_civil_date",
    conditionMessage(collision_error),
    ignore.case = TRUE
  ))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  dimension_table <- "dimension_failure_dates"
  original_fetch <- getFromNamespace("clean_pg_fetch", "episcout")
  expect_error(
    with_mocked_bindings(
      epi_eda_derive_civil_dates(
        source,
        operations,
        keys,
        destination_schema = schema,
        destination_table = dimension_table
      ),
      clean_pg_fetch = function(con, statement, params = list(), kind) {
        if (identical(kind, "civil_date_destination_dimensions")) {
          return(data.frame(wrong = "4"))
        }
        original_fetch(con, statement, params = params, kind = kind)
      },
      .package = "episcout"
    ),
    "dimensions returned an invalid scalar schema"
  )
  expect_false(DBI::dbExistsTable(
    con,
    DBI::Id(schema = schema, table = dimension_table)
  ))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  bad_source_table <- "non_midnight_source"
  bad_source_sql <- paste0(
    schema_sql,
    ".",
    as.character(DBI::dbQuoteIdentifier(con, bad_source_table))
  )
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", bad_source_sql,
    " (sequence integer, local_stamp timestamp without time zone)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", bad_source_sql, " VALUES ",
    "(1, TIMESTAMP '2024-01-01 00:00:00'),",
    "(2, TIMESTAMP '2024-01-02 00:00:00.001'),",
    "(3, TIMESTAMP '2024-01-03 12:00:00'),",
    "(4, NULL),",
    "(5, 'infinity'::timestamp)"
  ))
  bad_keys <- civil_date_test_keys(c("sequence", "local_stamp"))
  bad_operations <- epi_eda_approved_civil_dates(civil_date_test_operations(
    bad_keys$variable_key[bad_keys$name == "local_stamp"],
    "reviewed_date"
  ))
  bad_source <- epi_eda_postgres_source(con, schema, bad_source_table)
  bad_before <- DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", bad_source_sql, " ORDER BY sequence")
  )
  bad_error <- tryCatch(
    epi_eda_derive_civil_dates(
      bad_source,
      bad_operations,
      bad_keys,
      destination_schema = schema,
      destination_table = "blocked_dates"
    ),
    error = identity
  )
  expect_s3_class(bad_error, "error")
  expect_identical(
    conditionMessage(bad_error),
    "3 non-missing source values are not exact midnight; no civil dates were derived."
  )
  expect_false(DBI::dbExistsTable(
    con,
    DBI::Id(schema = schema, table = "blocked_dates")
  ))
  expect_identical(
    DBI::dbGetQuery(
      con,
      paste0("SELECT * FROM ", bad_source_sql, " ORDER BY sequence")
    ),
    bad_before
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  instant_keys <- civil_date_test_keys(c(
    "sequence",
    "local_stamp",
    "instant_stamp"
  ))
  instant_operations <- epi_eda_approved_civil_dates(
    civil_date_test_operations(
      instant_keys$variable_key[instant_keys$name == "instant_stamp"],
      "instant_date"
    )
  )
  expect_error(
    epi_eda_derive_civil_dates(
      source,
      instant_operations,
      instant_keys,
      destination_schema = schema,
      destination_table = "instant_dates"
    ),
    "incompatible"
  )
  expect_false(DBI::dbExistsTable(
    con,
    DBI::Id(schema = schema, table = "instant_dates")
  ))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  source_name_collision <- epi_eda_approved_civil_dates(
    civil_date_test_operations(
      keys$variable_key[keys$name == "local_stamp"],
      "sequence"
    )
  )
  expect_error(
    epi_eda_derive_civil_dates(
      source,
      source_name_collision,
      keys,
      destination_schema = schema,
      destination_table = "name_collision"
    ),
    "must be new"
  )
  expect_false(DBI::dbExistsTable(
    con,
    DBI::Id(schema = schema, table = "name_collision")
  ))

  rollback_table <- "rollback_dates"
  expect_error(
    with_mocked_bindings(
      epi_eda_derive_civil_dates(
        source,
        operations,
        keys,
        destination_schema = schema,
        destination_table = rollback_table
      ),
      clean_pg_destination_catalogue = function(...) {
        list(columns = data.frame(name = "wrong"))
      },
      .package = "episcout"
    ),
    "columns failed reconciliation"
  )
  expect_false(DBI::dbExistsTable(
    con,
    DBI::Id(schema = schema, table = rollback_table)
  ))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  empty_source_table <- "empty_local_stamps"
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE ", schema_sql, ".",
      as.character(DBI::dbQuoteIdentifier(con, empty_source_table)),
      " (local_stamp timestamp without time zone)"
    )
  )
  empty_keys <- civil_date_test_keys("local_stamp")
  empty_operations <- epi_eda_approved_civil_dates(
    civil_date_test_operations(
      empty_keys$variable_key,
      "reviewed_date"
    )
  )
  empty_result <- epi_eda_derive_civil_dates(
    epi_eda_postgres_source(con, schema, empty_source_table),
    empty_operations,
    empty_keys,
    destination_schema = schema,
    destination_table = "empty_dates"
  )
  expect_identical(empty_result$audit$summary$destination_rows, 0L)
  expect_identical(empty_result$audit$operations$n_missing_derived, 0L)
})
