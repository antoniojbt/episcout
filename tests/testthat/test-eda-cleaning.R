context("technical cleaning rules and processed outputs")

cleaning_test_rules <- function(variable_key,
                                declared_type,
                                valid_min = rep(NA_real_, length(variable_key)),
                                valid_max = rep(NA_real_, length(variable_key)),
                                allowed_values = rep("", length(variable_key)),
                                missing_codes = rep("", length(variable_key))) {
  data.frame(
    variable_key = variable_key,
    declared_type = declared_type,
    valid_min = valid_min,
    valid_max = valid_max,
    allowed_values = allowed_values,
    missing_codes = missing_codes,
    stringsAsFactors = FALSE
  )
}

cleaning_test_keys <- function(name) {
  data.frame(
    name = name,
    variable_key = sprintf("var_%016d", seq_along(name)),
    stringsAsFactors = FALSE
  )
}

cleaning_fixture <- function() {
  data <- data.frame(
    sequence = 1:6,
    measure = c(-1, 0, 5, 11, 999, NA),
    whole = c(-1L, 0L, 10L, 11L, 99L, NA_integer_),
    category = factor(
      c("A", "B", "X", "M", NA, "A"),
      levels = c("M", "X", "B", "A", "unused")
    ),
    flag = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE,
    row.names = paste0("row_", 1:6)
  )
  keys <- cleaning_test_keys(names(data))
  selected <- match(
    c("measure", "whole", "category", "flag"),
    keys$name
  )
  rules <- cleaning_test_rules(
    keys$variable_key[selected],
    c("numeric", "integer", "categorical", "binary"),
    valid_min = c(0, 0, NA, NA),
    valid_max = c(10, 10, NA, NA),
    allowed_values = c("", "", "B;A", "TRUE;FALSE"),
    missing_codes = c("999", "99", "M", "")
  )
  list(
    data = data,
    keys = keys,
    rules = episcout::epi_eda_cleaning_rules(rules)
  )
}

test_that("cleaning rules have one exact canonical schema and pending input is rejected", {
  expect_identical(names(formals(epi_eda_cleaning_rules)), "rules")
  expect_identical(
    names(formals(epi_eda_apply_cleaning_rules)),
    c(
      "data", "rules", "variable_keys", "output_path", "output_format",
      "destination_schema", "destination_table"
    )
  )
  raw <- cleaning_test_rules(
    c("var_0000000000000002", "var_0000000000000001"),
    c("categorical", "numeric"),
    valid_min = c(NA, 0),
    valid_max = c(NA, 10),
    allowed_values = c(" B ; A ", ""),
    missing_codes = c("M", "0999.0")
  )
  observed <- epi_eda_cleaning_rules(raw)

  expect_identical(class(observed), c("epi_eda_cleaning_rules", "data.frame"))
  expect_named(
    observed,
    c(
      "variable_key", "declared_type", "valid_min", "valid_max",
      "allowed_values", "missing_codes"
    )
  )
  expect_identical(
    observed$variable_key,
    c("var_0000000000000001", "var_0000000000000002")
  )
  expect_identical(observed$missing_codes[[1]], "999")
  expect_identical(observed$allowed_values[[2]], "A;B")
  expect_type(observed$valid_min, "double")
  expect_error(epi_eda_cleaning_rules(raw[, rev(names(raw)), drop = FALSE]))
  expect_error(epi_eda_cleaning_rules(raw[, -ncol(raw), drop = FALSE]))

  spec <- data.frame(
    name = "field_a",
    label = "Field A",
    database_type = "text", analysis_type = "numeric",
    role = "measure",
    stringsAsFactors = FALSE
  )
  keys <- cleaning_test_keys(spec$name)
  pending <- epi_eda_qc_proposals(data.frame(field_a = c(0, 1)), spec, keys)
  expect_error(epi_eda_cleaning_rules(pending$proposals))
  expect_error(epi_eda_cleaning_rules(pending), "data frame")
})

test_that("the old symbol redirects only the neutral schema and the old class never executes", {
  raw <- cleaning_test_rules(
    "var_0000000000000001",
    "numeric",
    valid_min = 0,
    valid_max = 10
  )
  redirected <- expect_warning(
    epi_eda_approved_rules(raw),
    "deprecated"
  )
  expect_identical(class(redirected), c("epi_eda_cleaning_rules", "data.frame"))
  expect_identical(redirected, epi_eda_cleaning_rules(raw))

  old_schema <- data.frame(
    variable_key = raw$variable_key,
    rule_state = "approved",
    declared_type = raw$declared_type,
    valid_min = raw$valid_min,
    valid_max = raw$valid_max,
    allowed_values = raw$allowed_values,
    missing_codes = raw$missing_codes,
    approval_id = "approval_0000000000000001",
    stringsAsFactors = FALSE
  )
  expect_error(
    suppressWarnings(epi_eda_approved_rules(old_schema)),
    "required order"
  )

  forged <- old_schema
  class(forged) <- c("epi_eda_approved_rules", "data.frame")
  legacy_display <- paste(
    capture.output(print(forged)),
    capture.output(str(forged)),
    collapse = "\n"
  )
  expect_false(grepl(
    "var_|approval_|rule_state|approval_id|valid_min|allowed_values|approved|pending",
    legacy_display,
    ignore.case = TRUE
  ))
  expect_error(
    epi_eda_apply_cleaning_rules(
      data.frame(value = 1),
      forged,
      cleaning_test_keys("value")
    ),
    "epi_eda_cleaning_rules|unmodified object"
  )
})

test_that("malformed contradictory and unsupported rules fail without echoing values", {
  base <- cleaning_test_rules("var_0000000000000001", "numeric", 0, 10)
  expect_error(epi_eda_cleaning_rules(base[0, , drop = FALSE]), "at least one")

  invalid_utf8 <- rawToChar(as.raw(255))
  Encoding(invalid_utf8) <- "UTF-8"
  factor_type <- base
  factor_type$declared_type <- factor(factor_type$declared_type)
  character_bounds <- base
  character_bounds$valid_min <- "0"
  missing_key <- base
  missing_key$variable_key <- NA_character_
  invalid_utf8_key <- base
  invalid_utf8_key$variable_key <- paste0("KEY_CANARY", invalid_utf8)
  invalid_utf8_list <- base
  invalid_utf8_list$missing_codes <- paste0("VALUE_CANARY", invalid_utf8)
  missing_allowed <- base
  missing_allowed$allowed_values <- NA_character_
  missing_codes <- base
  missing_codes$missing_codes <- NA_character_
  non_scalar_list <- base
  non_scalar_list$missing_codes <- I(list(c("VALUE_CANARY_A", "VALUE_CANARY_B")))
  invalid <- list(
    factor_type,
    character_bounds,
    missing_key,
    invalid_utf8_key,
    invalid_utf8_list,
    missing_allowed,
    missing_codes,
    non_scalar_list,
    transform(base, declared_type = "TEXT_TYPE_CANARY"),
    transform(base, valid_min = 11, valid_max = 10),
    transform(base, valid_min = Inf),
    transform(base, valid_min = NaN),
    transform(base, allowed_values = "VALUE_CANARY"),
    transform(base, variable_key = "KEY_CANARY"),
    transform(base, valid_min = NA_real_, valid_max = NA_real_),
    transform(base, missing_codes = "1;;2"),
    transform(base, missing_codes = "1;01"),
    transform(base, missing_codes = "0;-0"),
    transform(base, missing_codes = "Inf"),
    transform(base, missing_codes = "-Inf"),
    transform(base, missing_codes = "NaN")
  )
  categorical <- cleaning_test_rules(
    "var_0000000000000001", "categorical",
    allowed_values = "A;B", missing_codes = "M"
  )
  invalid <- c(
    invalid,
    list(
      transform(categorical, valid_min = 0),
      transform(categorical, missing_codes = "A"),
      transform(categorical, declared_type = "binary", allowed_values = "A")
    )
  )
  integer <- cleaning_test_rules(
    "var_0000000000000001", "integer", 0, 10
  )
  invalid <- c(
    invalid,
    list(
      transform(integer, valid_min = 0.5),
      transform(integer, missing_codes = "1.5"),
      transform(integer, valid_max = 9007199254740992),
      transform(integer, missing_codes = "9007199254740992"),
      cbind(base, extra_canary = "EXTRA_CANARY", stringsAsFactors = FALSE)
    )
  )

  for (value in invalid) {
    error <- tryCatch(epi_eda_cleaning_rules(value), error = identity)
    expect_s3_class(error, "error")
    expect_false(grepl(
      "CANARY|VALUE_CANARY|KEY_CANARY",
      conditionMessage(error)
    ))
  }
  duplicated <- rbind(base, base)
  expect_error(epi_eda_cleaning_rules(duplicated), "unique")
})

test_that("in-memory rules preserve source order and produce literal audit counts", {
  fixture <- cleaning_fixture()
  source_before <- fixture$data
  rules_before <- fixture$rules
  keys_before <- fixture$keys
  observed <- epi_eda_apply_cleaning_rules(
    fixture$data,
    fixture$rules,
    fixture$keys
  )

  expect_identical(class(observed), c("epi_eda_cleaning_result", "list"))
  expect_named(observed, c("data", "audit"))
  expect_named(observed$audit, c("summary", "variables"))
  expect_named(
    observed$audit$summary,
    c(
      "rule_set_sha256", "publication", "source_rows", "source_columns",
      "destination_rows", "destination_columns", "n_missing_before",
      "n_missing_after", "n_transitioned_to_missing",
      "dimensions_reconciled", "transitions_reconciled",
      "publication_reconciled"
    )
  )
  expect_named(
    observed$audit$variables,
    c(
      "variable_key", "n", "n_missing_before", "n_missing_after",
      "n_transitioned_to_missing", "reconciled"
    )
  )
  expect_identical(observed$data$sequence, 1:6)
  expect_identical(observed$data$measure, c(NA, 0, 5, NA, NA, NA))
  expect_identical(
    observed$data$whole,
    c(NA_integer_, 0L, 10L, NA_integer_, NA_integer_, NA_integer_)
  )
  expect_identical(as.character(observed$data$category), c("A", "B", NA, NA, NA, "A"))
  expect_identical(levels(observed$data$category), levels(fixture$data$category))
  expect_identical(observed$data$flag, fixture$data$flag)
  expect_identical(row.names(observed$data), row.names(fixture$data))
  expect_identical(names(observed$data), names(fixture$data))
  expect_identical(fixture$data, source_before)
  expect_identical(fixture$rules, rules_before)
  expect_identical(fixture$keys, keys_before)

  summary <- observed$audit$summary
  expect_identical(summary$publication, "memory")
  expect_identical(
    unname(as.integer(unlist(summary[c(
      "source_rows", "source_columns", "destination_rows",
      "destination_columns", "n_missing_before", "n_missing_after",
      "n_transitioned_to_missing"
    )], use.names = FALSE))),
    c(6L, 5L, 6L, 5L, 4L, 12L, 8L)
  )
  expect_true(all(unlist(summary[c(
    "dimensions_reconciled", "transitions_reconciled",
    "publication_reconciled"
  )])))
  expect_identical(
    observed$audit$variables$n_missing_before,
    c(1L, 1L, 1L, 1L)
  )
  expect_identical(
    observed$audit$variables$n_missing_after,
    c(4L, 4L, 3L, 1L)
  )
  expect_identical(
    observed$audit$variables$n_transitioned_to_missing,
    c(3L, 3L, 2L, 0L)
  )

  raw_reordered <- rules_before[rev(seq_len(nrow(rules_before))), , drop = FALSE]
  class(raw_reordered) <- "data.frame"
  raw_reordered$allowed_values[raw_reordered$declared_type == "categorical"] <- "B;A"
  raw_reordered$allowed_values[raw_reordered$declared_type == "binary"] <- "TRUE;FALSE"
  canonical <- epi_eda_cleaning_rules(raw_reordered)
  repeated <- epi_eda_apply_cleaning_rules(fixture$data, canonical, fixture$keys)
  expect_identical(
    repeated$audit$summary$rule_set_sha256,
    summary$rule_set_sha256
  )
  expect_identical(repeated$data, observed$data)
})

test_that("zero rows all-missing values and data.table sources remain stable", {
  keys <- cleaning_test_keys(c("measure", "category"))
  rules <- epi_eda_cleaning_rules(cleaning_test_rules(
    keys$variable_key,
    c("numeric", "categorical"),
    valid_min = c(0, NA),
    valid_max = c(1, NA),
    allowed_values = c("", "A;B")
  ))
  zero <- data.frame(measure = numeric(), category = character())
  zero_result <- epi_eda_apply_cleaning_rules(zero, rules, keys)
  expect_identical(dim(zero_result$data), c(0L, 2L))
  expect_identical(zero_result$audit$variables$n, c(0L, 0L))
  expect_identical(zero_result$audit$variables$n_missing_after, c(0L, 0L))

  missing <- data.frame(
    measure = c(NA_real_, NaN),
    category = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  missing_before <- missing
  missing_result <- epi_eda_apply_cleaning_rules(missing, rules, keys)
  expect_identical(missing_result$data, missing_before)
  expect_identical(missing_result$audit$variables$n_missing_before, c(2L, 2L))
  expect_identical(missing_result$audit$variables$n_transitioned_to_missing, c(0L, 0L))

  skip_if_not_installed("data.table")
  table_source <- data.table::as.data.table(cleaning_fixture()$data)
  table_before <- data.table::copy(table_source)
  table_fixture <- cleaning_fixture()
  table_result <- epi_eda_apply_cleaning_rules(
    table_source,
    table_fixture$rules,
    table_fixture$keys
  )
  expect_identical(table_source, table_before)
  expect_s3_class(table_result$data, "data.frame")
  expect_false(data.table::is.data.table(table_result$data))
})

test_that("all in-memory validation completes before transformation", {
  fixture <- cleaning_fixture()
  source_before <- fixture$data
  invalid_keys <- fixture$keys
  invalid_keys$name[invalid_keys$name == "measure"] <- "SOURCE_NAME_CANARY"
  error <- tryCatch(
    epi_eda_apply_cleaning_rules(fixture$data, fixture$rules, invalid_keys),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_false(grepl("CANARY|measure", conditionMessage(error)))
  expect_identical(fixture$data, source_before)

  unresolved_keys <- fixture$keys[fixture$keys$name != "measure", , drop = FALSE]
  unresolved_error <- tryCatch(
    epi_eda_apply_cleaning_rules(fixture$data, fixture$rules, unresolved_keys),
    error = identity
  )
  expect_s3_class(unresolved_error, "error")
  expect_match(conditionMessage(unresolved_error), "resolve every cleaning rule")
  expect_false(grepl("measure|var_", conditionMessage(unresolved_error)))
  expect_identical(fixture$data, source_before)

  incompatible <- fixture$data
  incompatible$measure <- rep("STORAGE_VALUE_CANARY", nrow(incompatible))
  storage_error <- tryCatch(
    epi_eda_apply_cleaning_rules(incompatible, fixture$rules, fixture$keys),
    error = identity
  )
  expect_s3_class(storage_error, "error")
  expect_match(conditionMessage(storage_error), "incompatible with source storage")
  expect_false(grepl("CANARY|measure", conditionMessage(storage_error)))
  expect_identical(fixture$data, source_before)

  mutated <- fixture$rules
  mutated$allowed_values[mutated$declared_type == "categorical"] <- "B;A"
  expect_error(
    epi_eda_apply_cleaning_rules(fixture$data, mutated, fixture$keys),
    "unmodified object"
  )
  expect_identical(fixture$data, source_before)
})

test_that("CSV and RDS publication is explicit reconciled and no-replace", {
  fixture <- cleaning_fixture()
  directory <- tempfile("eda-cleaning-files-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rds_path <- file.path(directory, "processed.rds")
  csv_path <- file.path(directory, "processed.csv")

  rds <- epi_eda_apply_cleaning_rules(
    fixture$data,
    fixture$rules,
    fixture$keys,
    output_path = rds_path,
    output_format = "rds"
  )
  csv <- epi_eda_apply_cleaning_rules(
    fixture$data,
    fixture$rules,
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
  expect_true(rds$audit$summary$publication_reconciled)
  expect_true(csv$audit$summary$publication_reconciled)

  collision <- file.path(directory, "COLLISION_PATH_CANARY")
  writeLines("ORIGINAL", collision)
  error <- tryCatch(
    epi_eda_apply_cleaning_rules(
      fixture$data,
      fixture$rules,
      fixture$keys,
      output_path = collision,
      output_format = "rds"
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_identical(readLines(collision), "ORIGINAL")
  expect_false(grepl("CANARY", conditionMessage(error)))

  for (format in c("rdata", "parquet")) {
    target <- file.path(directory, paste0("unsupported.", format))
    expect_error(
      epi_eda_apply_cleaning_rules(
        fixture$data,
        fixture$rules,
        fixture$keys,
        output_path = target,
        output_format = format
      ),
      "exactly csv or rds"
    )
    expect_false(file.exists(target))
  }
  expect_error(
    epi_eda_apply_cleaning_rules(
      fixture$data,
      fixture$rules,
      fixture$keys,
      output_path = file.path(directory, "implicit.csv")
    ),
    "supplied together"
  )
})

test_that("a neutral CSV-backed rule set reaches cleaning and existing EDA", {
  directory <- tempfile("eda-cleaning-config-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rules_path <- file.path(directory, "cleaning-rules.csv")
  source <- data.frame(
    sequence = 1:5,
    measure = c(NA_real_, -1, 5, 11, 999),
    category = c(NA_character_, "A", "B", "X", "M"),
    unruled = letters[1:5],
    stringsAsFactors = FALSE
  )
  source_before <- source
  keys <- cleaning_test_keys(names(source))
  raw_rules <- cleaning_test_rules(
    keys$variable_key[match(c("measure", "category"), keys$name)],
    c("numeric", "categorical"),
    valid_min = c(0, NA),
    valid_max = c(10, NA),
    allowed_values = c("", "A;B"),
    missing_codes = c("999", "M")
  )
  utils::write.csv(raw_rules, rules_path, row.names = FALSE, na = "")

  from_file <- utils::read.csv(
    rules_path,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = character()
  )
  from_file$valid_min <- suppressWarnings(as.numeric(from_file$valid_min))
  from_file$valid_max <- suppressWarnings(as.numeric(from_file$valid_max))
  rules <- epi_eda_cleaning_rules(from_file)
  cleaned <- epi_eda_apply_cleaning_rules(source, rules, keys)

  expect_identical(source, source_before)
  expect_identical(cleaned$data$sequence, source$sequence)
  expect_identical(cleaned$data$measure, c(NA_real_, NA, 5, NA, NA))
  expect_identical(cleaned$data$category, c(NA_character_, "A", "B", NA, NA))
  expect_identical(cleaned$data$unruled, source$unruled)
  expect_identical(
    cleaned$audit$variables$n_transitioned_to_missing,
    c(3L, 2L)
  )

  spec <- data.frame(
    name = names(source),
    label = c("Sequence", "Measure", "Category", "Unruled"),
    database_type = rep("text", ncol(source)),
    analysis_type = c("integer", "numeric", "categorical", "text"),
    role = rep("covariate", ncol(source)),
    levels = c("", "", "A;B", ""),
    missing_codes = rep("", ncol(source)),
    stringsAsFactors = FALSE
  )
  missing <- epi_eda_profile_missing(cleaned$data, spec)
  expect_identical(missing$n, rep(5L, 4L))
  expect_identical(missing$n_missing, c(0L, 4L, 3L, 0L))
})

test_that("file staging and reconciliation failures leave no artifact", {
  fixture <- cleaning_fixture()
  directory <- tempfile("eda-cleaning-failure-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  failed_write <- file.path(directory, "failed-write.rds")
  expect_error(
    with_mocked_bindings(
      epi_eda_apply_cleaning_rules(
        fixture$data,
        fixture$rules,
        fixture$keys,
        output_path = failed_write,
        output_format = "rds"
      ),
      clean_write_staged_file = function(...) stop("simulated staging failure"),
      .package = "episcout"
    ),
    "simulated staging failure"
  )
  expect_false(file.exists(failed_write))
  expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0L)

  failed_reconcile <- file.path(directory, "failed-reconcile.rds")
  calls <- 0L
  expect_error(
    with_mocked_bindings(
      epi_eda_apply_cleaning_rules(
        fixture$data,
        fixture$rules,
        fixture$keys,
        output_path = failed_reconcile,
        output_format = "rds"
      ),
      clean_staged_dimensions = function(data, path, format) {
        calls <<- calls + 1L
        if (calls == 1L) {
          return(c(nrow(data), ncol(data)))
        }
        stop("simulated reconciliation failure")
      },
      .package = "episcout"
    ),
    "failed reconciliation"
  )
  expect_false(file.exists(failed_reconcile))
  expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("CSV rejects non-scalar columns before creating a destination", {
  fixture <- cleaning_fixture()
  fixture$data$nested <- I(rep(list("ROW_VALUE_CANARY"), nrow(fixture$data)))
  directory <- tempfile("eda-cleaning-csv-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  target <- file.path(directory, "nested.csv")
  error <- tryCatch(
    epi_eda_apply_cleaning_rules(
      fixture$data,
      fixture$rules,
      fixture$keys,
      output_path = target,
      output_format = "csv"
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_false(grepl("CANARY", conditionMessage(error)))
  expect_false(file.exists(target))
})

test_that("rule objects audits and displays omit process metadata and displays redact rule values", {
  fixture <- cleaning_fixture()
  result <- epi_eda_apply_cleaning_rules(fixture$data, fixture$rules, fixture$keys)
  rule_display <- paste(
    capture.output(print(fixture$rules)),
    capture.output(str(fixture$rules)),
    collapse = "\n"
  )
  result_display <- paste(
    capture.output(print(result)),
    capture.output(str(result)),
    collapse = "\n"
  )
  expect_false(any(c("rule_state", "approval_id", "approved", "pending") %in% names(fixture$rules)))
  expect_false(any(c("rule_state", "approval_id", "approved", "pending") %in%
                     unlist(lapply(result$audit, names), use.names = FALSE)))
  expect_false(grepl("var_|999|approval_|approved|pending", rule_display, ignore.case = TRUE))
  expect_false(grepl(
    "var_|999|approval_|approved|pending|measure|category",
    result_display,
    ignore.case = TRUE
  ))
  expect_match(result_display, "cleaning rules: 4", fixed = TRUE)
})

test_that("PostgreSQL plans bind rule values in server-side CASE expressions", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- list(
    con = con,
    schema = "main",
    relation = "source_table",
    columns = data.frame(
      name = c("measure", "untouched"),
      base_udt_name = c("float8", "text"),
      typtype = c("b", "b"),
      collation_deterministic = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    )
  )
  rule <- epi_eda_cleaning_rules(cleaning_test_rules(
    "var_0000000000000001",
    "numeric",
    valid_min = 0,
    valid_max = 10,
    missing_codes = "999"
  ))
  plan_builder <- getFromNamespace("clean_pg_rule_plan", "episcout")
  plan <- plan_builder(
    source,
    rule,
    "measure"
  )
  statement <- getFromNamespace("clean_pg_create_statement", "episcout")(
    source,
    '"destination_table"',
    list(plan)
  )
  expect_match(statement, "CASE WHEN", fixed = TRUE)
  expect_match(statement, "$1::double precision", fixed = TRUE)
  expect_match(statement, "$2::double precision", fixed = TRUE)
  expect_match(statement, "$3::double precision", fixed = TRUE)
  expect_match(statement, "untouched", fixed = TRUE)
  expect_false(grepl("999|approval_|approved|pending|var_", statement, ignore.case = TRUE))
  expect_identical(length(plan$params), 3L)

  incompatible <- source
  incompatible$columns$base_udt_name[[1]] <- "text"
  storage_error <- tryCatch(
    plan_builder(incompatible, rule, "measure"),
    error = identity
  )
  expect_s3_class(storage_error, "error")
  expect_match(conditionMessage(storage_error), "incompatible with PostgreSQL source storage")
  expect_false(grepl("measure|source_table", conditionMessage(storage_error)))
})

cleaning_postgres_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    testthat::skip("Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL integration tests.")
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

test_that("PostgreSQL publication is equivalent new-only and transactional", {
  con <- cleaning_postgres_connection()
  schema <- paste0("epi_cleaning_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  source_sql <- paste0(
    schema_sql,
    ".",
    as.character(DBI::dbQuoteIdentifier(con, "private_source_canary"))
  )
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
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", source_sql, " (",
    "sequence integer, measure double precision, whole integer, ",
    "category text, flag boolean, all_missing double precision)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", source_sql, " VALUES ",
    "(1, -1, -1, 'A', TRUE, NULL),",
    "(2, 0, 0, 'B', FALSE, NULL),",
    "(3, 5, 10, 'X', NULL, NULL),",
    "(4, 11, 11, 'M', TRUE, NULL),",
    "(5, 999, 99, NULL, FALSE, NULL),",
    "(6, NULL, NULL, 'A', TRUE, NULL)"
  ))
  data <- data.frame(
    sequence = 1:6,
    measure = c(-1, 0, 5, 11, 999, NA),
    whole = c(-1L, 0L, 10L, 11L, 99L, NA_integer_),
    category = c("A", "B", "X", "M", NA, "A"),
    flag = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE),
    all_missing = rep(NA_real_, 6L),
    stringsAsFactors = FALSE
  )
  keys <- cleaning_test_keys(names(data))
  selected <- match(
    c("measure", "whole", "category", "flag", "all_missing"),
    keys$name
  )
  rules <- epi_eda_cleaning_rules(cleaning_test_rules(
    keys$variable_key[selected],
    c("numeric", "integer", "categorical", "binary", "numeric"),
    valid_min = c(0, 0, NA, NA, 0),
    valid_max = c(10, 10, NA, NA, NA),
    allowed_values = c("", "", "A;B", "FALSE;TRUE", ""),
    missing_codes = c("999", "99", "M", "", "999")
  ))
  source <- epi_eda_postgres_source(con, schema, "private_source_canary")
  before <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", source_sql, " ORDER BY sequence"))
  memory <- epi_eda_apply_cleaning_rules(data, rules, keys)
  database <- epi_eda_apply_cleaning_rules(
    source,
    rules,
    keys,
    destination_schema = schema,
    destination_table = "private_destination_canary"
  )
  destination_sql <- paste0(
    schema_sql,
    ".",
    as.character(DBI::dbQuoteIdentifier(con, "private_destination_canary"))
  )
  after <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", source_sql, " ORDER BY sequence"))
  processed <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", destination_sql, " ORDER BY sequence"))

  expect_identical(before, after)
  expect_identical(processed$sequence, memory$data$sequence)
  expect_equal(processed$measure, memory$data$measure)
  expect_identical(processed$whole, memory$data$whole)
  expect_identical(processed$category, memory$data$category)
  expect_identical(processed$flag, memory$data$flag)
  expect_true(all(is.na(processed$all_missing)))
  expect_null(database$data)
  expect_identical(database$audit$variables, memory$audit$variables)
  expect_identical(
    database$audit$summary$rule_set_sha256,
    memory$audit$summary$rule_set_sha256
  )
  expect_identical(database$audit$summary$publication, "postgresql")
  expect_true(database$audit$summary$publication_reconciled)
  expect_true(DBI::dbExistsTable(
    con,
    DBI::Id(schema = schema, table = "private_destination_canary")
  ))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  collision_error <- tryCatch(
    epi_eda_apply_cleaning_rules(
      source,
      rules,
      keys,
      destination_schema = schema,
      destination_table = "private_destination_canary"
    ),
    error = identity
  )
  expect_s3_class(collision_error, "error")
  expect_false(grepl(
    "private|canary|epi_cleaning",
    conditionMessage(collision_error),
    ignore.case = TRUE
  ))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))

  rollback_table <- "private_rollback_canary"
  expect_error(
    with_mocked_bindings(
      epi_eda_apply_cleaning_rules(
        source,
        rules,
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

  empty_source <- "empty_source"
  empty_destination <- "empty_destination"
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE ", schema_sql, ".",
      as.character(DBI::dbQuoteIdentifier(con, empty_source)),
      " (measure double precision)"
    )
  )
  empty_keys <- cleaning_test_keys("measure")
  empty_rules <- epi_eda_cleaning_rules(cleaning_test_rules(
    empty_keys$variable_key,
    "numeric",
    valid_min = 0
  ))
  empty_result <- epi_eda_apply_cleaning_rules(
    epi_eda_postgres_source(con, schema, empty_source),
    empty_rules,
    empty_keys,
    destination_schema = schema,
    destination_table = empty_destination
  )
  expect_identical(empty_result$audit$summary$destination_rows, 0L)
  expect_identical(empty_result$audit$variables$n_missing_after, 0L)
})
