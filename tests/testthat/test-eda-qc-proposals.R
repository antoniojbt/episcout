context("reviewable QC evidence and pending proposals")

qc_test_spec <- function(name,
                         type,
                         role = rep("measure", length(name)),
                         units = rep("", length(name)),
                         levels = rep("", length(name)),
                         min = rep("", length(name)),
                         max = rep("", length(name)),
                         missing_codes = rep("", length(name))) {
  data.frame(
    name = name,
    label = as.character(seq_along(name)),
    database_type = rep("text", length(name)), analysis_type = type,
    role = role,
    units = units,
    levels = levels,
    min = min,
    max = max,
    missing_codes = missing_codes,
    stringsAsFactors = FALSE
  )
}

qc_test_keys <- function(name) {
  data.frame(
    name = name,
    variable_key = sprintf("var_%016d", seq_along(name)),
    stringsAsFactors = FALSE
  )
}

qc_evidence_columns <- c(
  "variable_key", "evidence_state", "declared_type", "profile_status",
  "evidence_code", "n", "n_missing", "n_observed", "n_unique",
  "n_infinite", "n_finite", "observed_min", "observed_max",
  "tukey_lower_fence", "tukey_upper_fence", "n_below_tukey",
  "n_above_tukey"
)

qc_proposal_columns <- c(
  "variable_key", "proposal_state", "candidate_type",
  "units_review_required", "candidate_units", "candidate_screening_min",
  "candidate_screening_max", "screening_basis",
  "candidate_allowed_levels", "candidate_missing_codes", "rationale_codes"
)

test_that("public formals, classes, schemas, and empty results are exact", {
  expect_identical(
    names(formals(epi_eda_qc_proposals)),
    c("data", "spec", "variable_keys")
  )
  spec <- qc_test_spec(character(), character())
  keys <- qc_test_keys(character())
  observed <- epi_eda_qc_proposals(data.frame(), spec, keys)

  expect_identical(class(observed), c("epi_eda_qc_proposals", "list"))
  expect_named(observed, c("evidence", "proposals"))
  expect_named(observed$evidence, qc_evidence_columns)
  expect_named(observed$proposals, qc_proposal_columns)
  expect_equal(nrow(observed$evidence), 0L)
  expect_equal(nrow(observed$proposals), 0L)
  expect_true(all(vapply(
    observed$evidence[c("n", "n_missing", "n_observed", "n_unique")],
    is.integer,
    logical(1)
  )))
  expect_type(observed$proposals$units_review_required, "logical")
  expect_type(observed$proposals$candidate_screening_min, "double")
})

test_that("opaque key maps are exact and structural errors remain value-free", {
  spec <- qc_test_spec(c("field_a", "field_b"), c("numeric", "text"))
  data <- data.frame(field_a = 1:2, field_b = c("a", "b"))
  valid <- qc_test_keys(spec$name)
  reordered <- valid[2:1, , drop = FALSE]
  expect_identical(
    epi_eda_qc_proposals(data, spec, reordered)$evidence$variable_key,
    valid$variable_key
  )

  invalid <- list(
    "not a map",
    data.frame(variable_key = "var_0000000000000001", name = "field_a"),
    transform(valid, name = factor(name)),
    transform(valid, variable_key = factor(variable_key)),
    rbind(valid, valid[1, , drop = FALSE]),
    transform(valid, variable_key = rep(variable_key[[1]], 2L)),
    transform(valid, variable_key = c("KEY_CANARY", valid$variable_key[[2]])),
    transform(valid, name = c("LOOKUP_CANARY", "field_b")),
    rbind(valid, data.frame(
      name = "EXTRA_CANARY",
      variable_key = "var_0000000000000003"
    ))
  )
  for (value in invalid) {
    error <- tryCatch(
      epi_eda_qc_proposals(data, spec, value),
      error = identity
    )
    expect_s3_class(error, "error")
    message <- conditionMessage(error)
    expect_false(grepl("CANARY|field_a|field_b", message))
  }

  invalid_spec <- transform(spec, analysis_type = c("TYPE_CANARY", "text"))
  error <- tryCatch(
    epi_eda_qc_proposals(data, invalid_spec, valid),
    error = identity
  )
  expect_match(conditionMessage(error), "valid EDA specification", fixed = TRUE)
  expect_false(grepl("CANARY", conditionMessage(error), fixed = TRUE))
})

test_that("aggregate evidence and review prompts follow literal expectations", {
  data <- data.frame(
    indicator = c(0L, 1L, 0L, 1L),
    screening = c(1, 2, 3, 100),
    non_finite = c(1, 2, Inf, -Inf),
    coded_missing = c(NA_real_, 999, NA_real_, 999),
    category = c("ROW_CANARY_A", "ROW_CANARY_B", "ROW_CANARY_C", "ROW_CANARY_D"),
    event_date = as.Date(c("2024-01-01", "2024-01-02", NA, "2024-01-04")),
    record_key = c("KEY_CANARY_A", "KEY_CANARY_B", "KEY_CANARY_C", "KEY_CANARY_D"),
    stringsAsFactors = FALSE
  )
  spec <- qc_test_spec(
    names(data),
    c("integer", "numeric", "numeric", "numeric", "text", "date", "text"),
    role = c(rep("measure", 6L), " Identifier "),
    units = c("", "", "unit", "", "", "", ""),
    min = c("-10", "-1000", "", "", "", "", ""),
    max = c("10", "1000", "", "", "", "", ""),
    missing_codes = c("", "", "", "999", "", "", "")
  )
  keys <- qc_test_keys(spec$name)
  source_before <- data
  spec_before <- spec
  keys_before <- keys

  observed <- epi_eda_qc_proposals(data, spec, keys)
  repeated <- epi_eda_qc_proposals(data, spec, keys)
  evidence <- observed$evidence
  proposals <- observed$proposals

  expect_identical(observed, repeated)
  expect_identical(data, source_before)
  expect_identical(spec, spec_before)
  expect_identical(keys, keys_before)
  expect_named(evidence, qc_evidence_columns)
  expect_named(proposals, qc_proposal_columns)
  expect_identical(evidence$variable_key, keys$variable_key)
  expect_true(all(evidence$evidence_state == "descriptive"))
  expect_true(all(proposals$proposal_state == "pending"))
  expect_false(any(c(
    "name", "approved", "apply", "valid_min", "valid_max", "rule_state"
  ) %in% c(names(evidence), names(proposals))))

  indicator <- evidence[1, ]
  expect_identical(
    as.integer(unlist(indicator[c(
      "n", "n_missing", "n_observed", "n_unique", "n_infinite",
      "n_finite", "n_below_tukey", "n_above_tukey"
    )], use.names = FALSE)),
    c(4L, 0L, 4L, 2L, 0L, 4L, 0L, 0L)
  )
  expect_identical(indicator$observed_min, 0)
  expect_identical(indicator$observed_max, 1)

  screening <- evidence[2, ]
  expect_identical(screening$observed_min, 1)
  expect_identical(screening$observed_max, 100)
  expect_identical(screening$tukey_lower_fence, -36.5)
  expect_identical(screening$tukey_upper_fence, 65.5)
  expect_identical(screening$n_below_tukey, 0L)
  expect_identical(screening$n_above_tukey, 1L)

  non_finite <- evidence[3, ]
  expect_identical(non_finite$n_infinite, 2L)
  expect_identical(non_finite$n_finite, 2L)
  expect_identical(non_finite$observed_min, 1)
  expect_identical(non_finite$observed_max, 2)

  all_missing <- evidence[4, ]
  expect_identical(all_missing$evidence_code, "all_missing")
  expect_identical(
    as.integer(unlist(all_missing[c(
      "n", "n_missing", "n_observed", "n_unique", "n_infinite",
      "n_finite", "n_below_tukey", "n_above_tukey"
    )], use.names = FALSE)),
    c(4L, 4L, 0L, 0L, 0L, 0L, 0L, 0L)
  )
  expect_true(all(is.na(all_missing[c(
    "observed_min", "observed_max", "tukey_lower_fence",
    "tukey_upper_fence"
  )])))
  expect_identical(evidence$n_unique[[5]], 4L)
  expect_true(all(is.na(evidence[5, c(
    "n_infinite", "n_finite", "observed_min", "observed_max",
    "n_below_tukey", "n_above_tukey"
  )])))
  expect_identical(evidence$evidence_code[[7]], "declared_identifier")
  expect_true(all(is.na(evidence[7, 6:17])))

  binary <- proposals[proposals$variable_key == keys$variable_key[[1]], ]
  expect_identical(binary$candidate_type, "binary")
  expect_identical(binary$candidate_allowed_levels, "0;1")
  expect_identical(
    binary$rationale_codes,
    "units_not_declared;observed_integral_zero_one"
  )
  expect_true(is.na(binary$candidate_screening_min))
  screening_prompt <- proposals[
    proposals$variable_key == keys$variable_key[[2]], ,
    drop = FALSE
  ]
  expect_identical(screening_prompt$candidate_screening_min, -36.5)
  expect_identical(screening_prompt$candidate_screening_max, 65.5)
  expect_identical(screening_prompt$screening_basis, "tukey_1_5_iqr")
  expect_identical(
    screening_prompt$rationale_codes,
    "units_not_declared;finite_values_beyond_tukey"
  )
  infinity_prompt <- proposals[
    proposals$variable_key == keys$variable_key[[3]], ,
    drop = FALSE
  ]
  expect_identical(infinity_prompt$rationale_codes, "non_finite_values_present")
  expect_false(keys$variable_key[[5]] %in% proposals$variable_key)
  expect_true(keys$variable_key[[6]] %in% proposals$variable_key)
  expect_false(keys$variable_key[[7]] %in% proposals$variable_key)
  expect_true(all(proposals$candidate_units == ""))
  expect_true(all(proposals$candidate_missing_codes == ""))
  expect_true(all(proposals$candidate_allowed_levels %in% c("", "0;1")))
  expect_false(any(grepl("approved|executable", unlist(observed), ignore.case = TRUE)))

  visible <- paste(capture.output(print(observed)), collapse = "\n")
  structured <- paste(capture.output(str(observed)), collapse = "\n")
  expect_match(visible, "evidence rows: 7", fixed = TRUE)
  expect_match(structured, "pending proposals:", fixed = TRUE)
  expect_false(grepl("CANARY|var_", paste(visible, structured)))
})

test_that("binary precedence and finite screening cases remain prompts only", {
  data <- data.frame(
    imbalanced = c(rep(0L, 20L), 1L),
    constant_zero = rep(0L, 21L),
    constant_one = rep(1L, 21L),
    three_levels = rep(c(0L, 1L, 2L), 7L),
    signed = rep(c(-1L, 1L, -1L), 7L),
    zero_iqr_tail = c(1, 1, 1, 1, 10, rep(1, 16L)),
    infinite_binary = c(rep(0, 10L), rep(1, 10L), Inf),
    stringsAsFactors = FALSE
  )
  spec <- qc_test_spec(
    names(data),
    c(rep("integer", 5L), "numeric", "numeric"),
    units = rep("unit", 7L)
  )
  keys <- qc_test_keys(spec$name)
  observed <- epi_eda_qc_proposals(data, spec, keys)
  proposals <- observed$proposals

  imbalanced <- proposals[proposals$variable_key == keys$variable_key[[1]], ]
  expect_identical(imbalanced$candidate_type, "binary")
  expect_identical(imbalanced$candidate_allowed_levels, "0;1")
  expect_true(is.na(imbalanced$candidate_screening_min))
  expect_false(any(keys$variable_key[2:5] %in% proposals$variable_key))

  tail <- proposals[proposals$variable_key == keys$variable_key[[6]], ]
  expect_identical(tail$candidate_screening_min, 1)
  expect_identical(tail$candidate_screening_max, 1)
  expect_identical(tail$rationale_codes, "finite_values_beyond_tukey")
  infinity <- proposals[proposals$variable_key == keys$variable_key[[7]], ]
  expect_identical(infinity$candidate_type, "")
  expect_identical(infinity$candidate_allowed_levels, "")
  expect_identical(infinity$rationale_codes, "non_finite_values_present")
  expect_identical(spec$analysis_type, c(rep("integer", 5L), "numeric", "numeric"))
  expect_identical(data$imbalanced, c(rep(0L, 20L), 1L))
})

test_that("numeric uniqueness is counted without display-text coercion", {
  data <- data.frame(nearby_numeric = c(1, 1 + .Machine$double.eps))
  spec <- qc_test_spec("nearby_numeric", "numeric", units = "unit")
  evidence <- epi_eda_qc_proposals(
    data,
    spec,
    qc_test_keys(spec$name)
  )$evidence

  expect_identical(evidence$n_unique, 2L)
})

test_that("zero-row, missing, incompatible, unsupported, and high-cardinality inputs are safe", {
  zero_data <- data.frame(
    numeric_empty = numeric(),
    text_empty = character(),
    stringsAsFactors = FALSE
  )
  zero_spec <- qc_test_spec(
    names(zero_data),
    c("numeric", "text"),
    units = c("unit", "")
  )
  zero <- epi_eda_qc_proposals(
    zero_data,
    zero_spec,
    qc_test_keys(zero_spec$name)
  )$evidence
  expect_identical(zero$evidence_code, c("zero_rows", "zero_rows"))
  expect_identical(zero$n, c(0L, 0L))
  expect_identical(zero$n_missing, c(0L, 0L))
  expect_identical(zero$n_observed, c(0L, 0L))
  expect_identical(zero$n_unique, c(0L, 0L))
  expect_identical(zero$n_infinite, c(0L, NA_integer_))
  expect_identical(zero$n_finite, c(0L, NA_integer_))

  all_missing_data <- data.frame(
    number = c(NA_real_, NA_real_),
    text = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  all_missing_spec <- qc_test_spec(
    names(all_missing_data),
    c("numeric", "text"),
    units = c("unit", "")
  )
  all_missing <- epi_eda_qc_proposals(
    all_missing_data,
    all_missing_spec,
    qc_test_keys(all_missing_spec$name)
  )$evidence
  expect_identical(all_missing$evidence_code, c("all_missing", "all_missing"))
  expect_identical(all_missing$n_missing, c(2L, 2L))
  expect_identical(all_missing$n_observed, c(0L, 0L))
  expect_identical(all_missing$n_unique, c(0L, 0L))

  temporal_data <- data.frame(
    date_value = as.Date("2024-01-01"),
    datetime_value = as.POSIXct("2024-01-01", tz = "UTC")
  )
  temporal_spec <- qc_test_spec(
    names(temporal_data),
    c("date", "datetime")
  )
  temporal <- epi_eda_qc_proposals(
    temporal_data,
    temporal_spec,
    qc_test_keys(temporal_spec$name)
  )
  expect_true(all(temporal$proposals$units_review_required))
  expect_true(all(temporal$proposals$candidate_units == ""))

  data <- data.frame(
    incompatible = rep("TEXT_CANARY", 100L),
    high_cardinality = sprintf("LEVEL_CANARY_%03d", seq_len(100L)),
    safe = seq_len(100L),
    stringsAsFactors = FALSE
  )
  data$unsupported <- I(rep(list("NESTED_CANARY"), 100L))
  data$matrix_value <- I(matrix(seq_len(200L), nrow = 100L))
  data$raw_value <- as.raw(rep(1L, 100L))
  data$complex_value <- rep(1 + 2i, 100L)
  data$semantic_subclass <- structure(
    seq_len(100L),
    class = c("synthetic_semantic", "integer")
  )
  spec <- qc_test_spec(
    c(
      "incompatible", "high_cardinality", "unsupported", "matrix_value",
      "raw_value", "complex_value", "semantic_subclass", "absent", "safe"
    ),
    c(
      "numeric", "text", "text", "numeric", "numeric", "numeric",
      "integer", "numeric", "integer"
    ),
    role = c(rep("measure", 8L), "id")
  )
  keys <- qc_test_keys(spec$name)
  observed <- epi_eda_qc_proposals(data, spec, keys)

  expect_identical(
    observed$evidence$evidence_code,
    c(
      "incompatible_storage", "profiled", rep("unsupported_storage", 5L),
      "missing_variable", "declared_identifier"
    )
  )
  expect_identical(observed$evidence$n_unique[[2]], 100L)
  expect_true(all(is.na(observed$evidence[c(1, 3:9), 6:17])))
  rendered <- paste(capture.output(dput(observed)), collapse = "\n")
  expect_false(grepl("CANARY", rendered, fixed = TRUE))
  expect_false(any(grepl("LEVEL|TEXT|NESTED", unlist(observed))))
})

test_that("data.table inputs and failure paths do not mutate by reference", {
  skip_if_not_installed("data.table")
  data <- data.table::data.table(
    indicator = c(0L, 1L, 0L),
    category = factor(c("A", "B", "A"), levels = c("B", "A", "unused"))
  )
  spec <- qc_test_spec(names(data), c("integer", "categorical"))
  keys <- qc_test_keys(spec$name)
  data_before <- data.table::copy(data)
  spec_before <- spec
  keys_before <- keys

  expect_no_message(expect_no_warning(
    epi_eda_qc_proposals(data, spec, keys)
  ))
  expect_identical(data, data_before)
  expect_identical(levels(data$category), c("B", "A", "unused"))
  expect_identical(spec, spec_before)
  expect_identical(keys, keys_before)

  invalid <- keys
  invalid$variable_key[[1]] <- "INVALID_CANARY"
  expect_error(epi_eda_qc_proposals(data, spec, invalid), "opaque identifiers")
  expect_identical(data, data_before)
  expect_identical(spec, spec_before)
  expect_identical(keys, keys_before)
})

test_that("PostgreSQL QC helpers request scalar aggregates only", {
  numeric_qc <- getFromNamespace("eda_postgres_qc_numeric", "episcout")
  counts_qc <- getFromNamespace("eda_postgres_qc_counts", "episcout")
  source <- list(con = NULL)
  numeric_column <- data.frame(
    name = "measure", base_udt_name = "float8", stringsAsFactors = FALSE
  )
  text_column <- data.frame(
    name = "category", base_udt_name = "text", stringsAsFactors = FALSE
  )
  contract <- list(sql = "missing_expression", params = list())
  calls <- list()
  fetch <- function(con, statement, params, query_kind, limit, ...) {
    calls[[length(calls) + 1L]] <<- list(
      statement = statement,
      query_kind = query_kind,
      limit = limit
    )
    if (query_kind == "qc_numeric_aggregate") {
      return(data.frame(
        n = "4", n_missing = "0", n_observed = "4", n_unique = "4",
        n_infinite = "0", n_finite = "4", observed_min = 1,
        observed_max = 100, q1 = 1.75, q3 = 27.25,
        integer_exact = TRUE, stringsAsFactors = FALSE
      ))
    }
    if (query_kind == "qc_numeric_fences") {
      return(data.frame(n_below = "0", n_above = "1"))
    }
    data.frame(n = "100", n_missing = "0", n_observed = "100", n_unique = "100")
  }
  numeric <- with_mocked_bindings(
    numeric_qc(
      source, numeric_column, contract, "numeric", 1L,
      "var_0000000000000001"
    ),
    eda_postgres_value_expression = function(...) "value_expression",
    eda_postgres_table_sql = function(...) "source_relation",
    eda_db_fetch = fetch,
    .package = "episcout"
  )
  text <- with_mocked_bindings(
    counts_qc(
      source, text_column, contract, "text", 2L,
      "var_0000000000000002"
    ),
    eda_postgres_value_expression = function(...) "value_expression",
    eda_postgres_table_sql = function(...) "source_relation",
    eda_db_fetch = fetch,
    .package = "episcout"
  )

  expect_identical(numeric$tukey_lower_fence, -36.5)
  expect_identical(numeric$tukey_upper_fence, 65.5)
  expect_identical(numeric$n_above_tukey, 1L)
  expect_identical(text$n_unique, 100L)
  expect_identical(
    vapply(calls, `[[`, character(1), "query_kind"),
    c("qc_numeric_aggregate", "qc_numeric_fences", "qc_scalar_counts")
  )
  expect_identical(vapply(calls, `[[`, integer(1), "limit"), rep(1L, 3L))
  sql <- paste(vapply(calls, `[[`, character(1), "statement"), collapse = "\n")
  expect_match(sql, "count(DISTINCT value)", fixed = TRUE)
  expect_false(grepl("GROUP BY|shapiro|ORDER BY value[)]? SELECT value", sql, ignore.case = TRUE))
  require_scalar <- getFromNamespace("eda_postgres_qc_require_scalar", "episcout")
  expect_error(
    require_scalar(data.frame(n = c("1", "2")), "n"),
    "invalid scalar schema"
  )
})

qc_postgres_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    skip("Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL integration tests.")
  }
  skip_if_not_installed("RPostgres")
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    dbname = Sys.getenv("PGDATABASE", "synthetic_records"),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "")
  )
}

test_that("equivalent data-frame and PostgreSQL inputs produce equivalent QC", {
  con <- qc_postgres_connection()
  schema <- paste0("epi_qc_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table_sql <- paste0(
    schema_sql, ".", as.character(DBI::dbQuoteIdentifier(con, "qc_fixture"))
  )
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0("DROP SCHEMA ", schema_sql, " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_sql, " (",
    "indicator double precision, whole_indicator bigint, screening double precision, ",
    "non_finite double precision, category text, all_missing double precision, ",
    "event_date date, record_key text, incompatible text, high_cardinality text)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", table_sql, " VALUES ",
    "(0, 0, 1, 1, 'A', NULL, DATE '2024-01-01', 'K1', 'x', 'L1'),",
    "(1, 1, 2, 2, 'B', NULL, DATE '2024-01-02', 'K2', 'y', 'L2'),",
    "(0, 0, 3, 'Infinity', 'C', NULL, NULL, 'K3', 'z', 'L3'),",
    "(1, 1, 100, NULL, 'D', NULL, DATE '2024-01-04', 'K4', 'w', 'L4')"
  ))
  data <- data.frame(
    indicator = c(0, 1, 0, 1),
    whole_indicator = c(0L, 1L, 0L, 1L),
    screening = c(1, 2, 3, 100),
    non_finite = c(1, 2, Inf, NA),
    category = c("A", "B", "C", "D"),
    all_missing = rep(NA_real_, 4L),
    event_date = as.Date(c("2024-01-01", "2024-01-02", NA, "2024-01-04")),
    record_key = c("K1", "K2", "K3", "K4"),
    incompatible = c("x", "y", "z", "w"),
    high_cardinality = c("L1", "L2", "L3", "L4"),
    stringsAsFactors = FALSE
  )
  spec <- qc_test_spec(
    c(names(data), "absent"),
    c(
      "numeric", "integer", "numeric", "numeric", "text", "numeric",
      "date", "text", "numeric", "text", "numeric"
    ),
    role = c(rep("measure", 7L), "identifier", rep("measure", 3L)),
    units = c("", "", "", "unit", "", "", "", "", "", "", "")
  )
  keys <- qc_test_keys(spec$name)
  before <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_sql, " ORDER BY record_key"))
  memory <- epi_eda_qc_proposals(data, spec, keys)
  source <- epi_eda_postgres_source(con, schema, "qc_fixture")
  database <- epi_eda_qc_proposals(source, spec, keys)
  after <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_sql, " ORDER BY record_key"))

  discrete <- setdiff(
    qc_evidence_columns,
    c(
      "observed_min", "observed_max", "tukey_lower_fence",
      "tukey_upper_fence"
    )
  )
  expect_identical(database$evidence[discrete], memory$evidence[discrete])
  for (field in c(
    "observed_min", "observed_max", "tukey_lower_fence", "tukey_upper_fence"
  )) {
    expect_equal(database$evidence[[field]], memory$evidence[[field]], tolerance = 1e-10)
  }
  expect_identical(database$proposals, memory$proposals)
  expect_identical(before, after)
  expect_true(DBI::dbIsValid(con))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
})
