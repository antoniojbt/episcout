context("PostgreSQL EDA source and query boundary")

test_that("PostgreSQL EDA public formals and fixed contracts are stable", {
  expect_identical(names(formals(epi_eda_postgres_source)), c("con", "schema", "relation"))
  expect_identical(
    names(formals(epi_eda_db_run)),
    c(
      "source", "spec", "output_dir", "overwrite", "plots",
      "max_plot_levels", "maps", "map_vars", "max_map_points", "layout",
      "quiet"
    )
  )
  expect_identical(names(formals(epi_eda_check_schema)), c("data", "spec"))
  expect_identical(names(formals(epi_eda_profile_missing)), c("data", "spec"))
  expect_identical(names(formals(epi_eda_profile_summaries)), c("data", "spec"))
  expect_identical(names(formals(epi_eda_profile_plots)), c("data", "spec"))
})

test_that("categorical summaries reuse the supplied relation count", {
  categorical_summary <- getFromNamespace(
    "eda_pg_categorical_summary",
    "episcout"
  )
  source <- list(con = NULL)
  column <- data.frame(name = "group", stringsAsFactors = FALSE)
  contract <- list(sql = "FALSE", params = list())
  spec_row <- data.frame(
    type = "categorical",
    levels = "A;B",
    stringsAsFactors = FALSE
  )

  observed <- with_mocked_bindings(
    categorical_summary(
      source,
      column,
      contract,
      spec_row,
      1L,
      4L,
      NULL
    ),
    eda_postgres_value_expression = function(...) "value",
    eda_postgres_table_sql = function(...) "fixture_relation",
    eda_db_fetch = function(...) {
      data.frame(
        level = c("A", "B"),
        n = c("2", "1"),
        stringsAsFactors = FALSE
      )
    },
    eda_postgres_row_count = function(...) {
      stop("REDUNDANT_ROW_COUNT", call. = FALSE)
    },
    .package = "episcout"
  )

  expect_identical(observed$data$n, c(2L, 1L))
  expect_equal(observed$data$p_total, c(0.5, 0.25), tolerance = 1e-12)
  expect_equal(observed$data$p_observed, c(2 / 3, 1 / 3), tolerance = 1e-12)
  expect_identical(
    observed$counts,
    list(n_missing = 1L, n_observed = 3L, n_unique = 2L, n_infinite = 0L)
  )
})

test_that("categorical plot companions reuse summaries without a database fetch", {
  plot_data <- getFromNamespace("eda_postgres_plot_data_inside", "episcout")
  spec <- data.frame(
    name = "status", label = "Status", type = "categorical", role = "measure",
    stringsAsFactors = FALSE
  )
  summaries <- list(
    variables = data.frame(
      name = "status", status = "summarised", reason = NA_character_,
      n = 7L, n_missing = 2L, n_observed = 5L, stringsAsFactors = FALSE
    ),
    categorical = data.frame(
      name = c("status", "status"), level = c("no", "yes"),
      n = c(2L, 3L), stringsAsFactors = FALSE
    )
  )
  observed <- with_mocked_bindings(
    plot_data(list(), spec, summaries, 20L),
    eda_postgres_column = function(...) data.frame(name = "status"),
    eda_postgres_missing_contract = function(...) list(),
    eda_db_fetch = function(...) stop("UNEXPECTED_DATABASE_FETCH", call. = FALSE),
    .package = "episcout"
  )
  companion <- observed$entries$status$data

  expect_named(
    companion,
    getFromNamespace("eda_frequency_companion_names", "episcout")()
  )
  expect_identical(companion$count, companion$numerator)
  expect_identical(companion$denominator, c(5L, 5L))
  expect_equal(companion$proportion, c(3 / 5, 2 / 5))
})

test_that("identifier and exact count validation refuse ambiguous inputs", {
  identifier <- getFromNamespace("eda_postgres_identifier", "episcout")
  checked_count <- getFromNamespace("eda_checked_count", "episcout")

  expect_identical(identifier("odd table", "relation"), "odd table")
  expect_identical(identifier("select", "relation"), "select")
  expect_error(identifier("public.table", "relation"), "undotted")
  expect_error(identifier("table; DROP TABLE x", "relation"), "SQL fragments")
  expect_error(identifier(DBI::SQL("table"), "relation"), "plain character")
  expect_identical(checked_count("2147483647"), .Machine$integer.max)
  expect_error(checked_count("2147483648"), "integer range")
  expect_error(checked_count("1.5"), "decimal text")
})

test_that("redacted source methods reveal no connection or relation identity", {
  source <- structure(
    list(
      con = structure(list(host = "CANARY_HOST", password = "CANARY_PASSWORD"), class = "secret_connection"),
      schema = "CANARY_SCHEMA", relation = "CANARY_RELATION", relation_kind = "view",
      columns = data.frame(name = c("id", "value")), source_version = "postgres-source-1"
    ),
    class = c("epi_eda_postgres_source", "list")
  )
  printed <- capture.output(print(source))
  structured <- capture.output(str(source))
  output <- paste(c(printed, structured), collapse = "\n")

  expect_match(output, "relation kind: view", fixed = TRUE)
  expect_match(output, "columns: 2", fixed = TRUE)
  expect_false(grepl("CANARY", output, fixed = TRUE))
  expect_false(grepl("password|host", output, ignore.case = TRUE))
})

test_that("explicit PostgreSQL dispatch rejects forged and generic DBI objects", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  spec <- data.frame(name = "x", label = "X", type = "numeric", role = "measure")
  forged <- structure(
    list(con = con, schema = "main", relation = "x", relation_kind = "table", columns = data.frame(), source_version = "postgres-source-1"),
    class = c("epi_eda_postgres_source", "list")
  )

  expect_error(epi_eda_postgres_source(con, "main", "x"), "RPostgres")
  expect_error(epi_eda_check_schema(con, spec), "data frame or")
  expect_error(epi_eda_profile_missing(forged, spec), "unmodified object")
})

test_that("query boundary clears results and enforces fetch limits", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fetch <- getFromNamespace("eda_db_fetch", "episcout")

  observed <- fetch(con, "SELECT ? AS value", params = list("bound"), query_kind = "test_scalar", limit = 1L)
  expect_identical(observed$value, "bound")
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
  expect_error(fetch(con, "SELECT 1 UNION ALL SELECT 2", query_kind = "test_scalar", limit = 1L), "test_scalar")
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("transaction lifecycle failures remain value-free", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- structure(list(con = con), class = c("epi_eda_postgres_source", "list"))
  transaction <- getFromNamespace("eda_postgres_transaction", "episcout")

  begin_error <- tryCatch(
    with_mocked_bindings(
      transaction(source, 1L),
      eda_validate_postgres_source = function(...) invisible(TRUE),
      eda_db_begin = function(...) stop("BEGIN_CANARY", call. = FALSE),
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(begin_error), "could not begin", fixed = TRUE)
  expect_false(grepl("CANARY", conditionMessage(begin_error), fixed = TRUE))

  commit_error <- tryCatch(
    with_mocked_bindings(
      transaction(source, 1L),
      eda_validate_postgres_source = function(...) invisible(TRUE),
      eda_db_statement = function(...) invisible(TRUE),
      eda_db_commit = function(...) stop("COMMIT_CANARY", call. = FALSE),
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(commit_error), "could not commit safely", fixed = TRUE)
  expect_false(grepl("CANARY", conditionMessage(commit_error), fixed = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("database messages and warnings retain semantics without native text", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- structure(list(con = con), class = c("epi_eda_postgres_source", "list"))
  transaction <- getFromNamespace("eda_postgres_transaction", "episcout")
  observed_messages <- character()
  observed_warnings <- character()

  value <- withCallingHandlers(
    with_mocked_bindings(
      transaction(source, 42L),
      eda_validate_postgres_source = function(...) invisible(TRUE),
      eda_db_begin = function(...) {
        message("MESSAGE_CANARY")
        warning("WARNING_CANARY", call. = FALSE)
        invisible(TRUE)
      },
      eda_db_statement = function(...) invisible(TRUE),
      eda_db_commit = function(...) invisible(TRUE),
      .package = "episcout"
    ),
    message = function(condition) {
      observed_messages <<- c(observed_messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    },
    warning = function(condition) {
      observed_warnings <<- c(observed_warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(value, 42L)
  expect_length(observed_messages, 1L)
  expect_length(observed_warnings, 1L)
  expect_match(observed_messages, "database message", fixed = TRUE)
  expect_match(observed_warnings, "database warning", fixed = TRUE)
  expect_false(any(grepl("CANARY", c(observed_messages, observed_warnings), fixed = TRUE)))
})

test_that("result cleanup failures remain value-free", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fetch <- getFromNamespace("eda_db_fetch", "episcout")
  statement <- getFromNamespace("eda_db_statement", "episcout")
  fail_after_clear <- function(result) {
    DBI::dbClearResult(result)
    stop("CLEAR_CANARY", call. = FALSE)
  }

  fetch_error <- tryCatch(
    with_mocked_bindings(
      fetch(con, "SELECT 1 AS value", query_kind = "cleanup_fetch", limit = 1L),
      eda_db_clear_result = fail_after_clear,
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(fetch_error), "cleanup_fetch", fixed = TRUE)
  expect_false(grepl("CANARY", conditionMessage(fetch_error), fixed = TRUE))

  statement_error <- tryCatch(
    with_mocked_bindings(
      statement(
        con,
        "CREATE TEMP TABLE cleanup_statement (value integer)",
        query_kind = "transaction_setup"
      ),
      eda_db_clear_result = fail_after_clear,
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(statement_error), "transaction setup", fixed = TRUE)
  expect_false(grepl("CANARY", conditionMessage(statement_error), fixed = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("storage matrix and sentinel parsing are explicit", {
  compatibility <- getFromNamespace("eda_pg_type_compatibility", "episcout")
  column <- function(base, formatted = base, deterministic = TRUE, typtype = "b") {
    data.frame(
      name = "x", base_udt_name = base, typtype = typtype,
      formatted_type = formatted, collation_deterministic = deterministic,
      stringsAsFactors = FALSE
    )
  }

  expect_identical(compatibility(column("int8"), "integer")$status, "compatible")
  expect_identical(compatibility(NULL, "numeric")$status, "not_applicable")
  expect_identical(compatibility(column("timestamp"), "datetime")$status, "incompatible")
  expect_match(compatibility(column("text", deterministic = FALSE), "text")$reason, "nondeterministic")
  expect_match(compatibility(column("text", deterministic = FALSE), "categorical")$reason, "nondeterministic")
  expect_match(compatibility(column("text"), "binary")$reason, "exactly two")
  expect_identical(compatibility(column("int4"), "categorical")$status, "coercible")
  expect_identical(compatibility(column("bool"), "binary")$status, "compatible")
  expect_identical(compatibility(column("jsonb"), "text")$status, "incompatible")
})

test_that("profiler dispatch rejects unsupported objects and incompatible plot inputs", {
  skip_if_not_installed("ggplot2")
  numeric_spec <- data.frame(
    name = "x", label = "X", type = "numeric", role = "measure",
    stringsAsFactors = FALSE
  )
  text_spec <- transform(numeric_spec, type = "text")

  expect_error(epi_eda_profile_missing(list(), numeric_spec), "data frame")
  expect_error(epi_eda_profile_summaries(list(), numeric_spec), "data frame")
  expect_error(epi_eda_profile_plots(list(), numeric_spec), "data frame")
  expect_error(epi_eda_profile_plots(data.frame(y = 1), numeric_spec), "missing specified")
  expect_error(epi_eda_profile_plots(data.frame(x = "not numeric"), numeric_spec), "incompatible")
  expect_error(epi_eda_profile_plots(data.frame(x = 1), text_spec), "incompatible")

  collapse <- getFromNamespace("eda_collapse_frequencies", "episcout")
  empty <- collapse(getFromNamespace("eda_empty_categorical_display", "episcout")(), 20L)
  expect_equal(nrow(empty), 0L)
  expect_identical(
    names(empty),
    getFromNamespace("eda_frequency_companion_names", "episcout")()
  )

  histogram <- getFromNamespace("eda_histogram_from_counts", "episcout")
  empty_histogram <- histogram(
    NA_real_, NA_real_, data.frame(bin = integer(), count = integer())
  )
  constant_histogram <- histogram(
    5, 5, data.frame(bin = 15L, count = 2L)
  )
  expect_equal(nrow(empty_histogram), 30L)
  expect_equal(sum(empty_histogram$count), 0L)
  expect_equal(sum(constant_histogram$count), 2L)

  inventory <- getFromNamespace("eda_plot_inventory", "episcout")(list())
  expect_equal(nrow(inventory), 0L)
  expect_identical(
    names(inventory),
    c(
      "variable_index", "name", "type", "plot_type", "n_total", "n_missing",
      "n_plotted", "n_excluded_non_finite", "n_displayed_levels",
      "n_collapsed_levels", "status", "reason", "path"
    )
  )
})

test_that("data-frame roles do not suppress text summaries or plots", {
  skip_if_not_installed("ggplot2")
  data <- data.frame(
    participant_id = c("ID_CANARY_A", "ID_CANARY_B", NA),
    note = c("TEXT_CANARY_LONG", "x", NA),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("participant_id", "note"), label = c("Participant", "Note"),
    type = c("text", "text"), role = c("identifier", "comment"),
    stringsAsFactors = FALSE
  )

  summaries <- epi_eda_profile_summaries(data, spec)
  plots <- epi_eda_profile_plots(data, spec)
  expect_identical(summaries$variables$status, c("summarised", "summarised"))
  expect_true("participant_id" %in% summaries$text$name)
  expect_named(plots, spec$name)
  expect_s3_class(plots$participant_id, "ggplot")
  expect_s3_class(plots$note, "ggplot")
  expect_identical(names(plots$note$data), c("bin", "lower", "upper", "midpoint", "count"))
  expect_false(any(grepl("ID_CANARY|TEXT_CANARY", capture.output(str(plots)), fixed = TRUE)))
})
