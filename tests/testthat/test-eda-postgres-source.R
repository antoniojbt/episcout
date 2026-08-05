context("PostgreSQL EDA source and query boundary")

test_that("PostgreSQL EDA public formals and fixed contracts are stable", {
  expect_identical(names(formals(epi_eda_postgres_source)), c("con", "schema", "relation"))
  expect_identical(
    names(formals(epi_eda_db_run)),
    c("source", "spec", "output_dir", "overwrite", "plots", "max_plot_levels")
  )
  expect_identical(names(formals(epi_eda_check_schema)), c("data", "spec"))
  expect_identical(names(formals(epi_eda_profile_missing)), c("data", "spec"))
  expect_identical(names(formals(epi_eda_profile_summaries)), c("data", "spec"))
  expect_identical(names(formals(epi_eda_profile_plots)), c("data", "spec"))
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
  empty <- collapse(data.frame(level = character(), n = integer()), 20L)
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), c("level", "count", "display_order", "remainder"))

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

test_that("data-frame identifier and text plotting policies match the backend contract", {
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
  expect_identical(summaries$variables$status, c("skipped", "summarised"))
  expect_false("participant_id" %in% summaries$text$name)
  expect_named(plots, spec$name)
  expect_null(plots$participant_id)
  expect_s3_class(plots$note, "ggplot")
  expect_identical(names(plots$note$data), c("bin", "lower", "upper", "midpoint", "count"))
  expect_false(any(grepl("CANARY", capture.output(str(plots)), fixed = TRUE)))
})
