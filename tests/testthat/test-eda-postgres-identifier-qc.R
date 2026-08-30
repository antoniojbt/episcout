context("PostgreSQL identifier QC")

identifier_qc_pg_connection <- function() {
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

test_that("identifier QC applies the declared regex case sensitivity", {
  con <- identifier_qc_pg_connection()
  schema <- paste0(
    "identifier_case_", Sys.getpid(), "_", sample.int(1000000L, 1L)
  )
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  relation <- "neutral_identifiers"
  DBI::dbExecute(con, paste("CREATE SCHEMA", schema_sql))
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste("DROP SCHEMA", schema_sql, "CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = schema, table = relation),
    data.frame(identifier = c("ABC", "abc"), stringsAsFactors = FALSE),
    row.names = FALSE
  )
  source <- epi_eda_postgres_source(con, schema, relation)

  audit <- function(case_sensitive) {
    epi_eda_identifier_qc(
      source,
      data.frame(
        name = "identifier",
        expected_length = NA_integer_,
        pattern = "^[A-Z]+$",
        case_sensitive = case_sensitive,
        provenance = "neutral mixed-case fixture",
        stringsAsFactors = FALSE
      )
    )
  }
  sensitive <- audit(TRUE)
  insensitive <- audit(FALSE)

  expect_identical(sensitive$identifier_audit$n_pattern_violations, 1)
  expect_identical(insensitive$identifier_audit$n_pattern_violations, 0)
  expect_identical(sensitive$identifier_audit$n_case_variations, 1)
  expect_identical(insensitive$identifier_audit$n_case_variations, 1)
  expect_identical(sensitive$identifier_audit$n_distinct, 2)
  expect_identical(insensitive$identifier_audit$n_distinct, 2)
  expect_true(sensitive$identifier_audit$case_sensitive)
  expect_false(insensitive$identifier_audit$case_sensitive)

  restricted_values <- c("ABC", "abc")
  expect_false(any(restricted_values %in% unlist(sensitive, use.names = FALSE)))
  expect_false(any(restricted_values %in% unlist(insensitive, use.names = FALSE)))
  printed <- paste(
    c(capture.output(print(sensitive)), capture.output(print(insensitive))),
    collapse = " "
  )
  expect_false(any(vapply(
    restricted_values,
    grepl,
    logical(1),
    x = printed,
    fixed = TRUE
  )))
})
