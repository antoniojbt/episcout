context("PostgreSQL catalogue missingness contract")

catalogue_postgres_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    skip("Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL tests.")
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

catalogue_postgres_fixture <- function(con) {
  schema <- paste0("epi_catalogue_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  DBI::dbExecute(con, paste("CREATE SCHEMA", schema_sql))

  list(schema = schema, schema_sql = schema_sql)
}

populate_catalogue_fixture <- function(con, schema_sql) {
  statements <- c(
    "CREATE TABLE zero_rows (category text)",
    "CREATE TABLE all_null (category text)",
    "INSERT INTO all_null VALUES (NULL), (NULL)",
    "CREATE TABLE exact_limit (category text)",
    "INSERT INTO exact_limit VALUES ('A'), ('A'), ('B'), (NULL)",
    "CREATE TABLE over_limit (category text)",
    "INSERT INTO over_limit VALUES ('A'), ('B'), ('C'), (NULL)"
  )
  DBI::dbExecute(con, paste("SET search_path TO", schema_sql))
  for (statement in statements) {
    DBI::dbExecute(con, statement)
  }
  invisible(TRUE)
}

catalogue_profile_dictionary <- function(con, schema) {
  dictionary <- epi_eda_dictionary_scaffold(epi_db_inventory(con, schema))
  dictionary$type <- "categorical"
  dictionary$privacy_class <- "non_sensitive"
  dictionary$analytic_action <- "retain"
  dictionary$provenance <- "synthetic_postgresql_fixture"
  dictionary$validation_status <- "confirmed"
  dictionary
}

profile_catalogue_table <- function(con, dictionary, table, max_levels = 2) {
  selected <- dictionary
  selected$profile_catalogue <- selected$source_table == table
  epi_db_catalogue_profile(con, selected, max_levels = max_levels)
}

test_that("live PostgreSQL keeps NULL counts separate from catalogue values", {
  con <- catalogue_postgres_connection()
  fixture <- catalogue_postgres_fixture(con)
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste("DROP SCHEMA", fixture$schema_sql, "CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  populate_catalogue_fixture(con, fixture$schema_sql)
  dictionary <- catalogue_profile_dictionary(con, fixture$schema)

  zero <- profile_catalogue_table(con, dictionary, "zero_rows")
  expect_equal(nrow(zero$values), 0L)
  expect_identical(zero$missing$n_missing, 0)

  all_null <- profile_catalogue_table(con, dictionary, "all_null")
  expect_equal(nrow(all_null$values), 0L)
  expect_identical(all_null$missing$n_missing, 2)

  exact <- profile_catalogue_table(con, dictionary, "exact_limit")
  expect_identical(exact$values$source_value, c("A", "B"))
  expect_identical(exact$values$n, c(2, 1))
  expect_false(anyNA(exact$values$source_value))
  expect_identical(exact$missing$n_missing, 1)

  expect_error(
    profile_catalogue_table(con, dictionary, "over_limit"),
    "3 distinct values exceed max_levels = 2",
    fixed = TRUE
  )
})
