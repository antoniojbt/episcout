context("PostgreSQL relationship profile contract")

relationship_pg_connection <- function() {
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

relationship_postgres_fixture <- function(con) {
  schema <- paste0(
    "epi_relationship_", Sys.getpid(), "_", sample.int(1000000L, 1L)
  )
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table <- DBI::Id(schema = schema, table = "Relationship Cases")
  table_sql <- as.character(DBI::dbQuoteIdentifier(con, table))
  DBI::dbExecute(con, paste("CREATE SCHEMA", schema_sql))
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE ", table_sql,
      " (\"Code\" integer, \"Label\" text, \"Flag\" boolean, ",
      "\"Never\" text)"
    )
  )
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ", table_sql,
      " (\"Code\", \"Label\", \"Flag\", \"Never\") VALUES ",
      "(1, 'Alpha', TRUE, NULL), (1, 'Alpha', TRUE, NULL), ",
      "(2, 'Beta', FALSE, NULL), (2, 'Bravo', FALSE, NULL), ",
      "(3, NULL, TRUE, NULL), (NULL, 'Orphan', FALSE, NULL), ",
      "(NULL, NULL, NULL, NULL), (4, 'NULL', TRUE, NULL), ",
      "(5, '', FALSE, NULL)"
    )
  )
  list(schema = schema, schema_sql = schema_sql, table_sql = table_sql)
}

relationship_class_pg_fixture <- function(con) {
  schema <- paste0(
    "epi_relationship_classes_", Sys.getpid(), "_", sample.int(1000000L, 1L)
  )
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  DBI::dbExecute(con, paste("CREATE SCHEMA", schema_sql))
  cases <- data.frame(
    "One Left" = c("01", "02", "03", "04"),
    "One Right" = c("Alpha", "Beta", "Gamma", "Delta"),
    "Many Left" = c("a", "b", "c", "d"),
    "Many Right" = c("X", "X", "Y", "Y"),
    "One Many Left" = c("a", "a", "b", "b"),
    "One Many Right" = c("X", "Y", "Z", "W"),
    "Many Many Left" = c("a", "a", "b", "b"),
    "Many Many Right" = c("X", "Y", "X", "Z"),
    "Constant Left" = rep("constant", 4),
    "Constant Left Right" = c("X", "Y", "Z", "W"),
    "Constant Right Left" = c("a", "b", "c", "d"),
    "Constant Right" = rep("constant", 4),
    "Literal Left" = c("", "NA", "NULL", "ordinary"),
    "Literal Right" = c("empty", "na-token", "null-token", "ordinary"),
    "All Null Left" = rep(NA_character_, 4),
    "All Null Right" = rep(NA_character_, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = schema, table = "Class Cases"),
    cases,
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = schema, table = "Empty Cases"),
    cases[0, c("One Left", "One Right")],
    row.names = FALSE
  )
  list(schema = schema, schema_sql = schema_sql)
}

test_that("live PostgreSQL returns bounded neutral relationship aggregates", {
  con <- relationship_pg_connection()
  fixture <- relationship_postgres_fixture(con)
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste("DROP SCHEMA", fixture$schema_sql, "CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  dictionary <- epi_eda_dictionary_scaffold(
    epi_db_inventory(con, fixture$schema)
  )
  pairs <- data.frame(
    left_schema = fixture$schema,
    left_table = "Relationship Cases",
    left_column = "Code",
    right_schema = fixture$schema,
    right_table = "Relationship Cases",
    right_column = c("Never", "Label"),
    stringsAsFactors = FALSE
  )

  before <- DBI::dbGetQuery(
    con,
    paste("SELECT COUNT(*)::integer AS n FROM", fixture$table_sql)
  )$n[[1]]
  profile <- epi_db_relationship_profile(
    con,
    dictionary,
    pairs,
    max_levels = 5
  )
  after <- DBI::dbGetQuery(
    con,
    paste("SELECT COUNT(*)::integer AS n FROM", fixture$table_sql)
  )$n[[1]]

  expect_identical(class(profile), c("epi_db_relationship_profile", "list"))
  expect_identical(profile$summary$right_column, c("Label", "Never"))
  label <- profile$summary[profile$summary$right_column == "Label", ]
  expect_equal(
    label[c(
      "total_rows", "both_present", "left_missing", "right_missing",
      "both_missing", "distinct_left", "distinct_right",
      "distinct_combinations", "max_right_per_left", "max_left_per_right",
      "left_values_with_multiple_right", "right_values_with_multiple_left"
    )],
    data.frame(
      total_rows = 9, both_present = 6, left_missing = 1, right_missing = 1,
      both_missing = 1, distinct_left = 4, distinct_right = 5,
      distinct_combinations = 5, max_right_per_left = 2, max_left_per_right = 1,
      left_values_with_multiple_right = 1, right_values_with_multiple_left = 0
    )
  )
  expect_identical(label$relationship_class, "one_to_many")
  never <- profile$summary[profile$summary$right_column == "Never", ]
  expect_identical(never$both_present, 0)
  expect_identical(never$relationship_class, "insufficient_data")
  expect_true(all(never[c(
    "distinct_left", "distinct_right", "distinct_combinations",
    "max_right_per_left", "max_left_per_right",
    "left_values_with_multiple_right", "right_values_with_multiple_left"
  )] == 0))

  label_mappings <- profile$mappings[
    profile$mappings$right_column == "Label",
    c("left_value", "right_value", "n")
  ]
  expect_identical(
    label_mappings,
    data.frame(
      left_value = c("1", "2", "2", "4", "5"),
      right_value = c("Alpha", "Beta", "Bravo", "NULL", ""),
      n = c(2, 1, 1, 1, 1),
      stringsAsFactors = FALSE
    )
  )
  label_conflicts <- profile$conflicts[
    profile$conflicts$right_column == "Label",
  ]
  expect_identical(
    label_conflicts$exception_type,
    c(
      "left_maps_multiple_right", "left_maps_multiple_right", "both_missing",
      "left_missing", "right_missing"
    )
  )
  expect_identical(label_conflicts$left_value[1:2], c("2", "2"))
  expect_identical(label_conflicts$right_value[1:2], c("Beta", "Bravo"))
  expect_true(all(is.na(label_conflicts$left_value[3:5])))
  expect_true(all(is.na(label_conflicts$right_value[3:5])))
  expect_identical(before, after)

  expect_error(
    epi_db_relationship_profile(con, dictionary, pairs, max_levels = 4),
    "5 distinct non-NULL combinations exceed max_levels = 4",
    fixed = TRUE
  )
})

test_that("live PostgreSQL covers every class and deterministic edge cases", {
  con <- relationship_pg_connection()
  fixture <- relationship_class_pg_fixture(con)
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste("DROP SCHEMA", fixture$schema_sql, "CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  dictionary <- epi_eda_dictionary_scaffold(
    epi_db_inventory(con, fixture$schema)
  )
  columns <- data.frame(
    left = c(
      "One Left", "Many Left", "One Many Left", "Many Many Left",
      "Constant Left", "Constant Right Left", "Literal Left", "All Null Left"
    ),
    right = c(
      "One Right", "Many Right", "One Many Right", "Many Many Right",
      "Constant Left Right", "Constant Right", "Literal Right", "All Null Right"
    ),
    expected = c(
      "one_to_one", "many_to_one", "one_to_many", "many_to_many",
      "constant_left", "constant_right", "one_to_one", "insufficient_data"
    ),
    stringsAsFactors = FALSE
  )
  pairs <- data.frame(
    left_schema = fixture$schema,
    left_table = "Class Cases",
    left_column = columns$left,
    right_schema = fixture$schema,
    right_table = "Class Cases",
    right_column = columns$right,
    stringsAsFactors = FALSE
  )

  first <- epi_db_relationship_profile(con, dictionary, pairs, max_levels = 10)
  second <- epi_db_relationship_profile(con, dictionary, pairs, max_levels = 10)

  observed <- stats::setNames(
    first$summary$relationship_class,
    first$summary$left_column
  )
  expect_identical(unname(observed[columns$left]), columns$expected)
  expect_identical(first, second)
  literal <- first$mappings[first$mappings$left_column == "Literal Left", ]
  expect_identical(literal$left_value, c("", "NA", "NULL", "ordinary"))
  all_null <- first$summary[first$summary$left_column == "All Null Left", ]
  expect_identical(all_null$total_rows, 4)
  expect_identical(all_null$both_missing, 4)
  expect_identical(all_null$distinct_combinations, 0)

  empty_pairs <- data.frame(
    left_schema = fixture$schema,
    left_table = "Empty Cases",
    left_column = "One Left",
    right_schema = fixture$schema,
    right_table = "Empty Cases",
    right_column = "One Right",
    stringsAsFactors = FALSE
  )
  empty <- epi_db_relationship_profile(
    con,
    dictionary,
    empty_pairs,
    max_levels = 10
  )
  expect_identical(empty$summary$total_rows, 0)
  expect_identical(empty$summary$relationship_class, "insufficient_data")
  expect_equal(nrow(empty$mappings), 0L)
  expect_equal(nrow(empty$conflicts), 0L)
})
