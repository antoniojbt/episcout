library(episcout)
library(testthat)
library(DBI)

context("reusable database dictionary contract")

if (!methods::isClass("DictionaryMockConnection")) {
  methods::setClass(
    "DictionaryMockConnection",
    contains = "DBIConnection",
    slots = c(state = "environment")
  )
}

methods::setMethod("dbIsValid", "DictionaryMockConnection", function(dbObj, ...) {
  dbObj@state$valid
})

methods::setMethod("dbGetInfo", "DictionaryMockConnection", function(dbObj, ...) {
  list(dbms.name = "PostgreSQL")
})

methods::setMethod("dbGetQuery", signature(conn = "DictionaryMockConnection", statement = "character"), function(conn, statement, ...) {
  arguments <- list(...)
  conn@state$queries <- c(conn@state$queries, statement)
  if (grepl("FROM information_schema.tables", statement, fixed = TRUE)) {
    return(conn@state$tables)
  }
  if (grepl("FROM information_schema.columns", statement, fixed = TRUE)) {
    return(conn@state$columns)
  }
  if (grepl("FROM information_schema.table_constraints", statement, fixed = TRUE)) {
    return(conn@state$constraints)
  }
  if (grepl("reltuples", statement, fixed = TRUE)) {
    table <- arguments$params[[2]]
    return(data.frame(row_count = unname(conn@state$row_counts[[table]])))
  }
  if (grepl("COUNT(DISTINCT", statement, fixed = TRUE)) {
    return(data.frame(n_levels = conn@state$n_levels))
  }
  if (grepl("GROUP BY", statement, fixed = TRUE)) {
    return(data.frame(source_value = c("A", "B"), n = c(3, 2)))
  }
  if (grepl("COUNT(*)", statement, fixed = TRUE)) {
    table <- sub('.*\\."([^"]+)"$', "\\1", statement)
    return(data.frame(row_count = unname(conn@state$row_counts[[table]])))
  }
  stop("Unexpected mock query: ", statement)
})

methods::setMethod(
  "dbQuoteIdentifier",
  signature(conn = "DictionaryMockConnection", x = "Id"),
  function(conn, x, ...) {
    DBI::SQL(paste(sprintf('"%s"', x@name), collapse = "."))
  }
)

methods::setMethod(
  "dbQuoteIdentifier",
  signature(conn = "DictionaryMockConnection", x = "character"),
  function(conn, x, ...) DBI::SQL(sprintf('"%s"', x))
)

make_mock_connection <- function() {
  state <- new.env(parent = emptyenv())
  state$valid <- TRUE
  state$queries <- character()
  state$tables <- data.frame(
    source_schema = rep("study", 2),
    source_table = c("cohort_b", "cohort_a"),
    table_type = "BASE TABLE",
    table_comment = c(NA, "Artificial cohort"),
    stringsAsFactors = FALSE
  )
  state$columns <- rbind(
    make_test_inventory()$columns[3, , drop = FALSE],
    make_test_inventory()$columns[1:2, , drop = FALSE]
  )
  state$columns$source_table[[1]] <- "cohort_b"
  state$columns$source_column[[1]] <- "value"
  state$columns$source_ordinal[[1]] <- 1L
  state$constraints <- rbind(
    make_test_inventory()$constraints,
    transform(
      make_test_inventory()$constraints,
      source_table = "cohort_b",
      constraint_name = "cohort_b_value_key",
      source_columns = "value"
    )
  )
  state$row_counts <- c(cohort_a = 12, cohort_b = 7)
  state$n_levels <- 2
  methods::new("DictionaryMockConnection", state = state)
}

make_test_inventory <- function(columns = NULL) {
  if (is.null(columns)) {
    columns <- data.frame(
      source_schema = rep("study", 3),
      source_table = rep("cohort_a", 3),
      source_column = c("subject_code", "group_code", "score"),
      source_ordinal = 1:3,
      source_data_type = c("character varying", "character varying", "numeric"),
      source_udt_name = c("varchar", "varchar", "numeric"),
      source_is_nullable = c("NO", "YES", "YES"),
      source_character_maximum_length = c(20, 10, NA),
      source_numeric_precision = c(NA, NA, 8),
      source_numeric_scale = c(NA, NA, 2),
      source_column_comment = c(NA, "Assigned study group", NA),
      stringsAsFactors = FALSE
    )
  }
  tables <- unique(columns[c("source_schema", "source_table")])
  tables$table_type <- rep("BASE TABLE", nrow(tables))
  tables$table_comment <- rep(NA_character_, nrow(tables))
  tables$row_count <- rep(NA_real_, nrow(tables))
  constraints <- data.frame(
    source_schema = "study",
    source_table = "cohort_a",
    constraint_name = "cohort_a_code_key",
    constraint_type = "UNIQUE",
    source_columns = "subject_code",
    stringsAsFactors = FALSE
  )
  if (nrow(columns) == 0) constraints <- constraints[0, , drop = FALSE]
  structure(
    list(tables = tables, columns = columns, constraints = constraints),
    class = c("epi_db_inventory", "list")
  )
}

test_that("dictionary scaffold maps technical types without semantic guesses", {
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())

  expect_equal(dictionary$source_column, c("subject_code", "group_code", "score"))
  expect_equal(dictionary$type, c("text", "text", "numeric"))
  expect_equal(dictionary$label, dictionary$source_column)
  expect_true(all(dictionary$privacy_class == "unclassified"))
  expect_true(all(dictionary$analytic_action == "review"))
  expect_true(all(dictionary$validation_status == "unreviewed"))
  expect_equal(dictionary$analytic_order, 1:3)
  expect_silent(epi_eda_dictionary_validate(dictionary))
})

test_that("PostgreSQL inventory is deterministic and supports row count modes", {
  connection <- make_mock_connection()

  inventory <- epi_db_inventory(connection, "study", row_counts = "none")
  expect_equal(inventory$tables$source_table, c("cohort_a", "cohort_b"))
  expect_equal(
    inventory$columns[c("source_table", "source_ordinal")],
    data.frame(
      source_table = c("cohort_a", "cohort_a", "cohort_b"),
      source_ordinal = c(1L, 2L, 1L)
    )
  )
  expect_true(all(is.na(inventory$tables$row_count)))

  estimated <- epi_db_inventory(connection, "study", row_counts = "estimate")
  expect_equal(estimated$tables$row_count, c(12, 7))
  exact <- epi_db_inventory(connection, "study", row_counts = "exact")
  expect_equal(exact$tables$row_count, c(12, 7))

  selected <- epi_db_inventory(connection, "study", tables = "cohort_a")
  expect_equal(selected$tables$source_table, "cohort_a")
  expect_true(all(selected$columns$source_table == "cohort_a"))
  expect_true(all(selected$constraints$source_table == "cohort_a"))
})

test_that("PostgreSQL inventory validates its inputs and table selection", {
  connection <- make_mock_connection()
  expect_error(epi_db_inventory(NULL, "study"), "open DBI connection")
  connection@state$valid <- FALSE
  expect_error(epi_db_inventory(connection, "study"), "open DBI connection")
  connection@state$valid <- TRUE
  expect_error(epi_db_inventory(connection, ""), "schema")
  expect_error(epi_db_inventory(connection, "study", tables = character()), "tables")
  expect_error(epi_db_inventory(connection, "study", tables = c("cohort_a", "cohort_a")), "duplicates")
  expect_error(epi_db_inventory(connection, "study", tables = "missing"), "were not found")
})

test_that("dictionary refresh preserves curation and reports source drift", {
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  dictionary$label[dictionary$source_column == "group_code"] <- "Study group"
  dictionary$privacy_class <- "non_sensitive"
  dictionary$analytic_action <- "retain"
  dictionary$validation_status <- "confirmed"

  current_columns <- make_test_inventory()$columns
  current_columns <- current_columns[current_columns$source_column != "score", , drop = FALSE]
  current_columns$source_data_type[current_columns$source_column == "group_code"] <- "text"
  current_columns <- rbind(
    current_columns,
    data.frame(
      source_schema = "study",
      source_table = "cohort_a",
      source_column = "visit_date",
      source_ordinal = 4L,
      source_data_type = "date",
      source_udt_name = "date",
      source_is_nullable = "YES",
      source_character_maximum_length = NA_real_,
      source_numeric_precision = NA_real_,
      source_numeric_scale = NA_real_,
      source_column_comment = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  refreshed <- epi_eda_dictionary_refresh(dictionary, make_test_inventory(current_columns))

  expect_equal(
    refreshed$label[refreshed$source_column == "group_code"],
    "Study group"
  )
  expect_equal(
    refreshed$drift_status[match(c("subject_code", "group_code", "score", "visit_date"), refreshed$source_column)],
    c("current", "modified", "removed", "added")
  )
  expect_equal(
    refreshed$privacy_class[refreshed$source_column == "score"],
    "non_sensitive"
  )
})

test_that("dictionary validation rejects ambiguous keys and orders", {
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  duplicate <- rbind(dictionary, dictionary[1, , drop = FALSE])
  expect_error(
    epi_eda_dictionary_validate(duplicate),
    "keys must be unique"
  )

  dictionary$analytic_order[[2]] <- dictionary$analytic_order[[1]]
  expect_error(
    epi_eda_dictionary_validate(dictionary),
    "analytic_order values must be unique"
  )

  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  dictionary$provenance[[1]] <- ""
  expect_error(
    epi_eda_dictionary_validate(dictionary),
    "provenance.*non-empty"
  )
})

test_that("dictionary validation rejects invalid contract values", {
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  expect_error(epi_eda_dictionary_validate(list()), "data frame")
  expect_error(
    epi_eda_dictionary_validate(dictionary[setdiff(names(dictionary), "group")]),
    "missing required columns"
  )

  invalid <- dictionary
  invalid$type[[1]] <- "unknown"
  expect_error(epi_eda_dictionary_validate(invalid), "type.*invalid")
  invalid <- dictionary
  invalid$privacy_class[[1]] <- "public"
  expect_error(epi_eda_dictionary_validate(invalid), "privacy_class.*invalid")
  invalid <- dictionary
  invalid$analytic_action[[1]] <- "publish"
  expect_error(epi_eda_dictionary_validate(invalid), "analytic_action.*invalid")
  invalid <- dictionary
  invalid$validation_status[[1]] <- "done"
  expect_error(epi_eda_dictionary_validate(invalid), "validation_status.*invalid")
  invalid <- dictionary
  invalid$drift_status[[1]] <- "changed"
  expect_error(epi_eda_dictionary_validate(invalid), "drift_status.*invalid")
  invalid <- dictionary
  invalid$required <- as.character(invalid$required)
  expect_error(epi_eda_dictionary_validate(invalid), "required")
  invalid <- dictionary
  invalid$profile_catalogue <- as.character(invalid$profile_catalogue)
  expect_error(epi_eda_dictionary_validate(invalid), "profile_catalogue")
  invalid <- dictionary
  invalid$analytic_order[[1]] <- 0
  expect_error(epi_eda_dictionary_validate(invalid), "positive whole")
})

test_that("empty inventories and refreshes keep the dictionary contract", {
  inventory <- make_test_inventory(make_test_inventory()$columns[0, , drop = FALSE])
  dictionary <- epi_eda_dictionary_scaffold(inventory)
  expect_equal(nrow(dictionary), 0L)
  expect_silent(epi_eda_dictionary_validate(dictionary))

  added <- epi_eda_dictionary_refresh(dictionary, make_test_inventory())
  expect_true(all(added$drift_status == "added"))
  expect_error(epi_eda_dictionary_scaffold(list()), "epi_db_inventory")
})

test_that("dictionary specifications use reviewed catalogue values", {
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  dictionary$role <- c("id", "covariate", "outcome")
  dictionary$group <- c("identifiers", "design", "measurements")
  dictionary$privacy_class <- c("direct_identifier", "non_sensitive", "sensitive")
  dictionary$analytic_action <- c("drop", "retain", "retain_restricted")
  dictionary$validation_status <- "confirmed"
  dictionary$type[dictionary$source_column == "group_code"] <- "categorical"
  dictionary$catalog_name[dictionary$source_column == "group_code"] <- "study_group"
  catalogues <- data.frame(
    catalog_name = c("study_group", "study_group"),
    source_value = c("A", "B"),
    label = c("Group A", "Group B"),
    display_order = 1:2,
    is_missing = c(FALSE, TRUE),
    provenance = "test_definition",
    validation_status = "confirmed",
    stringsAsFactors = FALSE
  )

  spec <- epi_eda_dictionary_spec(
    dictionary,
    table = "study.cohort_a",
    catalogues = catalogues
  )

  expect_equal(spec$name, c("subject_code", "group_code", "score"))
  expect_equal(spec$levels, c("", "A;B", ""))
  expect_equal(spec$missing_codes, c("", "B", ""))
  expect_equal(spec$group, c("identifiers", "design", "measurements"))

  catalogues$display_order[[2]] <- catalogues$display_order[[1]]
  expect_error(
    epi_eda_dictionary_validate(dictionary, catalogues),
    "display_order values must be unique"
  )
})

test_that("catalogue validation rejects malformed definitions", {
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  dictionary$type[[2]] <- "categorical"
  dictionary$catalog_name[[2]] <- "study_group"
  catalogue <- data.frame(
    catalog_name = "study_group",
    source_value = "A",
    label = "Group A",
    display_order = 1,
    is_missing = FALSE,
    provenance = "artificial_test",
    validation_status = "confirmed",
    stringsAsFactors = FALSE
  )

  expect_error(epi_eda_dictionary_validate(dictionary), "catalogues must be supplied")
  expect_error(epi_eda_dictionary_validate(dictionary, list()), "data frame")
  expect_error(
    epi_eda_dictionary_validate(dictionary, catalogue[setdiff(names(catalogue), "label")]),
    "missing required columns"
  )
  duplicate <- rbind(catalogue, catalogue)
  expect_error(epi_eda_dictionary_validate(dictionary, duplicate), "keys must be unique")
  invalid <- catalogue
  invalid$source_value <- "A;B"
  expect_error(epi_eda_dictionary_validate(dictionary, invalid), "must not contain semicolons")
  invalid <- catalogue
  invalid$label <- ""
  expect_error(epi_eda_dictionary_validate(dictionary, invalid), "label.*non-empty")
  invalid <- catalogue
  invalid$validation_status <- "done"
  expect_error(epi_eda_dictionary_validate(dictionary, invalid), "invalid values")
  invalid <- catalogue
  invalid$is_missing <- "FALSE"
  expect_error(epi_eda_dictionary_validate(dictionary, invalid), "is_missing")
  invalid <- catalogue
  invalid$display_order <- 0
  expect_error(epi_eda_dictionary_validate(dictionary, invalid), "positive whole")
  dictionary$catalog_name[[2]] <- "missing_catalogue"
  expect_error(epi_eda_dictionary_validate(dictionary, catalogue), "missing catalogues")
  dictionary$catalog_name[[2]] <- "study_group"
  dictionary$type[[2]] <- "text"
  expect_error(epi_eda_dictionary_validate(dictionary, catalogue), "categorical or binary")
})

test_that("table selection is explicit and active", {
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  duplicate_schema <- dictionary
  duplicate_schema$source_schema <- "archive"
  combined <- rbind(dictionary, duplicate_schema)
  combined$analytic_order <- rep(1:3, 2)
  expect_error(epi_eda_dictionary_spec(combined, "cohort_a"), "ambiguous")
  expect_error(epi_eda_dictionary_spec(dictionary, "missing"), "not found")
  dictionary$drift_status <- "removed"
  expect_error(epi_eda_dictionary_spec(dictionary, "study.cohort_a"), "No active columns")
})

test_that("catalogue profiling blocks unsafe and free-text fields before querying", {
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  dictionary$profile_catalogue[[1]] <- TRUE
  dictionary$privacy_class[[1]] <- "direct_identifier"
  dictionary$analytic_action[[1]] <- "drop"
  expect_error(
    epi_db_catalogue_profile(NULL, dictionary),
    "Catalogue profiling is not allowed"
  )

  dictionary$profile_catalogue <- FALSE
  dictionary$profile_catalogue[[2]] <- TRUE
  dictionary$privacy_class[[2]] <- "non_sensitive"
  dictionary$analytic_action[[2]] <- "retain"
  expect_error(
    epi_db_catalogue_profile(NULL, dictionary),
    "Catalogue profiling is not allowed"
  )
})

test_that("catalogue profiling returns only approved aggregate counts", {
  connection <- make_mock_connection()
  dictionary <- epi_eda_dictionary_scaffold(make_test_inventory())
  dictionary$type[[2]] <- "categorical"
  dictionary$privacy_class[[2]] <- "non_sensitive"
  dictionary$analytic_action[[2]] <- "retain"
  dictionary$validation_status[[2]] <- "confirmed"
  dictionary$profile_catalogue[[2]] <- TRUE

  profile <- epi_db_catalogue_profile(connection, dictionary, max_levels = 2)
  expect_equal(profile$source_value, c("A", "B"))
  expect_equal(profile$n, c(3, 2))
  expect_true(all(profile$source_column == "group_code"))

  connection@state$n_levels <- 3
  expect_error(
    epi_db_catalogue_profile(connection, dictionary, max_levels = 2),
    "exceed max_levels"
  )
  expect_error(epi_db_catalogue_profile(connection, dictionary, max_levels = 0), "positive whole")

  dictionary$profile_catalogue <- FALSE
  expect_equal(nrow(epi_db_catalogue_profile(NULL, dictionary)), 0L)
})

test_that("database inventory rejects unsupported DBI backends", {
  skip_if_not_installed("RSQLite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(connection), add = TRUE)

  expect_error(
    epi_db_inventory(connection, schema = "main"),
    "supports PostgreSQL connections only"
  )
})

test_that("new dictionary functions are exported", {
  exports <- getNamespaceExports("episcout")
  expected <- c(
    "epi_db_inventory",
    "epi_db_catalogue_profile",
    "epi_eda_dictionary_scaffold",
    "epi_eda_dictionary_refresh",
    "epi_eda_dictionary_validate",
    "epi_eda_dictionary_spec"
  )
  expect_true(all(expected %in% exports))
})
