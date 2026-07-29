context("reusable database dictionary contract")

library(episcout)
library(testthat)

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
  tables$table_type <- "BASE TABLE"
  tables$table_comment <- NA_character_
  tables$row_count <- NA_real_
  constraints <- data.frame(
    source_schema = "study",
    source_table = "cohort_a",
    constraint_name = "cohort_a_code_key",
    constraint_type = "UNIQUE",
    source_columns = "subject_code",
    stringsAsFactors = FALSE
  )
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
