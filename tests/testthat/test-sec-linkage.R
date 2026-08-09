library(episcout)
library(testthat)

context("longitudinal linkage metadata")

make_linkage_dictionary <- function() {
  columns <- data.frame(
    source_schema = rep("source_data", 5),
    source_table = c("entities", "entities", "events", "events", "events"),
    source_column = c(
      "entity_code", "entity_kind", "event_entity_code",
      "event_sequence", "event_kind"
    ),
    source_ordinal = c(1L, 2L, 1L, 2L, 3L),
    source_data_type = c(
      "character varying", "text", "character varying", "integer", "text"
    ),
    source_udt_name = c("varchar", "text", "varchar", "int4", "text"),
    source_is_nullable = rep("NO", 5),
    source_character_maximum_length = c(24, NA, 24, NA, NA),
    source_numeric_precision = c(NA, NA, NA, 32, NA),
    source_numeric_scale = c(NA, NA, NA, 0, NA),
    source_column_comment = rep(NA_character_, 5),
    stringsAsFactors = FALSE
  )
  inventory <- structure(
    list(
      tables = unique(columns[c("source_schema", "source_table")]),
      columns = columns,
      constraints = data.frame()
    ),
    class = c("epi_db_inventory", "list")
  )
  epi_eda_dictionary_scaffold(inventory)
}

make_linkage_columns <- function() {
  dictionary <- make_linkage_dictionary()
  identifier <- dictionary$source_column %in% c("entity_code", "event_entity_code")
  data.frame(
    dictionary[c("source_schema", "source_table", "source_column")],
    privacy_class = ifelse(identifier, "direct_identifier", "non_sensitive"),
    analytic_action = ifelse(identifier, "bridge", "retain"),
    validation_status = "confirmed",
    stringsAsFactors = FALSE
  )
}

make_linkage_tables <- function() {
  data.frame(
    source_schema = c("source_data", "source_data"),
    source_table = c("entities", "events"),
    id_column = c("entity_code", "event_entity_code"),
    identity_namespace = c("entity_codes", "event_codes"),
    can_enrol = c(TRUE, FALSE),
    one_row_per_entity = c(TRUE, FALSE),
    destination_table = c("entities", "events"),
    provenance = c("reviewed_dictionary", "reviewed_dictionary"),
    validation_status = c("confirmed", "confirmed"),
    stringsAsFactors = FALSE
  )
}

make_linkage_record_keys <- function() {
  data.frame(
    source_schema = c("source_data", "source_data"),
    source_table = c("events", "events"),
    key_column = c("event_sequence", "event_kind"),
    key_order = c(1L, 2L),
    stringsAsFactors = FALSE
  )
}

make_linkage_crosswalks <- function() {
  data.frame(
    crosswalk_schema = "identity_data",
    crosswalk_table = "entity_aliases",
    alias_namespace = "event_codes",
    alias_id_column = "alias_code",
    canonical_namespace = "entity_codes",
    canonical_id_column = "canonical_code",
    provenance = "reviewed_crosswalk",
    validation_status = "confirmed",
    stringsAsFactors = FALSE
  )
}

test_that("linkage scaffold separates semantic dictionary and column policy", {
  dictionary <- make_linkage_dictionary()
  dictionary$catalog_name[dictionary$source_column == "entity_kind"] <- "entity_kinds"
  scaffold <- epi_sec_linkage_scaffold(dictionary)

  expect_s3_class(scaffold, "epi_sec_linkage_scaffold")
  expect_named(scaffold, c("tables", "columns", "record_keys", "crosswalks"))
  expect_equal(scaffold$tables$source_table, c("entities", "events"))
  expect_true(all(scaffold$tables$id_column == ""))
  expect_equal(scaffold$tables$destination_table, scaffold$tables$source_table)
  expect_true(all(is.na(scaffold$tables$can_enrol)))
  expect_true(all(is.na(scaffold$tables$one_row_per_entity)))
  expect_true(all(scaffold$tables$identity_namespace == ""))
  expect_true(all(scaffold$tables$provenance == ""))
  expect_true(all(scaffold$tables$validation_status == "unreviewed"))
  expect_named(scaffold$columns, c(
    "source_schema", "source_table", "source_column", "privacy_class",
    "analytic_action", "validation_status"
  ))
  expect_equal(nrow(scaffold$columns), nrow(dictionary))
  expect_true(all(scaffold$columns$privacy_class == "unclassified"))
  expect_true(all(scaffold$columns$analytic_action == "review"))
  expect_true(all(scaffold$columns$validation_status == "unreviewed"))
  expect_equal(nrow(scaffold$record_keys), 0L)
  expect_equal(nrow(scaffold$crosswalks), 0L)
})

test_that("linkage scaffold supports explicit metadata-only table selection", {
  selection <- data.frame(
    source_schema = "source_data",
    source_table = "events",
    stringsAsFactors = FALSE
  )
  scaffold <- epi_sec_linkage_scaffold(make_linkage_dictionary(), selection)

  expect_equal(scaffold$tables$source_table, "events")
  expect_true(all(scaffold$columns$source_table == "events"))
  expect_error(
    epi_sec_linkage_scaffold(
      make_linkage_dictionary(),
      transform(selection, source_identifier = "opaque-001")
    ),
    "must not contain row values"
  )
  selection$source_table <- "absent"
  expect_error(
    epi_sec_linkage_scaffold(make_linkage_dictionary(), selection),
    "present and active"
  )
  expect_error(
    epi_sec_linkage_scaffold(make_linkage_dictionary(), selection[0, ]),
    "select at least one"
  )

  dictionary_path <- tempfile(fileext = ".csv")
  on.exit(unlink(dictionary_path), add = TRUE)
  utils::write.csv(make_linkage_dictionary(), dictionary_path, row.names = FALSE, na = "")
  csv_scaffold <- epi_sec_linkage_scaffold(dictionary_path)
  expect_equal(csv_scaffold$tables$source_table, c("entities", "events"))
})

test_that("linkage specification normalises all four metadata components", {
  keys <- make_linkage_record_keys()[2:1, ]
  tables <- make_linkage_tables()[rev(names(make_linkage_tables()))]
  columns <- make_linkage_columns()[5:1, ]
  spec <- epi_sec_linkage_spec(
    tables,
    columns,
    record_keys = keys,
    crosswalks = make_linkage_crosswalks()
  )

  expect_s3_class(spec, "epi_sec_linkage_spec")
  expect_named(spec, c("tables", "columns", "record_keys", "crosswalks"))
  expect_named(spec$tables, names(make_linkage_tables()))
  expect_named(spec$columns, names(make_linkage_columns()))
  expect_type(spec$tables$can_enrol, "logical")
  expect_type(spec$tables$one_row_per_entity, "logical")
  expect_equal(spec$record_keys$key_column, c("event_sequence", "event_kind"))
  expect_type(spec$record_keys$key_order, "integer")
  expect_equal(spec$crosswalks$alias_namespace, "event_codes")
  expect_named(spec$crosswalks, names(make_linkage_crosswalks()))
})

test_that("linkage specification reads CSV metadata robustly", {
  tables <- make_linkage_tables()
  tables$can_enrol <- c("yes", "no")
  tables$one_row_per_entity <- c("1", "0")
  paths <- rep(NA_character_, 4)
  paths <- vapply(seq_along(paths), function(index) tempfile(fileext = ".csv"), character(1))
  on.exit(unlink(paths), add = TRUE)
  utils::write.csv(tables, paths[[1]], row.names = FALSE, na = "")
  utils::write.csv(make_linkage_columns(), paths[[2]], row.names = FALSE, na = "")
  utils::write.csv(make_linkage_record_keys(), paths[[3]], row.names = FALSE, na = "")
  utils::write.csv(make_linkage_crosswalks(), paths[[4]], row.names = FALSE, na = "")

  spec <- epi_sec_linkage_spec(
    paths[[1]], paths[[2]], record_keys = paths[[3]], crosswalks = paths[[4]]
  )
  expect_identical(spec$tables$can_enrol, c(TRUE, FALSE))
  expect_identical(spec$tables$one_row_per_entity, c(TRUE, FALSE))
  expect_identical(spec$record_keys$key_order, 1:2)
  expect_error(epi_sec_linkage_spec(tempfile(fileext = ".csv")), "columns component is required")
})

test_that("old linkage calls fail with migration guidance", {
  expect_error(
    epi_sec_linkage_spec(make_linkage_tables()),
    "Move privacy_class, analytic_action and validation_status.*columns"
  )
})

test_that("linkage table decisions are explicit", {
  columns <- make_linkage_columns()
  tables <- make_linkage_tables()
  tables$can_enrol <- FALSE
  expect_error(epi_sec_linkage_spec(tables, columns), "exactly one can_enrol")

  tables <- make_linkage_tables()
  tables$validation_status[[2]] <- "pending"
  expect_error(epi_sec_linkage_spec(tables, columns), "must be 'confirmed'")

  tables <- make_linkage_tables()
  tables$destination_table[[2]] <- tables$destination_table[[1]]
  expect_error(epi_sec_linkage_spec(tables, columns), "destination_table values must be unique")

  tables <- make_linkage_tables()
  tables$source_table[[2]] <- tables$source_table[[1]]
  expect_error(epi_sec_linkage_spec(tables, columns), "pairs must be unique")

  tables <- make_linkage_tables()
  tables$can_enrol[[2]] <- "sometimes"
  expect_error(epi_sec_linkage_spec(tables, columns), "non-missing logical values")

  tables <- make_linkage_tables()
  tables$id_column[[2]] <- ""
  expect_error(epi_sec_linkage_spec(tables, columns), "id_column.*non-empty")

  tables <- make_linkage_tables()
  tables$identifier_value <- "opaque-001"
  expect_error(epi_sec_linkage_spec(tables, columns), "must not contain row values")
})

test_that("column policy is complete, confirmed and consistent with identifiers", {
  tables <- make_linkage_tables()
  columns <- make_linkage_columns()

  missing_table <- columns[columns$source_table != "events", ]
  expect_error(epi_sec_linkage_spec(tables, missing_table), "cover every declared source table")
  duplicate <- rbind(columns, columns[1, ])
  expect_error(epi_sec_linkage_spec(tables, duplicate), "source keys must be unique")
  pending <- columns
  pending$validation_status[[1]] <- "pending"
  expect_error(epi_sec_linkage_spec(tables, pending), "must be 'confirmed'")
  unclassified <- columns
  unclassified$privacy_class[[2]] <- "unclassified"
  expect_error(epi_sec_linkage_spec(tables, unclassified), "classified supported output action")
  wrong_bridge <- columns
  wrong_bridge$source_column[[1]] <- "other_id"
  expect_error(epi_sec_linkage_spec(tables, wrong_bridge), "id_column must match")
  extra_direct <- columns
  extra_direct$privacy_class[[2]] <- "direct_identifier"
  expect_error(epi_sec_linkage_spec(tables, extra_direct), "additional direct identifier.*drop")
})

test_that("record keys are table-bound, retained, ordered and distinct from identifiers", {
  tables <- make_linkage_tables()
  columns <- make_linkage_columns()
  keys <- make_linkage_record_keys()
  keys$key_order <- c(1L, 1L)
  expect_error(
    epi_sec_linkage_spec(tables, columns, keys),
    "key_order values must be unique"
  )

  keys <- make_linkage_record_keys()
  keys$source_table <- "entities"
  expect_error(epi_sec_linkage_spec(tables, columns, keys), "one_row_per_entity")

  keys <- make_linkage_record_keys()[1, ]
  keys$key_column <- "event_entity_code"
  expect_error(epi_sec_linkage_spec(tables, columns, keys), "must not repeat.*id_column")

  keys <- make_linkage_record_keys()[1, ]
  keys$source_table <- "absent"
  expect_error(epi_sec_linkage_spec(tables, columns, keys), "declared source table")

  keys <- make_linkage_record_keys()[1, ]
  keys$key_order <- 0
  expect_error(epi_sec_linkage_spec(tables, columns, keys), "positive whole numbers")

  dropped <- columns
  dropped$analytic_action[dropped$source_column == "event_sequence"] <- "drop"
  expect_error(epi_sec_linkage_spec(tables, dropped, make_linkage_record_keys()), "retain")
})

test_that("crosswalk specifications contain relation metadata only", {
  tables <- make_linkage_tables()
  columns <- make_linkage_columns()
  crosswalks <- make_linkage_crosswalks()
  crosswalks$source_id <- "opaque-001"
  expect_error(
    epi_sec_linkage_spec(tables, columns, crosswalks = crosswalks),
    "must not contain row values"
  )

  crosswalks <- make_linkage_crosswalks()
  crosswalks$validation_status <- "unreviewed"
  expect_error(epi_sec_linkage_spec(tables, columns, crosswalks = crosswalks), "must be 'confirmed'")

  crosswalks <- make_linkage_crosswalks()
  crosswalks$alias_namespace <- "undeclared_codes"
  expect_error(epi_sec_linkage_spec(tables, columns, crosswalks = crosswalks), "declared by a source table")

  crosswalks <- make_linkage_crosswalks()
  crosswalks$alias_namespace <- "entity_codes"
  crosswalks$canonical_namespace <- "entity_codes"
  same_namespace <- epi_sec_linkage_spec(tables, columns, crosswalks = crosswalks)
  expect_equal(same_namespace$crosswalks$canonical_namespace, "entity_codes")

  crosswalks <- make_linkage_crosswalks()
  crosswalks$canonical_namespace <- "existing_registry_codes"
  expect_error(epi_sec_linkage_spec(tables, columns, crosswalks = crosswalks), "enrolment table namespace")

  crosswalks <- rbind(make_linkage_crosswalks(), make_linkage_crosswalks())
  expect_error(epi_sec_linkage_spec(tables, columns, crosswalks = crosswalks), "at most one relation")
})

test_that("linkage print methods are friendly and redact metadata values", {
  spec <- epi_sec_linkage_spec(
    make_linkage_tables(),
    make_linkage_columns(),
    make_linkage_record_keys(),
    make_linkage_crosswalks()
  )
  output <- paste(utils::capture.output(print(spec)), collapse = "\n")

  expect_match(output, "Confirmed metadata for 2 source table")
  expect_match(output, "Confirmed column policies: 5")
  expect_match(output, "Identifier values: not present")
  expect_false(grepl("entity_code", output, fixed = TRUE))
  expect_false(grepl("event_sequence", output, fixed = TRUE))
  expect_false(grepl("identity_data", output, fixed = TRUE))

  scaffold_output <- paste(
    utils::capture.output(print(epi_sec_linkage_scaffold(make_linkage_dictionary()))),
    collapse = "\n"
  )
  expect_match(scaffold_output, "Review required")
  expect_match(scaffold_output, "Column policies: 5")
  expect_false(grepl("entity_code", scaffold_output, fixed = TRUE))
})
