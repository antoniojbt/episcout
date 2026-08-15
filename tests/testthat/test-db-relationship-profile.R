library(episcout)
library(testthat)
library(DBI)

context("bounded PostgreSQL relationship profiling")

test_that("empty relationship selections return the exact typed contract", {
  dictionary <- relationship_test_dictionary()
  pairs <- relationship_test_pairs(character(), character())

  profile <- epi_db_relationship_profile(NULL, dictionary, pairs)

  expect_identical(class(profile), c("epi_db_relationship_profile", "list"))
  expect_identical(names(profile), c("summary", "mappings", "conflicts"))
  expect_identical(
    names(profile$summary),
    c(
      names(pairs), "total_rows", "both_present", "left_missing", "right_missing",
      "both_missing", "distinct_left", "distinct_right",
      "distinct_combinations", "max_right_per_left", "max_left_per_right",
      "left_values_with_multiple_right", "right_values_with_multiple_left",
      "relationship_class"
    )
  )
  expect_identical(
    names(profile$mappings),
    c(names(pairs), "left_value", "right_value", "n")
  )
  expect_identical(
    names(profile$conflicts),
    c(names(pairs), "exception_type", "left_value", "right_value", "n")
  )
  expect_equal(nrow(profile$summary), 0L)
  expect_equal(nrow(profile$mappings), 0L)
  expect_equal(nrow(profile$conflicts), 0L)
  expect_true(all(vapply(profile$summary[7:18], is.numeric, logical(1))))
  expect_true(is.character(profile$summary$relationship_class))
  expect_true(is.character(profile$mappings$left_value))
  expect_true(is.character(profile$conflicts$exception_type))
})

test_that("relationship selectors are validated before a connection", {
  dictionary <- relationship_test_dictionary()
  pair <- relationship_test_pairs("a", "b")

  expect_error(
    epi_db_relationship_profile(NULL, dictionary, pair[, -1, drop = FALSE]),
    "exactly left_schema"
  )
  missing <- pair
  missing$right_column <- "absent"
  expect_error(
    epi_db_relationship_profile(NULL, dictionary, missing),
    "active dictionary row"
  )
  removed <- dictionary
  removed$drift_status[removed$source_column == "b"] <- "removed"
  expect_error(
    epi_db_relationship_profile(NULL, removed, pair),
    "active dictionary row"
  )
  cross_table <- pair
  cross_table$right_table <- "other"
  expect_error(
    epi_db_relationship_profile(NULL, dictionary, cross_table),
    "same table"
  )
  self_pair <- relationship_test_pairs("a", "a")
  expect_error(
    epi_db_relationship_profile(NULL, dictionary, self_pair),
    "self-pairs"
  )
  expect_error(
    epi_db_relationship_profile(NULL, dictionary, rbind(pair, pair)),
    "must be unique"
  )
  unsupported <- dictionary
  unsupported$source_data_type[unsupported$source_column == "b"] <- "jsonb"
  expect_error(
    epi_db_relationship_profile(NULL, unsupported, pair),
    "source_data_type"
  )
  expect_error(
    epi_db_relationship_profile(NULL, dictionary, pair, max_levels = Inf),
    "positive whole"
  )
})

test_that("recorded source types use the exact scalar allow-list", {
  supported <- c(
    "boolean", "character", "character varying", "text", "smallint",
    "integer", "bigint", "numeric", "decimal", "real",
    "double precision", "date", "time without time zone",
    "time with time zone", "timestamp without time zone",
    "timestamp with time zone", "uuid"
  )
  pair <- relationship_test_pairs("a", "b")

  for (source_type in supported) {
    dictionary <- relationship_test_dictionary(
      columns = c("a", "b"),
      types = c(source_type, "text")
    )
    connection <- relationship_mock_connection(list(
      relationship_preflight_result()
    ))
    expect_silent(epi_db_relationship_profile(connection, dictionary, pair))
  }

  dictionary <- relationship_test_dictionary(
    columns = c("a", "b"),
    types = c("timestamp", "text")
  )
  expect_error(
    epi_db_relationship_profile(NULL, dictionary, pair),
    "source_data_type"
  )
})

test_that("relationship classification follows the frozen precedence", {
  dictionary <- relationship_test_dictionary()
  pairs <- relationship_test_pairs(
    c("g", "a", "f", "c", "e", "d", "b"),
    c("h", "b", "g", "d", "f", "e", "a")
  )
  preflights <- list(
    relationship_preflight_result(
      both_present = 1, distinct_left = 1, distinct_right = 1,
      distinct_combinations = 1, max_right_per_left = 1, max_left_per_right = 1
    ),
    relationship_preflight_result(
      both_present = 2, distinct_left = 1, distinct_right = 2,
      distinct_combinations = 2, max_right_per_left = 2, max_left_per_right = 1,
      left_values_with_multiple_right = 1
    ),
    relationship_preflight_result(
      both_present = 2, distinct_left = 2, distinct_right = 1,
      distinct_combinations = 2, max_right_per_left = 1, max_left_per_right = 2,
      right_values_with_multiple_left = 1
    ),
    relationship_preflight_result(
      both_present = 3, distinct_left = 2, distinct_right = 3,
      distinct_combinations = 3, max_right_per_left = 2, max_left_per_right = 1,
      left_values_with_multiple_right = 1
    ),
    relationship_preflight_result(
      both_present = 3, distinct_left = 3, distinct_right = 2,
      distinct_combinations = 3, max_right_per_left = 1, max_left_per_right = 2,
      right_values_with_multiple_left = 1
    ),
    relationship_preflight_result(
      both_present = 4, distinct_left = 2, distinct_right = 2,
      distinct_combinations = 3, max_right_per_left = 2, max_left_per_right = 2,
      left_values_with_multiple_right = 1, right_values_with_multiple_left = 1
    ),
    relationship_preflight_result(
      left_missing = 2, right_missing = 3, both_missing = 1
    )
  )
  connection <- relationship_mock_connection(preflights)

  profile <- epi_db_relationship_profile(connection, dictionary, pairs)

  expect_identical(
    paste(profile$summary$left_column, profile$summary$right_column),
    c("a b", "b a", "c d", "d e", "e f", "f g", "g h")
  )
  expect_identical(
    profile$summary$relationship_class,
    c(
      "one_to_one", "constant_left", "constant_right", "one_to_many",
      "many_to_one", "many_to_many", "insufficient_data"
    )
  )
  count_columns <- setdiff(
    names(profile$summary),
    c(names(pairs), "relationship_class")
  )
  expect_true(all(vapply(profile$summary[count_columns], is.numeric, logical(1))))
  expect_true(all(unlist(profile$summary[count_columns]) >= 0))
})

test_that("relationship details are text aggregates with byte-order sorting", {
  dictionary <- relationship_test_dictionary()
  pair <- relationship_test_pairs("a", "b")
  preflight <- relationship_preflight_result(
    both_present = 6,
    left_missing = 2,
    right_missing = 1,
    both_missing = 3,
    distinct_left = 4,
    distinct_right = 4,
    distinct_combinations = 5,
    max_right_per_left = 2,
    max_left_per_right = 2,
    left_values_with_multiple_right = 1,
    right_values_with_multiple_left = 1
  )
  mappings <- data.frame(
    left_value = c("ä", "z", "A", "z", "x"),
    right_value = c("four", "two", "one", "three", "two"),
    n = c(1L, 1L, 2L, 1L, 1L),
    stringsAsFactors = FALSE
  )
  connection <- relationship_mock_connection(
    list(preflight),
    mappings = list(mappings)
  )

  profile <- epi_db_relationship_profile(connection, dictionary, pair)

  expect_identical(profile$mappings$left_value, c("A", "x", "z", "z", "ä"))
  expect_identical(
    profile$mappings$right_value,
    c("one", "two", "three", "two", "four")
  )
  expect_identical(profile$mappings$n, c(2, 1, 1, 1, 1))
  expect_identical(
    profile$conflicts$exception_type,
    c(
      "right_maps_multiple_left", "left_maps_multiple_right",
      "left_maps_multiple_right", "right_maps_multiple_left",
      "both_missing", "left_missing", "right_missing"
    )
  )
  missing <- profile$conflicts$exception_type %in%
    c("left_missing", "right_missing", "both_missing")
  expect_true(all(is.na(profile$conflicts$left_value[missing])))
  expect_true(all(is.na(profile$conflicts$right_value[missing])))

  queries <- connection@state$queries
  expect_identical(
    vapply(
      c("preflight", "mappings"),
      function(marker) {
        sum(grepl(
          paste0("episcout_relationship_", marker, " */"),
          queries,
          fixed = TRUE
        ))
      },
      integer(1)
    ),
    c(preflight = 1L, mappings = 1L)
  )
  expect_true(all(grepl(
    '"study"."relationship source"',
    queries,
    fixed = TRUE
  )))
  expect_true(all(grepl('"a"', queries, fixed = TRUE)))
  expect_true(all(grepl('"b"', queries, fixed = TRUE)))
  expect_true(all(!grepl(
    "BEGIN|COMMIT|ROLLBACK|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER",
    queries,
    ignore.case = TRUE
  )))
})

test_that("mapping retrieval stays bounded if cardinality changes after preflight", {
  dictionary <- relationship_test_dictionary()
  pair <- relationship_test_pairs("a", "b")
  preflight <- relationship_preflight_result(
    both_present = 1,
    distinct_left = 1,
    distinct_right = 1,
    distinct_combinations = 1,
    max_right_per_left = 1,
    max_left_per_right = 1
  )
  mappings <- data.frame(
    left_value = c("1", "2"),
    right_value = c("A", "B"),
    n = c(1, 1),
    observed_combinations = c(2, 2),
    stringsAsFactors = FALSE
  )
  connection <- relationship_mock_connection(
    list(preflight),
    mappings = list(mappings)
  )

  expect_error(
    epi_db_relationship_profile(connection, dictionary, pair, max_levels = 1),
    "2 distinct non-NULL combinations exceed max_levels = 1 after preflight",
    fixed = TRUE
  )
  expect_equal(length(connection@state$queries), 2L)
  expect_match(connection@state$queries[[2]], "LIMIT 2", fixed = TRUE)
})

test_that("every pair is preflighted before an over-bound call fails", {
  dictionary <- relationship_test_dictionary()
  pairs <- relationship_test_pairs(c("c", "a"), c("d", "b"))
  connection <- relationship_mock_connection(list(
    relationship_preflight_result(
      both_present = 2, distinct_left = 2, distinct_right = 2,
      distinct_combinations = 2, max_right_per_left = 1, max_left_per_right = 1
    ),
    relationship_preflight_result(
      both_present = 4, distinct_left = 4, distinct_right = 4,
      distinct_combinations = 4, max_right_per_left = 1, max_left_per_right = 1
    )
  ))

  expect_error(
    epi_db_relationship_profile(connection, dictionary, pairs, max_levels = 3),
    "study.relationship source.c -> study.relationship source.d: 4 distinct"
  )
  expect_equal(length(connection@state$queries), 2L)
  expect_true(all(grepl(
    "episcout_relationship_preflight",
    connection@state$queries,
    fixed = TRUE
  )))
})

test_that("reversed relationship requests remain independently valid", {
  dictionary <- relationship_test_dictionary()
  pairs <- rbind(
    relationship_test_pairs("b", "a"),
    relationship_test_pairs("a", "b")
  )
  preflights <- rep(list(relationship_preflight_result()), 2L)
  connection <- relationship_mock_connection(preflights)

  profile <- epi_db_relationship_profile(connection, dictionary, pairs)

  expect_identical(profile$summary$left_column, c("a", "b"))
  expect_identical(profile$summary$right_column, c("b", "a"))
})

test_that("the relationship profiler is exported", {
  expect_true("epi_db_relationship_profile" %in% getNamespaceExports("episcout"))
})
