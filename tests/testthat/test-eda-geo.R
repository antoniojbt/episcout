context("reviewed coordinate-pair EDA")

library(testthat)
library(episcout)

geo_spec <- function(crs = "4326") {
  data.frame(
    name = c("x_coord", "y_coord", "value"),
    label = c("Reviewed x", "Reviewed y", "Value"),
    type = c("numeric", "numeric", "numeric"),
    role = c("", "", ""),
    missing_codes = c("-999", "-999", ""),
    geo_role = c("x", "y", ""),
    geo_pair = c("reviewed_pair", "reviewed_pair", ""),
    geo_crs = c(crs, crs, ""),
    stringsAsFactors = FALSE
  )
}

test_that("coordinate metadata validates exact reviewed pairs", {
  expect_identical(names(formals(epi_eda_profile_geo)), c("data", "spec"))
  observed <- epi_eda_spec(geo_spec())
  expect_identical(observed$geo_role, c("x", "y", ""))
  expect_identical(observed$geo_pair, c("reviewed_pair", "reviewed_pair", ""))

  legacy <- geo_spec()[, c("name", "label", "type", "role", "missing_codes")]
  expect_false(any(c("geo_role", "geo_pair", "geo_crs") %in% names(epi_eda_spec(legacy))))
  empty <- epi_eda_profile_geo(data.frame(
    x_coord = 1, y_coord = 2, value = 3
  ), legacy)
  expect_identical(nrow(empty), 0L)
  expect_identical(
    vapply(empty, typeof, character(1)),
    c(
      geo_pair = "character", x_name = "character", y_name = "character",
      geo_crs = "character", crs_epsg = "integer", n = "integer",
      complete_pairs = "integer", missing_x = "integer",
      missing_y = "integer", both_missing = "integer",
      non_finite = "integer", range_failures = "integer",
      eligible = "logical", status = "character", reason = "character"
    )
  )

  partial <- geo_spec()
  partial$geo_crs <- NULL
  expect_error(epi_eda_spec(partial), "requires geo_role, geo_pair and geo_crs")

  bad_role <- geo_spec()
  bad_role$geo_role[[1]] <- "longitude"
  expect_error(epi_eda_spec(bad_role), "blank, x or y")

  orphan <- geo_spec()
  orphan$geo_pair[[3]] <- "orphan"
  expect_error(epi_eda_spec(orphan), "blank or complete")

  duplicate <- geo_spec()
  duplicate$geo_role[[2]] <- "x"
  expect_error(epi_eda_spec(duplicate), "exactly one x row and one y row")

  mismatched <- geo_spec()
  mismatched$geo_crs[[2]] <- "3857"
  expect_error(epi_eda_spec(mismatched), "same explicit geo_crs")

  text_pair <- geo_spec()
  text_pair$type[[1]] <- "text"
  expect_error(epi_eda_spec(text_pair), "numeric or integer")

  unresolved <- geo_spec("not-a-crs")
  expect_error(epi_eda_spec(unresolved), "could not be resolved")

  normalized <- geo_spec()
  normalized$geo_role <- c(" X ", " Y ", "")
  normalized$geo_pair <- c(" reviewed_pair ", " reviewed_pair ", "")
  normalized$geo_crs <- c(" 4326 ", " 4326 ", "")
  observed <- epi_eda_spec(normalized)
  expect_identical(observed$geo_role, c("x", "y", ""))
  expect_identical(observed$geo_pair, c("reviewed_pair", "reviewed_pair", ""))
  expect_identical(observed$geo_crs, c("4326", "4326", ""))

  path <- tempfile(fileext = ".csv")
  utils::write.csv(observed, path, row.names = FALSE, na = "")
  round_trip <- epi_eda_spec(path)
  expect_identical(round_trip$geo_role, observed$geo_role)
  expect_identical(round_trip$geo_pair, observed$geo_pair)
  expect_identical(round_trip$geo_crs, observed$geo_crs)
})

test_that("scaffolds reserve blank coordinate metadata without inference", {
  data <- data.frame(
    longitude = c(-99.1, -99.2),
    latitude = c(19.4, 19.5),
    ordinary = 1:2
  )
  observed <- epi_eda_spec_scaffold(data)

  expect_true(all(c("geo_role", "geo_pair", "geo_crs") %in% names(observed)))
  expect_true(all(observed$geo_role == ""))
  expect_true(all(observed$geo_pair == ""))
  expect_true(all(observed$geo_crs == ""))

  empty <- epi_eda_spec_scaffold(data.frame())
  expect_type(empty$geo_role, "character")
  expect_type(empty$geo_pair, "character")
  expect_type(empty$geo_crs, "character")
})

test_that("data-frame coordinate QA returns independent aggregate counts", {
  data <- data.frame(
    x_coord = c(-180, 180, 181, NA, 4, NA, NaN, Inf, -999),
    y_coord = c(-90, 90, 0, 2, NA, NA, 3, 4, 5),
    value = seq_len(9)
  )
  original <- data
  observed <- epi_eda_profile_geo(data, geo_spec())

  expect_identical(names(observed), c(
    "geo_pair", "x_name", "y_name", "geo_crs", "crs_epsg", "n",
    "complete_pairs", "missing_x", "missing_y", "both_missing",
    "non_finite", "range_failures", "eligible", "status", "reason"
  ))
  expect_identical(observed$geo_pair, "reviewed_pair")
  expect_identical(observed$x_name, "x_coord")
  expect_identical(observed$y_name, "y_coord")
  expect_identical(observed$crs_epsg, 4326L)
  expect_identical(observed$n, 9L)
  expect_identical(observed$complete_pairs, 3L)
  expect_identical(observed$missing_x, 2L)
  expect_identical(observed$missing_y, 1L)
  expect_identical(observed$both_missing, 1L)
  expect_identical(observed$non_finite, 2L)
  expect_identical(observed$range_failures, 1L)
  expect_false(observed$eligible)
  expect_identical(observed$status, "not_eligible")
  expect_identical(
    observed$reason,
    "incomplete_pairs;non_finite_coordinates;reviewed_crs_range_failure"
  )
  expect_identical(data, original)
})

test_that("eligibility, zero rows and non-geographic ranges are stable", {
  eligible <- epi_eda_profile_geo(
    data.frame(x_coord = c(-180, 180), y_coord = c(-90, 90), value = 1:2),
    geo_spec()
  )
  expect_true(eligible$eligible)
  expect_identical(eligible$status, "eligible")
  expect_identical(eligible$reason, "all_rows_eligible")

  zero <- epi_eda_profile_geo(
    data.frame(x_coord = numeric(), y_coord = numeric(), value = numeric()),
    geo_spec()
  )
  expect_identical(zero$n, 0L)
  expect_false(zero$eligible)
  expect_identical(zero$reason, "no_rows")

  projected <- epi_eda_profile_geo(
    data.frame(x_coord = 1e9, y_coord = -1e9, value = 1),
    geo_spec("3857")
  )
  expect_identical(projected$range_failures, 0L)
  expect_true(projected$eligible)
})

test_that("multiple reviewed pairs retain specification order", {
  spec <- rbind(
    geo_spec()[1:2, ],
    transform(
      geo_spec()[1:2, ],
      name = c("second_x", "second_y"),
      label = c("Second x", "Second y"),
      geo_pair = "second_pair",
      geo_crs = "3857"
    )
  )
  data <- data.frame(
    x_coord = 1, y_coord = 2, second_x = 3, second_y = 4
  )
  observed <- epi_eda_profile_geo(data, spec)

  expect_identical(observed$geo_pair, c("reviewed_pair", "second_pair"))
  expect_true(all(observed$eligible))
  expect_silent(getFromNamespace("eda_geo_reconcile", "episcout")(observed, 1L))

  bad_partition <- observed
  bad_partition$missing_x[[1]] <- 2L
  expect_error(
    getFromNamespace("eda_geo_reconcile", "episcout")(bad_partition, 1L),
    "did not reconcile"
  )
  bad_status <- observed
  bad_status$eligible[[1]] <- FALSE
  expect_error(
    getFromNamespace("eda_geo_reconcile", "episcout")(bad_status, 1L),
    "did not reconcile"
  )
})

test_that("coordinate profiling fails without reproducing values", {
  private <- "987654.321098"
  missing <- geo_spec()
  expect_error(
    epi_eda_profile_geo(data.frame(x_coord = as.numeric(private), value = 1), missing),
    "missing from the data"
  )
  incompatible <- data.frame(
    x_coord = private, y_coord = "123456.789012", value = 1,
    stringsAsFactors = FALSE
  )
  condition <- tryCatch(
    epi_eda_profile_geo(incompatible, geo_spec()),
    error = identity
  )
  expect_s3_class(condition, "error")
  expect_false(grepl(private, conditionMessage(condition), fixed = TRUE))
})

test_that("ordinary workflows publish aggregate geo QA without coordinates", {
  private_x <- 12.34567890123
  private_y <- 45.67890123456
  data <- data.frame(x_coord = private_x, y_coord = private_y, value = 7)
  spec <- geo_spec()

  simple_dir <- tempfile("eda-geo-run-")
  dir.create(simple_dir)
  simple <- epi_eda_run(data, spec, output_dir = simple_dir)
  expect_identical(simple$geo$status, "eligible")
  expect_identical(simple$summaries$variables$status[1:2], c("skipped", "skipped"))
  expect_null(simple$plots$x_coord)
  expect_null(simple$plots$y_coord)
  expect_true(file.exists(file.path(simple_dir, "geo_qa.csv")))

  intake_dir <- tempfile("eda-geo-intake-")
  intake <- epi_eda_intake_run(
    data, spec, intake_dir, prepare = "none", render = TRUE
  )
  expect_identical(intake$status, "complete")
  expect_identical(intake$geo$status, "eligible")
  expect_identical(
    intake$manifest$status[intake$manifest$artifact == "geo_qa"],
    "created"
  )

  forbidden <- c(
    format(private_x, digits = 14), format(private_y, digits = 14),
    "POINT (", "POLYGON (("
  )
  returned <- rawToChar(serialize(list(simple = simple, intake = intake), NULL, ascii = TRUE))
  files <- c(
    list.files(simple_dir, full.names = TRUE),
    list.files(intake_dir, full.names = TRUE)
  )
  bytes <- paste(vapply(files, function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  expect_false(any(vapply(forbidden, grepl, logical(1), x = returned, fixed = TRUE)))
  expect_false(any(vapply(forbidden, grepl, logical(1), x = bytes, fixed = TRUE)))
})
