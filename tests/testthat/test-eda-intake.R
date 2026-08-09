context("EDA intake workflow")

library(testthat)
library(episcout)

make_intake_fixture <- function() {
  data <- data.frame(
    arm = factor(c("A", "A", "B", "B"), levels = c("A", "B", "C")),
    value = c(1, 2, NA_real_, 4),
    status = factor(c("yes", "no", "yes", NA), levels = c("no", "yes", "unused")),
    note = c("PRIVATE_TEXT_A", "PRIVATE_TEXT_B", "PRIVATE_TEXT_C", NA),
    participant_id = c(900001L, 900002L, 900003L, 900004L),
    stringsAsFactors = FALSE
  )
  spec <- epi_eda_spec_scaffold(data)
  spec$role <- c("exposure", "measure", "measure", "measure", "identifier")
  list(data = data, spec = spec)
}

bundle_files <- function(path) {
  sort(list.files(path, recursive = TRUE, include.dirs = FALSE))
}

test_that("public intake and return contracts include map components", {
  expect_named(
    formals(epi_eda_intake_run),
    c(
      "data", "spec", "output_dir", "prepare", "strata", "render",
      "overwrite", "source_id", "maps", "map_vars", "max_map_points"
    )
  )
  fixture <- make_intake_fixture()
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, tempfile("intake-contract-"), render = FALSE
  )

  expect_s3_class(observed, "epi_eda_intake")
  expect_named(observed, c(
    "status", "stage", "output_dir", "manifest", "input", "spec",
    "schema_before", "schema_after", "preparation_audit", "missing", "geo",
    "maps", "map_inventory", "summary", "stratified", "table1", "report",
    "messages", "metadata"
  ))
  expect_identical(observed$status, "complete")
  expect_identical(observed$spec$state, "supplied")
  expect_named(
    observed$manifest,
    c("artifact", "type", "path", "status", "checksum_md5")
  )
  expect_length(observed$maps, 0L)
  expect_equal(nrow(observed$map_inventory), 0L)
})

test_that("spec NULL generates, saves, returns and uses a semantic dictionary", {
  fixture <- make_intake_fixture()
  original <- fixture$data
  output_dir <- tempfile("intake-generated-")
  observed <- epi_eda_intake_run(
    fixture$data, output_dir = output_dir, render = TRUE
  )

  expect_identical(observed$status, "complete")
  expect_identical(observed$stage, "canonical_summary")
  expect_identical(observed$spec$state, "generated")
  expect_identical(observed$spec$source, "generated")
  expect_identical(observed$spec$data, epi_eda_spec_scaffold(fixture$data))
  expect_true(file.exists(file.path(output_dir, "specification.csv")))
  expect_false(file.exists(file.path(output_dir, "review_guide.md")))
  expect_true(file.exists(file.path(output_dir, "summary_variables.csv")))
  expect_true(file.exists(file.path(output_dir, "report.html")))
  expect_identical(fixture$data, original)
  html <- paste(readLines(file.path(output_dir, "report.html")), collapse = "\n")
  expect_match(html, "Analysis completed")
  expect_match(
    html,
    "episcout creates the outputs explicitly requested by the analyst",
    fixed = TRUE
  )
  expect_false(grepl("review_required|caller_asserted", html))
})

test_that("removed and malformed specifications return actionable blocked bundles", {
  fixture <- make_intake_fixture()
  removed <- fixture$spec
  removed$review_status <- "reviewed"
  output_dir <- tempfile("intake-old-spec-")
  old <- epi_eda_intake_run(
    fixture$data, removed, output_dir, render = FALSE
  )
  expect_identical(old$status, "blocked")
  expect_match(old$messages$reason, "removed evidence/review scaffold")
  expect_false(file.exists(file.path(output_dir, "specification.csv")))

  malformed <- fixture$spec[, setdiff(names(fixture$spec), "type"), drop = FALSE]
  invalid <- epi_eda_intake_run(
    fixture$data, malformed, tempfile("intake-invalid-spec-"), render = FALSE
  )
  expect_identical(invalid$status, "blocked")
  expect_match(invalid$messages$reason, "missing required columns")
})

test_that("audit, blockers and apply retain factual processing outcomes", {
  fixture <- make_intake_fixture()
  audit <- epi_eda_intake_run(
    fixture$data, fixture$spec, tempfile("intake-audit-"),
    prepare = "audit", render = FALSE
  )
  expect_identical(audit$status, "audit_complete")
  expect_identical(audit$stage, "audit")
  expect_null(audit$summary)

  absent <- fixture$spec[1, , drop = FALSE]
  absent$name <- "required_absent"
  absent$label <- "Required absent"
  absent$required <- TRUE
  blocked <- epi_eda_intake_run(
    fixture$data, rbind(fixture$spec, absent), tempfile("intake-blocked-"),
    prepare = "apply", render = FALSE
  )
  expect_identical(blocked$status, "blocked")
  expect_true(any(blocked$preparation_audit$status == "blocking"))

  changed <- fixture
  changed$data$value <- c(1L, 2L, NA_integer_, 4L)
  changed$spec$type[changed$spec$name == "value"] <- "numeric"
  none <- epi_eda_intake_run(
    changed$data, changed$spec, tempfile("intake-none-"), render = FALSE
  )
  expect_identical(none$status, "blocked")
  applied <- epi_eda_intake_run(
    changed$data, changed$spec, tempfile("intake-apply-"),
    prepare = "apply", render = FALSE
  )
  expect_identical(applied$status, "complete")
  expect_s3_class(applied$schema_after, "data.frame")
})

test_that("roles describe variables without suppressing summaries or strata", {
  fixture <- make_intake_fixture()
  identifier <- fixture$spec$name == "participant_id"
  fixture$spec$type[identifier] <- "categorical"
  fixture$spec$levels[identifier] <- "900001;900002;900003;900004"
  observed <- epi_eda_intake_run(
    fixture$data,
    fixture$spec,
    tempfile("intake-identifier-strata-"),
    strata = "participant_id",
    prepare = "apply",
    render = FALSE
  )

  expect_identical(observed$status, "complete")
  expect_identical(
    observed$summary$variables$status[identifier],
    "summarised"
  )
  expect_s3_class(observed$stratified, "epi_eda_stratified")
  expect_true("participant_id" %in% observed$summary$categorical$name)
})

test_that("manifest and checksums cover every regular bundle artifact", {
  fixture <- make_intake_fixture()
  output_dir <- tempfile("intake-manifest-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir, strata = "arm"
  )
  created <- sort(observed$manifest$path[observed$manifest$status == "created"])

  expect_identical(created, bundle_files(output_dir))
  expect_true(all(!grepl("^/|[.][.]", observed$manifest$path)))
  checked <- observed$manifest$status == "created" &
    observed$manifest$artifact != "manifest"
  expect_identical(
    observed$manifest$checksum_md5[checked],
    unname(tools::md5sum(file.path(output_dir, observed$manifest$path[checked])))
  )
})

test_that("owned overwrite is conservative, transactional and rejects old manifests", {
  fixture <- make_intake_fixture()
  owned <- tempfile("intake-owned-")
  first <- epi_eda_intake_run(
    fixture$data, fixture$spec, owned, render = FALSE
  )
  second <- epi_eda_intake_run(
    fixture$data, fixture$spec, owned, overwrite = TRUE, render = FALSE
  )
  expect_identical(second$status, "complete")
  before_files <- bundle_files(owned)
  before_checksums <- unname(tools::md5sum(file.path(owned, before_files)))

  expect_error(
    with_mocked_bindings(
      epi_eda_intake_run(
        fixture$data, fixture$spec, owned,
        overwrite = TRUE, render = FALSE
      ),
      intake_write_csv = function(...) stop("simulated staging failure"),
      .package = "episcout"
    ),
    "simulated staging failure"
  )
  expect_identical(bundle_files(owned), before_files)
  expect_identical(
    unname(tools::md5sum(file.path(owned, before_files))),
    before_checksums
  )

  manifest_path <- file.path(owned, "manifest.csv")
  manifest <- utils::read.csv(
    manifest_path, stringsAsFactors = FALSE, na.strings = character()
  )
  manifest$sensitivity <- "internal_review"
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, owned,
      overwrite = TRUE, render = FALSE
    ),
    "removed sensitivity schema"
  )
  expect_identical(first$status, "complete")
})

test_that("unowned output and report failures preserve safe artifacts", {
  fixture <- make_intake_fixture()
  unowned <- tempfile("intake-unowned-")
  dir.create(unowned)
  writeLines("preserve", file.path(unowned, "user.txt"))
  expect_error(
    epi_eda_intake_run(fixture$data, fixture$spec, unowned),
    "non-empty"
  )
  expect_identical(readLines(file.path(unowned, "user.txt")), "preserve")

  output_dir <- tempfile("intake-report-failure-")
  local_mocked_bindings(
    intake_render_report = function(...) stop("simulated render failure"),
    .package = "episcout"
  )
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir, render = TRUE
  )
  expect_identical(observed$status, "blocked")
  expect_true(file.exists(file.path(output_dir, "summary_variables.csv")))
  expect_false(file.exists(file.path(output_dir, "report.html")))
})

test_that("intake bundles maps, checksums and portable HTML", {
  data <- data.frame(
    lon = c(-1, 0, 1),
    lat = c(-1, 0, 1),
    theme = c("A", "MISSING", "B"),
    stringsAsFactors = FALSE
  )
  spec <- epi_eda_spec_scaffold(data)
  spec$geo_role <- c("x", "y", "")
  spec$geo_pair <- c("site", "site", "")
  spec$geo_crs <- c("4326", "4326", "")
  spec$missing_codes[[3]] <- "MISSING"
  output_dir <- tempfile("intake-maps-")
  observed <- epi_eda_intake_run(
    data, spec, output_dir, prepare = "apply", render = TRUE,
    maps = TRUE, map_vars = "theme"
  )

  expect_identical(observed$status, "complete")
  expect_identical(
    observed$map_inventory$map_id,
    c("map-p001-geometry", "map-p001-v003")
  )
  expect_true(all(file.exists(file.path(output_dir, observed$map_inventory$path))))
  expect_true(all(observed$manifest$type[observed$manifest$type == "map"] == "map"))
  html <- paste(readLines(file.path(output_dir, "report.html")), collapse = "\n")
  expect_match(html, "maps/map-p001-geometry.svg", fixed = TRUE)
  expect_match(html, "Map inventory", fixed = TRUE)
  expect_false(grepl(output_dir, html, fixed = TRUE))
})

test_that("argument validation happens before publication", {
  fixture <- make_intake_fixture()
  expect_error(epi_eda_intake_run(list(), output_dir = tempfile()), "data frame")
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, tempfile(), map_vars = "value"
    ),
    "requires maps = TRUE"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, tempfile(), source_id = "/private/source.csv"
    ),
    "absolute"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, "https://example.test/spec.csv", tempfile()
    ),
    "network URLs"
  )
})

test_that("zero-row inputs retain summary and map inventory schemas", {
  fixture <- make_intake_fixture()
  data <- fixture$data[0, , drop = FALSE]
  spec <- epi_eda_spec_scaffold(data)
  observed <- epi_eda_intake_run(
    data, spec, tempfile("intake-zero-row-"), maps = TRUE, render = FALSE
  )

  expect_identical(observed$status, "complete")
  expect_true(all(observed$summary$variables$n == 0L))
  expect_equal(nrow(observed$map_inventory), 0L)
})
