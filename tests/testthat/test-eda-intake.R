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

intake_internal <- function(name) {
  getFromNamespace(name, "episcout")
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
    fixture$data, fixture$spec, tempfile("intake-contract-"),
    render = FALSE
  )

  expect_s3_class(observed, "epi_eda_intake")
  expect_named(observed, c(
    "status", "stage", "output_dir", "manifest", "input", "spec",
    "schema_before", "schema_after", "preparation_audit", "missing", "geo",
    "maps", "map_inventory", "summary", "categorical_display", "stratified", "table1", "report",
    "messages", "metadata"
  ))
  expect_identical(observed$status, "complete")
  expect_s3_class(observed$categorical_display, "data.frame")
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
    fixture$data,
    output_dir = output_dir, render = TRUE
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
    fixture$data, removed, output_dir,
    render = FALSE
  )
  expect_identical(old$status, "blocked")
  expect_match(old$messages$reason, "removed evidence/review scaffold")
  expect_false(file.exists(file.path(output_dir, "specification.csv")))

  malformed <- fixture$spec[, setdiff(names(fixture$spec), "type"), drop = FALSE]
  invalid <- epi_eda_intake_run(
    fixture$data, malformed, tempfile("intake-invalid-spec-"),
    render = FALSE
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
    changed$data, changed$spec, tempfile("intake-none-"),
    render = FALSE
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
    fixture$data, fixture$spec, output_dir,
    strata = "arm"
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
    fixture$data, fixture$spec, owned,
    render = FALSE
  )
  second <- epi_eda_intake_run(
    fixture$data, fixture$spec, owned,
    overwrite = TRUE, render = FALSE
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
    manifest_path,
    stringsAsFactors = FALSE, na.strings = character()
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
    fixture$data, fixture$spec, output_dir,
    render = TRUE
  )
  expect_identical(observed$status, "blocked")
  expect_true(file.exists(file.path(output_dir, "summary_variables.csv")))
  expect_false(file.exists(file.path(output_dir, "report.html")))
})

test_that("canonical reconciliation fails closed for corrupted artifacts", {
  fixture <- make_intake_fixture()
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, tempfile("intake-reconcile-canonical-"),
    strata = "arm", render = FALSE
  )
  reconcile <- intake_internal("intake_reconcile_canonical")
  check <- function(summary = observed$summary, missing = observed$missing) {
    reconcile(summary, missing, fixture$data, fixture$spec)
  }

  changed <- observed$summary
  names(changed)[[1]] <- "wrong"
  expect_match(check(changed), "six-component")

  changed <- observed$summary
  changed$variables$name[[1]] <- "wrong"
  expect_match(check(changed), "variable membership")

  changed <- observed$summary
  changed$numeric <- changed$numeric[0, , drop = FALSE]
  expect_match(check(changed), "component membership")

  changed <- observed$summary
  changed$categorical <- rbind(changed$categorical, changed$categorical[1, ])
  expect_match(check(changed), "component membership")

  changed <- observed$summary
  changed$variables$n[[1]] <- changed$variables$n[[1]] + 1L
  expect_match(check(changed), "row counts")

  changed <- observed$summary
  changed$variables$n_missing[[1]] <- changed$variables$n_missing[[1]] + 1L
  expect_match(check(changed), "missing and observed")

  missing <- observed$missing
  missing$n_missing[[1]] <- missing$n_missing[[1]] + 1L
  expect_match(check(missing = missing), "missingness")

  changed <- observed$summary
  changed$categorical$n[[1]] <- changed$categorical$n[[1]] + 1L
  expect_match(check(changed), "categorical counts")

  changed <- observed$summary
  changed$numeric$n_finite[[1]] <- changed$numeric$n_finite[[1]] + 1L
  expect_match(check(changed), "finite and infinite")
})

test_that("stratified reconciliation fails closed for corrupted artifacts", {
  fixture <- make_intake_fixture()
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, tempfile("intake-reconcile-stratified-"),
    strata = "arm", render = FALSE
  )
  reconcile <- intake_internal("intake_reconcile_stratified")
  reconcile_groups <- intake_internal("intake_reconcile_groups")
  check <- function(stratified) {
    reconcile(observed$summary, stratified, fixture$data)
  }

  changed <- observed$stratified
  changed$metadata$n_input[[1]] <- changed$metadata$n_input[[1]] + 1L
  expect_match(check(changed), "input and included")

  changed <- observed$stratified
  overall <- which(changed$groups$is_overall)[[1]]
  changed$groups$n[[overall]] <- changed$groups$n[[overall]] + 1L
  expect_match(check(changed), "Overall group")

  changed <- observed$stratified
  grouped <- which(!changed$groups$is_overall)[[1]]
  changed$groups$n[[grouped]] <- changed$groups$n[[grouped]] + 1L
  expect_match(check(changed), "group counts")

  changed <- observed$stratified
  changed$variables$n[[1]] <- changed$variables$n[[1]] + 1L
  expect_match(check(changed), "variable counts")

  changed <- observed$stratified
  changed$numeric <- changed$numeric[-1, , drop = FALSE]
  expect_match(reconcile_groups(changed), "component membership")

  changed <- observed$stratified
  changed$numeric$n[[1]] <- changed$numeric$n[[1]] + 1L
  expect_match(reconcile_groups(changed), "denominators")

  changed <- observed$stratified
  changed$categorical <- rbind(changed$categorical, changed$categorical[1, ])
  expect_match(reconcile_groups(changed), "categorical component membership")

  changed <- observed$stratified
  changed$categorical$n_total[[1]] <- changed$categorical$n_total[[1]] + 1L
  expect_match(reconcile_groups(changed), "categorical denominators")

  changed <- observed$stratified
  ordinary <- which(!changed$categorical$is_missing_level)[[1]]
  changed$categorical$n[[ordinary]] <-
    changed$categorical$n[[ordinary]] + 1L
  expect_match(reconcile_groups(changed), "counts or proportions")

  changed <- observed$stratified
  changed$variables$label[[1]] <- "corrupted"
  expect_match(check(changed), "variable summaries")

  changed <- observed$stratified
  overall_numeric <- which(changed$numeric$is_overall)[[1]]
  changed$numeric$mean[[overall_numeric]] <- 999
  expect_match(check(changed), "numeric summaries")

  changed <- observed$stratified
  overall_categorical <- which(
    changed$categorical$is_overall &
      !changed$categorical$is_missing_level
  )[[1]]
  changed$categorical$is_declared[[overall_categorical]] <-
    !changed$categorical$is_declared[[overall_categorical]]
  expect_match(check(changed), "categorical summaries")
})

test_that("overwrite rejects corrupted ownership and restores failed swaps", {
  fixture <- make_intake_fixture()
  malformed <- tempfile("intake-malformed-manifest-")
  epi_eda_intake_run(
    fixture$data, fixture$spec, malformed,
    render = FALSE
  )
  manifest_path <- file.path(malformed, "manifest.csv")
  manifest <- utils::read.csv(
    manifest_path,
    stringsAsFactors = FALSE, na.strings = character()
  )
  manifest$type[manifest$artifact == "summary_numeric"] <- "untrusted"
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, malformed,
      overwrite = TRUE, render = FALSE
    ),
    "valid prior"
  )

  owned <- tempfile("intake-failed-swap-")
  epi_eda_intake_run(
    fixture$data, fixture$spec, owned,
    render = FALSE
  )
  before_files <- bundle_files(owned)
  before_checksums <- unname(tools::md5sum(file.path(owned, before_files)))
  rename_count <- 0L
  expect_error(
    with_mocked_bindings(
      epi_eda_intake_run(
        fixture$data, fixture$spec, owned,
        overwrite = TRUE, render = FALSE
      ),
      intake_rename = function(from, to) {
        rename_count <<- rename_count + 1L
        if (rename_count == 2L) {
          return(FALSE)
        }
        base::file.rename(from, to)
      },
      .package = "episcout"
    ),
    "prior bundle was restored"
  )
  expect_identical(rename_count, 3L)
  expect_identical(bundle_files(owned), before_files)
  expect_identical(
    unname(tools::md5sum(file.path(owned, before_files))),
    before_checksums
  )
})

test_that("rendered intake HTML escapes metadata and remains portable", {
  fixture <- make_intake_fixture()
  fixture$spec$label[fixture$spec$name == "value"] <-
    "<script>alert('x')</script>"
  output_dir <- tempfile("intake-portable-report-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir,
    strata = "arm", render = TRUE
  )
  html <- paste(readLines(file.path(output_dir, "report.html")), collapse = "\n")

  expect_identical(observed$status, "complete")
  expect_false(grepl("<script>alert", html, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;alert", html, fixed = TRUE))
  expect_false(grepl(output_dir, html, fixed = TRUE))

  moved <- paste0(output_dir, "-moved")
  expect_true(file.rename(output_dir, moved))
  created <- observed$manifest$path[observed$manifest$status == "created"]
  expect_true(all(file.exists(file.path(moved, created))))
  moved_html <- paste(readLines(file.path(moved, "report.html")), collapse = "\n")
  expect_true(all(vapply(created, function(path) {
    grepl(paste0("href=\"", path, "\""), moved_html, fixed = TRUE) ||
      identical(path, "report.html")
  }, logical(1))))
})

test_that("factual stage failures publish blocked diagnostic bundles", {
  fixture <- make_intake_fixture()

  scaffold <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data,
      output_dir = tempfile("intake-scaffold-failure-"),
      render = FALSE
    ),
    epi_eda_spec_scaffold = function(...) stop("simulated scaffold failure"),
    .package = "episcout"
  )
  expect_identical(scaffold$status, "blocked")
  expect_match(scaffold$messages$reason, "simulated scaffold failure")

  audit <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-audit-failure-"),
      render = FALSE
    ),
    epi_eda_prepare = function(...) stop("simulated audit failure"),
    .package = "episcout"
  )
  expect_identical(audit$status, "blocked")
  expect_match(audit$messages$reason, "simulated audit failure")

  geo <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-geo-failure-"),
      render = FALSE
    ),
    epi_eda_profile_geo = function(...) stop("simulated geo failure"),
    .package = "episcout"
  )
  expect_identical(geo$status, "blocked")
  expect_match(geo$messages$reason, "simulated geo failure")

  summary <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-summary-failure-"),
      render = FALSE
    ),
    epi_eda_profile_summaries = function(...) {
      stop("simulated summary failure")
    },
    .package = "episcout"
  )
  expect_identical(summary$status, "blocked")
  expect_match(summary$messages$reason, "simulated summary failure")

  canonical <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-canonical-failure-"),
      render = FALSE
    ),
    intake_reconcile_canonical = function(...) {
      "simulated canonical reconciliation failure"
    },
    .package = "episcout"
  )
  expect_identical(canonical$status, "blocked")
  expect_match(canonical$messages$reason, "simulated canonical")

  categorical_display <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-categorical-display-failure-"),
      render = FALSE
    ),
    epi_eda_categorical_display = function(...) {
      stop("simulated categorical display failure")
    },
    .package = "episcout"
  )
  expect_identical(categorical_display$status, "blocked")
  expect_match(
    categorical_display$messages$reason,
    "simulated categorical display failure"
  )
  expect_null(categorical_display$categorical_display)

  invalid_strata <- epi_eda_intake_run(
    fixture$data, fixture$spec,
    tempfile("intake-invalid-strata-"),
    strata = "value", render = FALSE
  )
  expect_identical(invalid_strata$status, "blocked")
  expect_match(invalid_strata$messages$reason, "categorical or binary")

  stratified <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-stratified-failure-"),
      strata = "arm", render = FALSE
    ),
    epi_eda_profile_stratified = function(...) {
      stop("simulated stratified failure")
    },
    .package = "episcout"
  )
  expect_identical(stratified$status, "blocked")
  expect_match(stratified$messages$reason, "simulated stratified failure")

  stratified_reconciliation <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-stratified-reconcile-failure-"),
      strata = "arm", render = FALSE
    ),
    intake_reconcile_stratified = function(...) {
      "simulated stratified reconciliation failure"
    },
    .package = "episcout"
  )
  expect_identical(stratified_reconciliation$status, "blocked")
  expect_match(
    stratified_reconciliation$messages$reason,
    "simulated stratified reconciliation"
  )

  display_calls <- 0L
  actual_display <- epi_eda_categorical_display
  stratified_display <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-stratified-display-failure-"),
      strata = "arm", render = FALSE
    ),
    epi_eda_categorical_display = function(result, ...) {
      display_calls <<- display_calls + 1L
      if (display_calls == 2L) {
        stop("simulated stratified display failure")
      }
      actual_display(result, ...)
    },
    .package = "episcout"
  )
  expect_identical(stratified_display$status, "blocked")
  expect_s3_class(stratified_display$stratified, "epi_eda_stratified")
  expect_match(
    stratified_display$messages$reason,
    "simulated stratified display failure"
  )

  table1 <- with_mocked_bindings(
    epi_eda_intake_run(
      fixture$data, fixture$spec,
      tempfile("intake-table1-failure-"),
      strata = "arm", render = FALSE
    ),
    epi_eda_table1 = function(...) stop("simulated Table 1 failure"),
    .package = "episcout"
  )
  expect_identical(table1$status, "blocked")
  expect_s3_class(table1$stratified, "epi_eda_stratified")
  expect_null(table1$table1)
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
    data, spec, output_dir,
    prepare = "apply", render = TRUE,
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

  duplicate <- fixture$data
  names(duplicate)[2] <- names(duplicate)[1]
  expect_error(
    epi_eda_intake_run(duplicate, output_dir = tempfile()),
    "Duplicate"
  )
  blank <- fixture$data
  names(blank)[[1]] <- ""
  expect_error(
    epi_eda_intake_run(blank, output_dir = tempfile()),
    "non-empty"
  )
  reserved <- fixture$data
  names(reserved)[[1]] <- ".dataset.private"
  expect_error(
    epi_eda_intake_run(reserved, output_dir = tempfile()),
    "reserved"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, tempfile(),
      render = NA
    ),
    "render"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, tempfile(),
      strata = c("arm", "status")
    ),
    "strata"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, tempfile(),
      map_vars = "value"
    ),
    "requires maps = TRUE"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, tempfile(),
      source_id = "/private/source.csv"
    ),
    "absolute"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, tempfile(),
      source_id = "\\\\server\\share\\source.csv"
    ),
    "absolute"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, tempfile(),
      source_id = "\n"
    ),
    "source_id"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, "https://example.test/spec.csv", tempfile()
    ),
    "network URLs"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data, tempfile("missing-spec-", fileext = ".csv"), tempfile()
    ),
    "must exist"
  )

  output_file <- tempfile("intake-output-file-")
  writeLines("not a directory", output_file)
  expect_error(
    epi_eda_intake_run(fixture$data, fixture$spec, output_file),
    "not a directory"
  )

  target <- tempfile("intake-link-target-")
  dir.create(target)
  link <- tempfile("intake-link-")
  linked <- file.symlink(target, link)
  if (linked) {
    expect_error(
      epi_eda_intake_run(fixture$data, fixture$spec, link),
      "symbolic link"
    )
    expect_length(list.files(target), 0L)
  }
})

test_that("zero-row inputs retain summary and map inventory schemas", {
  fixture <- make_intake_fixture()
  data <- fixture$data[0, , drop = FALSE]
  spec <- epi_eda_spec_scaffold(data)
  observed <- epi_eda_intake_run(
    data, spec, tempfile("intake-zero-row-"),
    maps = TRUE, render = FALSE
  )

  expect_identical(observed$status, "complete")
  expect_true(all(observed$summary$variables$n == 0L))
  expect_equal(nrow(observed$map_inventory), 0L)
})
