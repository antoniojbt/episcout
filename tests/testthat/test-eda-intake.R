context("stage-gated EDA intake workflow")

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
  spec$review_status <- "reviewed"
  list(data = data, spec = spec)
}

bundle_text <- function(path) {
  files <- list.files(path, full.names = TRUE)
  paste(vapply(files, function(file) {
    paste(readLines(file, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
}

test_that("public intake interface and return contract are fixed", {
  expect_named(
    formals(epi_eda_intake_run),
    c("data", "spec", "output_dir", "prepare", "strata", "render", "overwrite", "source_id")
  )
  fixture <- make_intake_fixture()
  output_dir <- tempfile("intake-contract-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir, render = FALSE
  )

  expect_s3_class(observed, "epi_eda_intake")
  expect_named(observed, c(
    "status", "stage", "output_dir", "manifest", "input", "spec",
    "schema_before", "schema_after", "preparation_audit", "missing", "summary",
    "stratified", "table1", "report", "messages", "metadata"
  ))
  expect_identical(observed$status, "complete")
  expect_identical(observed$stage, "canonical_summary")
  expect_named(observed$summary, c(
    "variables", "numeric", "categorical", "text", "temporal", "skipped"
  ))
  expect_named(observed$manifest, c(
    "artifact", "type", "path", "status", "sensitivity", "checksum_md5"
  ))
  expect_named(observed$messages, c(
    "stage", "severity", "subject", "reason", "recommended_action"
  ))
  expect_false(observed$report$requested)
  expect_false(observed$report$created)
  expect_s3_class(observed$metadata, "data.frame")
})

test_that("first intake call writes a scaffold and stops for human review", {
  fixture <- make_intake_fixture()
  original <- fixture$data
  output_dir <- tempfile("intake-scaffold-")
  observed <- epi_eda_intake_run(fixture$data, output_dir = output_dir)

  expect_identical(observed$status, "review_required")
  expect_identical(observed$stage, "intake")
  expect_identical(fixture$data, original)
  expect_true(all(observed$spec$data$review_status == "review_required"))
  expect_true(file.exists(file.path(output_dir, "spec_scaffold.csv")))
  expect_true(file.exists(file.path(output_dir, "review_guide.md")))
  expect_true(file.exists(file.path(output_dir, "report.html")))
  expect_null(observed$schema_before)
  expect_null(observed$summary)
  expect_null(observed$stratified)
  expect_false(any(grepl("^summary_|^stratified_|^table1", list.files(output_dir))))
  report <- paste(readLines(file.path(output_dir, "report.html")), collapse = "\n")
  expect_match(report, "INCOMPLETE")
  expect_match(report, "review_required", fixed = TRUE)
  expect_match(report, "not disclosure-controlled", fixed = TRUE)
})

test_that("invalid specifications return a blocked bundle with actionable messages", {
  fixture <- make_intake_fixture()
  invalid <- fixture$spec[, setdiff(names(fixture$spec), "type"), drop = FALSE]
  output_dir <- tempfile("intake-invalid-spec-")
  observed <- epi_eda_intake_run(
    fixture$data, invalid, output_dir, render = FALSE
  )

  expect_identical(observed$status, "blocked")
  expect_identical(observed$stage, "intake")
  expect_identical(observed$spec$state, "invalid")
  expect_true(any(observed$messages$severity == "blocker"))
  expect_match(observed$messages$reason, "did not satisfy")
  expect_false(file.exists(file.path(output_dir, "spec_reviewed.csv")))
  expect_false(file.exists(file.path(output_dir, "schema_before.csv")))
})

test_that("audit-only mode is distinct and preserves canonical audit results", {
  fixture <- make_intake_fixture()
  expected <- epi_eda_prepare(fixture$data, fixture$spec, mode = "audit")
  output_dir <- tempfile("intake-audit-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir, prepare = "audit", render = FALSE
  )

  expect_identical(observed$status, "audit_complete")
  expect_identical(observed$stage, "audit")
  expect_identical(observed$schema_before, expected$schema_before)
  expect_identical(observed$preparation_audit, expected$audit)
  expect_null(observed$schema_after)
  expect_null(observed$summary)
  expect_false(any(grepl("^summary_", list.files(output_dir))))
})

test_that("audit blockers stop before preparation and summaries", {
  fixture <- make_intake_fixture()
  absent <- fixture$spec[1, , drop = FALSE]
  absent$name <- "required_absent"
  absent$label <- "Required absent"
  absent$required <- TRUE
  spec <- rbind(fixture$spec, absent)
  output_dir <- tempfile("intake-blocked-")
  observed <- epi_eda_intake_run(
    fixture$data, spec, output_dir, prepare = "apply", render = FALSE
  )

  expect_identical(observed$status, "blocked")
  expect_identical(observed$stage, "audit")
  expect_true(any(observed$preparation_audit$status == "blocking"))
  expect_true(any(observed$messages$subject == "required_absent"))
  expect_null(observed$schema_after)
  expect_null(observed$summary)
  expect_false(file.exists(file.path(output_dir, "schema_after.csv")))
})

test_that("unreviewed scaffold evidence is never labelled reviewed", {
  fixture <- make_intake_fixture()
  unreviewed <- fixture$spec
  unreviewed$review_status[[1]] <- "review_required"
  output_dir <- tempfile("intake-unreviewed-")
  observed <- epi_eda_intake_run(
    fixture$data, unreviewed, output_dir, render = FALSE
  )

  expect_identical(observed$status, "blocked")
  expect_identical(observed$spec$state, "review_required")
  expect_identical(
    observed$metadata$spec_review_state,
    "review_required"
  )
})

test_that("prepare none blocks planned changes while apply is all-or-nothing", {
  fixture <- make_intake_fixture()
  fixture$data$value <- c(1L, 2L, NA_integer_, 4L)
  value_row <- fixture$spec$name == "value"
  fixture$spec$type[value_row] <- "numeric"
  fixture$spec$observed_class[value_row] <- "integer"
  original_data <- fixture$data
  original_spec <- fixture$spec

  blocked_dir <- tempfile("intake-none-")
  blocked <- epi_eda_intake_run(
    fixture$data, fixture$spec, blocked_dir, prepare = "none", render = FALSE
  )
  expect_identical(blocked$status, "blocked")
  expect_true(any(grepl("Preparation is required", blocked$messages$reason, fixed = TRUE)))
  expect_null(blocked$summary)

  applied_dir <- tempfile("intake-apply-")
  applied <- epi_eda_intake_run(
    fixture$data, fixture$spec, applied_dir, prepare = "apply", render = FALSE
  )
  expect_identical(applied$status, "complete")
  expect_true(file.exists(file.path(applied_dir, "schema_after.csv")))
  expect_true(all(
    applied$schema_after$type_status[
      applied$schema_after$expected_present & applied$schema_after$observed_present
    ] == "compatible"
  ))
  expect_equal(
    applied$summary$numeric$mean[applied$summary$numeric$name == "value"],
    mean(c(1, 2, 4))
  )
  expect_identical(fixture$data, original_data)
  expect_identical(fixture$spec, original_spec)
})

test_that("canonical outputs policy-skip explicit identifier values", {
  fixture <- make_intake_fixture()
  canonical <- epi_eda_profile_summaries(fixture$data, fixture$spec)
  output_dir <- tempfile("intake-privacy-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir, render = FALSE
  )

  expect_identical(
    observed$summary$numeric[observed$summary$numeric$name != "participant_id", , drop = FALSE],
    canonical$numeric[canonical$numeric$name != "participant_id", , drop = FALSE]
  )
  exported_variables <- utils::read.csv(
    file.path(output_dir, "summary_variables.csv"), stringsAsFactors = FALSE
  )
  exported_numeric <- utils::read.csv(
    file.path(output_dir, "summary_numeric.csv"), stringsAsFactors = FALSE
  )
  exported_skipped <- utils::read.csv(
    file.path(output_dir, "summary_skipped.csv"), stringsAsFactors = FALSE
  )
  expect_identical(
    exported_variables$status[exported_variables$name == "participant_id"],
    "skipped"
  )
  expect_false("participant_id" %in% exported_numeric$name)
  expect_true("participant_id" %in% exported_skipped$name)
  expect_true(is.na(observed$missing$n_missing[observed$missing$name == "participant_id"]))
  contents <- bundle_text(output_dir)
  expect_false(grepl("900001", contents, fixed = TRUE))
  expect_false(grepl("PRIVATE_TEXT_A", contents, fixed = TRUE))
  expect_false(grepl("CODECOV_TOKEN", contents, fixed = TRUE))
})

test_that("valid stratification is optional and reconciles with canonical counts", {
  fixture <- make_intake_fixture()
  without_dir <- tempfile("intake-unstratified-")
  without <- epi_eda_intake_run(
    fixture$data, fixture$spec, without_dir, render = FALSE
  )
  with_dir <- tempfile("intake-stratified-")
  with <- epi_eda_intake_run(
    fixture$data, fixture$spec, with_dir, strata = "arm", render = FALSE
  )

  expect_identical(with$status, "complete")
  expect_identical(with$summary, without$summary)
  expect_s3_class(with$stratified, "epi_eda_stratified")
  expect_equal(with$stratified$metadata$n_input, nrow(fixture$data))
  expect_equal(with$stratified$groups$n[with$stratified$groups$is_overall], nrow(fixture$data))
  expect_true(file.exists(file.path(with_dir, "stratified_variables.csv")))
  expect_true(file.exists(file.path(with_dir, "stratified_metadata.csv")))
  expect_true(file.exists(file.path(with_dir, "table1.csv")))
  expect_false(file.exists(file.path(without_dir, "table1.csv")))
})

test_that("completion reconciliation detects missing components and changed Overall values", {
  fixture <- make_intake_fixture()
  output_dir <- tempfile("intake-reconciliation-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir,
    strata = "arm", render = FALSE
  )
  profile_data <- fixture$data[, names(fixture$data) != "participant_id", drop = FALSE]
  missing_component <- observed$summary
  missing_component$numeric <- missing_component$numeric[0, , drop = FALSE]

  expect_match(
    episcout:::intake_reconcile_canonical(
      missing_component, observed$missing, profile_data, fixture$spec
    ),
    "component membership"
  )

  duplicated_component <- observed$summary
  duplicated_component$numeric <- rbind(
    duplicated_component$numeric,
    duplicated_component$numeric[1, , drop = FALSE]
  )
  expect_match(
    episcout:::intake_reconcile_canonical(
      duplicated_component, observed$missing, profile_data, fixture$spec
    ),
    "component membership"
  )

  changed_overall <- observed$stratified
  overall <- changed_overall$numeric$group_id == ".overall" &
    changed_overall$numeric$name == "value"
  changed_overall$numeric$mean[overall] <- 999
  expect_match(
    episcout:::intake_reconcile_stratified(
      observed$summary, changed_overall, fixture$data
    ),
    "numeric summaries do not agree"
  )

  changed_missing <- observed$stratified
  missing_row <- changed_missing$categorical$group_id == ".overall" &
    changed_missing$categorical$name == "status" &
    changed_missing$categorical$is_missing_level
  changed_missing$categorical$n[missing_row] <- 999L
  expect_match(
    episcout:::intake_reconcile_stratified(
      observed$summary, changed_missing, fixture$data
    ),
    "categorical counts or proportions"
  )

  changed_group <- observed$stratified
  arm_a_value <- changed_group$numeric$group_id == ".stratum.001" &
    changed_group$numeric$name == "value"
  changed_group$numeric$n[arm_a_value] <- 999L
  expect_match(
    episcout:::intake_reconcile_stratified(
      observed$summary, changed_group, fixture$data
    ),
    "denominators"
  )

  missing_group_numeric <- observed$stratified
  missing_group_numeric$numeric <- missing_group_numeric$numeric[
    missing_group_numeric$numeric$group_id != ".stratum.001", ,
    drop = FALSE
  ]
  expect_match(
    episcout:::intake_reconcile_stratified(
      observed$summary, missing_group_numeric, fixture$data
    ),
    "component membership"
  )

  missing_group_categorical <- observed$stratified
  missing_group_categorical$categorical <-
    missing_group_categorical$categorical[
      missing_group_categorical$categorical$group_id != ".stratum.001", ,
      drop = FALSE
    ]
  expect_match(
    episcout:::intake_reconcile_stratified(
      observed$summary, missing_group_categorical, fixture$data
    ),
    "component membership"
  )
})

test_that("invalid stratifiers retain truthful canonical artifacts and block completion", {
  fixture <- make_intake_fixture()
  output_dir <- tempfile("intake-invalid-strata-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir,
    strata = "value", render = FALSE
  )

  expect_identical(observed$status, "blocked")
  expect_identical(observed$stage, "canonical_summary")
  expect_false(is.null(observed$summary))
  expect_true(file.exists(file.path(output_dir, "summary_variables.csv")))
  expect_false(file.exists(file.path(output_dir, "stratified_groups.csv")))
  expect_false(file.exists(file.path(output_dir, "table1.csv")))
  expect_true(any(observed$messages$subject == "value"))
})

test_that("explicit identifier roles cannot be used as stratifiers", {
  fixture <- make_intake_fixture()
  identifier <- fixture$spec$name == "participant_id"
  fixture$spec$type[identifier] <- "categorical"
  fixture$spec$levels[identifier] <- "900001;900002;900003;900004"
  output_dir <- tempfile("intake-identifier-strata-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir,
    prepare = "apply", strata = "participant_id", render = FALSE
  )

  expect_identical(observed$status, "blocked")
  expect_identical(observed$stage, "canonical_summary")
  expect_true(any(grepl(
    "cannot be used as an intake stratifier",
    observed$messages$reason[observed$messages$subject == "participant_id"],
    fixed = TRUE
  )))
  expect_false(file.exists(file.path(output_dir, "stratified_groups.csv")))
})

test_that("specification provenance is local, portable and explicit", {
  fixture <- make_intake_fixture()
  core <- fixture$spec[, c(
    "name", "label", "type", "role", "levels", "missing_codes", "required"
  )]
  data_dir <- tempfile("intake-caller-asserted-")
  from_data <- epi_eda_intake_run(
    fixture$data, core, data_dir, render = FALSE
  )
  expect_identical(from_data$spec$state, "caller_asserted")
  expect_true(any(grepl("caller-asserted", from_data$messages$reason, fixed = TRUE)))

  spec_path <- tempfile("reviewed-local-", fileext = ".csv")
  utils::write.csv(fixture$spec, spec_path, row.names = FALSE, na = "")
  csv_dir <- tempfile("intake-csv-spec-")
  from_csv <- epi_eda_intake_run(
    fixture$data, spec_path, csv_dir, render = FALSE
  )
  from_reviewed_frame <- epi_eda_intake_run(
    fixture$data, fixture$spec, tempfile("intake-frame-spec-"), render = FALSE
  )
  expect_identical(from_csv$spec$source, "csv")
  expect_identical(from_csv$spec$source_name, basename(spec_path))
  expect_false(grepl(dirname(spec_path), bundle_text(csv_dir), fixed = TRUE))
  expect_identical(
    from_csv$spec$fingerprint_sha256,
    from_reviewed_frame$spec$fingerprint_sha256
  )

  remote_dir <- tempfile("intake-remote-spec-")
  expect_error(
    epi_eda_intake_run(
      fixture$data, "https://example.test/spec.csv", remote_dir
    ),
    "network URLs"
  )
  expect_false(dir.exists(remote_dir))
})

test_that("manifest created rows correspond exactly to bundle files", {
  fixture <- make_intake_fixture()
  output_dir <- tempfile("intake-manifest-")
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir, strata = "arm"
  )
  created <- sort(observed$manifest$path[observed$manifest$status == "created"])
  actual <- sort(list.files(output_dir))

  expect_identical(created, actual)
  expect_true(all(!grepl("^/|[.][.]", observed$manifest$path)))
  checksum_rows <- observed$manifest$status == "created" &
    observed$manifest$artifact != "manifest"
  expected <- unname(tools::md5sum(file.path(
    output_dir, observed$manifest$path[checksum_rows]
  )))
  expect_identical(observed$manifest$checksum_md5[checksum_rows], expected)
})

test_that("output collisions are refused and owned overwrite is conservative", {
  fixture <- make_intake_fixture()
  unowned <- tempfile("intake-unowned-")
  dir.create(unowned)
  writeLines("preserve", file.path(unowned, "user.txt"))
  expect_error(
    epi_eda_intake_run(fixture$data, output_dir = unowned),
    "non-empty"
  )
  expect_error(
    epi_eda_intake_run(fixture$data, output_dir = unowned, overwrite = TRUE),
    "valid prior"
  )
  expect_identical(readLines(file.path(unowned, "user.txt")), "preserve")

  owned <- tempfile("intake-owned-")
  first <- epi_eda_intake_run(
    fixture$data, fixture$spec, owned, strata = "arm", render = FALSE
  )
  expect_identical(first$status, "complete")
  second <- epi_eda_intake_run(
    fixture$data, output_dir = owned, overwrite = TRUE, render = FALSE
  )
  expect_identical(second$status, "review_required")
  expect_false(file.exists(file.path(owned, "summary_variables.csv")))
  expect_false(file.exists(file.path(owned, "table1.csv")))

  writeLines("unowned", file.path(owned, "user.txt"))
  expect_error(
    epi_eda_intake_run(
      fixture$data, fixture$spec, owned, overwrite = TRUE, render = FALSE
    ),
    "do not match"
  )
  expect_identical(readLines(file.path(owned, "user.txt")), "unowned")
})

test_that("overwrite is transactional and rejects registered-name impostors", {
  fixture <- make_intake_fixture()
  owned <- tempfile("intake-transaction-")
  first <- epi_eda_intake_run(
    fixture$data, fixture$spec, owned, strata = "arm", render = FALSE
  )
  before_files <- sort(list.files(owned))
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
  expect_identical(sort(list.files(owned)), before_files)
  expect_identical(
    unname(tools::md5sum(file.path(owned, before_files))),
    before_checksums
  )
  expect_identical(first$status, "complete")

  review_dir <- tempfile("intake-impostor-")
  review_run <- epi_eda_intake_run(
    fixture$data, output_dir = review_dir, render = FALSE
  )
  expect_identical(review_run$status, "review_required")
  writeLines("user-owned", file.path(review_dir, "summary_numeric.csv"))
  expect_error(
    epi_eda_intake_run(
      fixture$data, output_dir = review_dir,
      overwrite = TRUE, render = FALSE
    ),
    "do not match"
  )
  expect_identical(
    readLines(file.path(review_dir, "summary_numeric.csv")),
    "user-owned"
  )
})

test_that("overwrite rejects malformed manifests and restores failed swaps", {
  fixture <- make_intake_fixture()
  malformed <- tempfile("intake-malformed-manifest-")
  epi_eda_intake_run(
    fixture$data, fixture$spec, malformed, render = FALSE
  )
  manifest_path <- file.path(malformed, "manifest.csv")
  manifest <- utils::read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    na.strings = character()
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
    fixture$data, fixture$spec, owned, render = FALSE
  )
  before_files <- sort(list.files(owned))
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
  expect_identical(sort(list.files(owned)), before_files)
  expect_identical(
    unname(tools::md5sum(file.path(owned, before_files))),
    before_checksums
  )
})

test_that("report failure retains analysis artifacts and cannot return complete", {
  fixture <- make_intake_fixture()
  output_dir <- tempfile("intake-report-failure-")
  local_mocked_bindings(
    intake_render_report = function(...) stop("simulated render failure"),
    .package = "episcout"
  )
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir, render = TRUE
  )

  expect_identical(observed$status, "blocked")
  expect_false(observed$report$created)
  expect_match(observed$report$reason, "could not be created")
  expect_true(file.exists(file.path(output_dir, "summary_variables.csv")))
  expect_false(file.exists(file.path(output_dir, "report.html")))
  expect_identical(
    observed$manifest$status[observed$manifest$artifact == "report"],
    "not_created"
  )
})

test_that("Table 1 failure retains reconciled stratified components", {
  fixture <- make_intake_fixture()
  output_dir <- tempfile("intake-table1-failure-")
  local_mocked_bindings(
    epi_eda_table1 = function(...) stop("simulated Table 1 failure"),
    .package = "episcout"
  )
  observed <- epi_eda_intake_run(
    fixture$data, fixture$spec, output_dir,
    strata = "arm", render = FALSE
  )

  expect_identical(observed$status, "blocked")
  expect_identical(observed$stage, "stratified_summary")
  expect_s3_class(observed$stratified, "epi_eda_stratified")
  expect_null(observed$table1)
  expect_true(file.exists(file.path(output_dir, "stratified_groups.csv")))
  expect_false(file.exists(file.path(output_dir, "table1.csv")))
})

test_that("report failure does not hide an underlying human-review gate", {
  fixture <- make_intake_fixture()
  output_dir <- tempfile("intake-review-report-failure-")
  local_mocked_bindings(
    intake_render_report = function(...) stop("simulated gate render failure"),
    .package = "episcout"
  )
  observed <- epi_eda_intake_run(fixture$data, output_dir = output_dir)

  expect_identical(observed$status, "review_required")
  expect_identical(observed$stage, "intake")
  expect_false(observed$report$created)
  expect_true(any(observed$messages$subject == "report.html"))
})

test_that("rendered reports escape metadata and remain portable", {
  fixture <- make_intake_fixture()
  fixture$spec$label[fixture$spec$name == "value"] <- "<script>alert('x')</script>"
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
  expect_match(html, "finished_at_utc", fixed = TRUE)

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

test_that("argument validation occurs before bundle writes", {
  fixture <- make_intake_fixture()
  expect_error(
    epi_eda_intake_run(list(), output_dir = tempfile()),
    "data frame"
  )
  duplicate <- fixture$data
  names(duplicate)[2] <- names(duplicate)[1]
  expect_error(
    epi_eda_intake_run(duplicate, output_dir = tempfile()),
    "Duplicate"
  )
  expect_error(
    epi_eda_intake_run(fixture$data, output_dir = tempfile(), render = NA),
    "render"
  )
  expect_error(
    epi_eda_intake_run(fixture$data, output_dir = tempfile(), strata = c("arm", "status")),
    "strata"
  )
  expect_error(
    epi_eda_intake_run(fixture$data, output_dir = tempfile(), source_id = "/private/source.csv"),
    "absolute"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data,
      output_dir = tempfile(),
      source_id = "\\\\server\\share\\source.csv"
    ),
    "absolute"
  )
  expect_error(
    epi_eda_intake_run(
      fixture$data,
      output_dir = tempfile(),
      source_id = "\\Users\\private\\source.csv"
    ),
    "absolute"
  )
  reserved <- fixture$data
  names(reserved)[1] <- ".dataset.private"
  expect_error(
    epi_eda_intake_run(reserved, output_dir = tempfile()),
    "reserved"
  )

  target <- tempfile("intake-link-target-")
  dir.create(target)
  link <- tempfile("intake-link-")
  linked <- file.symlink(target, link)
  if (linked) {
    expect_error(
      epi_eda_intake_run(fixture$data, output_dir = link),
      "symbolic link"
    )
    expect_length(list.files(target), 0L)
  }
})

test_that("data-frame subclasses are not mutated by reference", {
  skip_if_not_installed("data.table")
  fixture <- make_intake_fixture()
  data <- data.table::as.data.table(fixture$data)
  original <- data.table::copy(data)
  observed <- epi_eda_intake_run(
    data, fixture$spec, tempfile("intake-data-table-"),
    strata = "arm", render = FALSE
  )

  expect_identical(observed$status, "complete")
  expect_identical(data, original)
})

test_that("zero-row inputs retain stable summary schemas", {
  fixture <- make_intake_fixture()
  data <- fixture$data[0, , drop = FALSE]
  spec <- epi_eda_spec_scaffold(data)
  spec$role <- fixture$spec$role
  spec$review_status <- "reviewed"
  output_dir <- tempfile("intake-zero-row-")
  observed <- epi_eda_intake_run(
    data, spec, output_dir, prepare = "none", render = FALSE
  )

  expect_identical(observed$status, "complete")
  expect_true(all(
    observed$summary$variables$n[
      observed$summary$variables$name != "participant_id"
    ] == 0L
  ))
  expect_true(is.na(
    observed$summary$variables$n[
      observed$summary$variables$name == "participant_id"
    ]
  ))
  expect_true(file.exists(file.path(output_dir, "summary_categorical.csv")))
  expect_named(
    utils::read.csv(file.path(output_dir, "summary_numeric.csv"), check.names = FALSE),
    names(observed$summary$numeric)
  )
})
