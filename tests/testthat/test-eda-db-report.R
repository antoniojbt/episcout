context("PostgreSQL EDA bundle reports")

db_report_skip <- function() {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  if (!rmarkdown::pandoc_available()) skip("Pandoc is required")
}

db_report_empty <- function(columns) {
  out <- as.data.frame(
    stats::setNames(rep(list(character()), length(columns)), columns),
    stringsAsFactors = FALSE
  )
  out
}

db_report_fixture <- function(layout = "bundle",
                              with_plot = TRUE,
                              with_skipped_map = FALSE,
                              categorical = FALSE,
                              plot_data_contract = "compact-plot-data-2") {
  root <- tempfile("eda-db-report-bundle-")
  dir.create(root)
  metadata <- data.frame(
    workflow_contract = "postgres-eda-bundle-2",
    status = "complete",
    n_rows = 42L,
    n_spec_variables = 1L,
    plot_data_contract = if (layout == "delivery") {
      plot_data_contract
    } else {
      "compact-plot-data-1"
    },
    max_plot_levels = 2L,
    stringsAsFactors = FALSE
  )
  messages <- data.frame(
    stage = character(), severity = character(), subject = character(),
    reason = character(), recommended_action = character(),
    stringsAsFactors = FALSE
  )
  variable_name <- if (categorical) "status" else "measurement"
  variable_label <- if (categorical) "Status" else "Measurement"
  variable_type <- if (categorical) "categorical" else "numeric"
  spec <- data.frame(
    name = variable_name, label = variable_label, type = variable_type,
    role = "measure", stringsAsFactors = FALSE
  )
  source <- data.frame(
    schema_name = "fixture", relation_name = "aggregate_source",
    relation_kind = "table", column_count = 1L,
    source_contract = "postgres-source-1", server_version_num = 180000L,
    stringsAsFactors = FALSE
  )
  snapshot <- list(
    schema = data.frame(name = variable_name, status = "compatible", stringsAsFactors = FALSE),
    missing = data.frame(
      name = variable_name, n = 42L, n_missing = 2L,
      p_missing = 2 / 42, stringsAsFactors = FALSE
    ),
    summaries = list(
      variables = data.frame(
        name = variable_name, n = 42L, n_missing = 2L,
        n_observed = 40L, status = "summarised", reason = NA_character_,
        stringsAsFactors = FALSE
      ),
      numeric = if (categorical) {
        db_report_empty(c("name", "n_finite", "min", "mean", "max"))
      } else {
        data.frame(
          name = "measurement", n_finite = 40L, min = 1,
          mean = 3.5, max = 6, stringsAsFactors = FALSE
        )
      },
      categorical = if (categorical) {
        data.frame(
          name = c("status", "status"), level = c("no", "yes"),
          n = c(15L, 25L), p_total = c(15 / 42, 25 / 42),
          p_observed = c(15 / 40, 25 / 40), stringsAsFactors = FALSE
        )
      } else {
        db_report_empty(c("name", "level", "n", "p_total", "p_observed"))
      },
      text = db_report_empty(c("name", "n", "n_missing", "n_observed", "n_unique")),
      temporal = db_report_empty(c("name", "n", "n_missing", "n_observed", "range_unit")),
      skipped = db_report_empty(c("name", "type", "reason"))
    ),
    identifier_qa = db_report_empty(c("name", "status", "reason")),
    geo = db_report_empty(c("geo_pair", "status", "reason"))
  )
  plot_inventory <- data.frame(
    variable_index = 1L, name = variable_name, type = variable_type,
    plot_type = if (categorical) "frequency" else "histogram",
    n_total = 42L, n_missing = 2L,
    n_plotted = 40L, n_excluded_non_finite = 0L,
    n_displayed_levels = if (categorical) 2L else NA_integer_,
    n_collapsed_levels = if (categorical) 0L else NA_integer_,
    status = if (with_plot) "created" else "not_created",
    reason = if (with_plot) NA_character_ else "Plot creation was disabled by the caller.",
    path = if (with_plot) {
      paste0("plots/001-", if (categorical) "frequency" else "histogram", ".svg")
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
  map_inventory <- if (with_skipped_map) {
    data.frame(
      map_id = "map-p001-geometry", geo_pair = "sites", value = "",
      status = "not_created", reason = "The source exceeded max_map_points.",
      n_source_rows = 10001L, n_mapped = 0L, path = NA_character_,
      stringsAsFactors = FALSE
    )
  } else {
    db_report_empty(c(
      "map_id", "geo_pair", "value", "status", "reason", "n_source_rows",
      "n_mapped", "path"
    ))
  }
  timings <- data.frame(
    stage = "complete_run", query_kind = "end_to_end",
    elapsed_seconds = 0.25, status = "complete", stringsAsFactors = FALSE
  )
  write_tables <- getFromNamespace("eda_db_write_bundle_tables", "episcout")
  write_tables(
    root, metadata, messages, spec, source, snapshot, plot_inventory,
    map_inventory, timings, layout
  )
  if (with_plot) {
    dir.create(file.path(root, "plots"))
    writeLines(
      '<svg xmlns="http://www.w3.org/2000/svg"><text>aggregate plot</text></svg>',
      file.path(
        root, "plots",
        paste0("001-", if (categorical) "frequency" else "histogram", ".svg")
      )
    )
  }
  plot_data_registry <- db_report_empty(c(
    "artifact", "type", "path", "status", "checksum_md5"
  ))
  if (layout == "delivery") {
    data <- if (categorical) {
      display <- getFromNamespace("eda_cat_display_frequency", "episcout")(
        data.frame(level = c("no", "yes"), n = c(15L, 25L)),
        "status", "Status", "categorical", 42L, 2L
      )
      compact <- getFromNamespace("eda_collapse_frequencies", "episcout")(
        display, 2L
      )
      if (plot_data_contract == "compact-plot-data-1") {
        compact[, c("level", "count", "display_order", "remainder"), drop = FALSE]
      } else {
        compact
      }
    } else {
      data.frame(
        bin = 1:2, lower = c(0, 1), upper = c(1, 2),
        midpoint = c(0.5, 1.5), count = c(20L, 20L)
      )
    }
    entry <- list(
      data = data,
      plot_type = if (categorical) "frequency" else "histogram",
      box_data = NULL
    )
    plot_data_registry <- getFromNamespace("eda_db_write_plot_data", "episcout")(
      root, list(entry), layout
    )
  }
  manifest <- getFromNamespace("eda_db_create_manifest", "episcout")(
    root, plot_inventory, map_inventory, layout, plot_data_registry
  )
  manifest_path <- getFromNamespace(
    "eda_db_manifest_relative_path", "episcout"
  )(layout)
  getFromNamespace("intake_atomic_csv", "episcout")(
    manifest, file.path(root, manifest_path)
  )
  root
}

db_report_bytes <- function(root) {
  paths <- sort(list.files(root, recursive = TRUE, full.names = FALSE))
  stats::setNames(lapply(file.path(root, paths), function(path) {
    readBin(path, "raw", n = file.info(path)$size)
  }), paths)
}

db_report_refresh_checksum <- function(root, artifact) {
  manifest_path <- if (file.exists(file.path(root, "manifest.csv"))) {
    file.path(root, "manifest.csv")
  } else {
    file.path(root, "run_manifests", "manifest.csv")
  }
  manifest <- utils::read.csv(
    manifest_path,
    check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = character()
  )
  index <- match(artifact, manifest$artifact)
  manifest$checksum_md5[index] <- unname(tools::md5sum(file.path(
    root, manifest$path[index]
  )))
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  invisible(TRUE)
}

test_that("database report public formals and default bundle paths are compatible", {
  expect_identical(
    names(formals(epi_eda_render_db_report)),
    c("bundle", "overwrite", "quiet")
  )
  expect_identical(
    names(formals(epi_eda_db_run)),
    c(
      "source", "spec", "output_dir", "overwrite", "plots",
      "max_plot_levels", "maps", "map_vars", "max_map_points", "layout",
      "quiet", "strata", "include_overall", "include_missing_stratum",
      "table1_basis", "plot_style", "plot_style_id"
    )
  )
  registry <- getFromNamespace("eda_db_artifact_registry", "episcout")
  expect_identical(
    registry("bundle")$path,
    c(
      "manifest.csv", "run_metadata.csv", "messages.csv", "specification.csv",
      "source_metadata.csv", "schema.csv", "missing.csv",
      "summary_variables.csv", "summary_numeric.csv",
      "summary_categorical.csv", "summary_text.csv", "summary_temporal.csv",
      "summary_skipped.csv", "identifier_qa.csv", "geo_qa.csv",
      "plot_inventory.csv", "map_inventory.csv", "query_timings.csv"
    )
  )
})

test_that("delivery dependencies are checked before PostgreSQL preflight", {
  source <- structure(
    list(columns = data.frame(name = "measurement", stringsAsFactors = FALSE)),
    class = c("epi_eda_postgres_source", "list")
  )
  spec <- data.frame(
    name = "measurement", label = "Measurement", type = "numeric",
    role = "measure", stringsAsFactors = FALSE
  )
  expect_error(
    with_mocked_bindings(
      epi_eda_db_run(
        source, spec, tempfile("delivery-preflight-"),
        layout = "delivery"
      ),
      eda_db_report_dependencies = function() {
        stop("DEPENDENCY_PRECHECK", call. = FALSE)
      },
      eda_validate_postgres_source = function(...) {
        stop("DATABASE_PREFLIGHT", call. = FALSE)
      },
      .package = "episcout"
    ),
    "DEPENDENCY_PRECHECK"
  )
  expect_error(
    with_mocked_bindings(
      epi_eda_db_run(
        source, spec, tempfile("bundle-preflight-"),
        quiet = NA
      ),
      eda_validate_postgres_source = function(...) {
        stop("DATABASE_PREFLIGHT", call. = FALSE)
      },
      .package = "episcout"
    ),
    "DATABASE_PREFLIGHT"
  )
})

test_that("a validated flat bundle renders atomically and remains relocatable", {
  db_report_skip()
  root <- db_report_fixture()
  report <- epi_eda_render_db_report(root)

  expect_identical(report, normalizePath(
    file.path(root, "reports", "eda-report.html"),
    winslash = "/"
  ))
  expect_true(file.exists(file.path(root, "README.md")))
  manifest <- utils::read.csv(
    file.path(root, "manifest.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE, na.strings = character()
  )
  expect_identical(names(manifest), c(
    "artifact", "type", "path", "status", "checksum_md5"
  ))
  expect_setequal(manifest$artifact[manifest$artifact %in% c("readme", "report")], c("readme", "report"))
  expect_identical(
    sort(manifest$path[manifest$status == "created"]),
    sort(list.files(root, recursive = TRUE))
  )
  checked <- manifest$status == "created" & manifest$artifact != "manifest"
  expect_identical(
    manifest$checksum_md5[checked],
    unname(tools::md5sum(file.path(root, manifest$path[checked])))
  )
  html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(html, "PostgreSQL EDA Report", fixed = TRUE)
  expect_match(html, "42", fixed = TRUE)
  expect_match(html, "Measurement", fixed = TRUE)
  expect_match(html, "Identifier QA", fixed = TRUE)
  expect_match(html, "Declared coordinate-pair QA", fixed = TRUE)
  expect_match(html, "Query timings", fixed = TRUE)
  expect_match(html, "../plots/001-histogram.svg", fixed = TRUE)
  expect_match(html, "../missing.csv", fixed = TRUE)
  expect_false(grepl("ROW_VALUE_CANARY|CONNECTION_CANARY|SOURCE_ROW_CANARY", html))
  first_html <- readBin(report, "raw", n = file.info(report)$size)

  relocated <- paste0(root, "-relocated")
  expect_true(file.rename(root, relocated))
  parsed <- getFromNamespace("eda_db_read_bundle", "episcout")(relocated)
  expect_identical(parsed$layout, "bundle")
  expect_error(epi_eda_render_db_report(relocated), "already has an owned report")
  overwritten <- epi_eda_render_db_report(relocated, overwrite = TRUE)
  expect_true(file.exists(overwritten))
  expect_identical(
    readBin(overwritten, "raw", n = file.info(overwritten)$size), first_html
  )
})

test_that("empty and skipped report states remain explicit", {
  db_report_skip()
  root <- db_report_fixture(
    with_plot = FALSE, with_skipped_map = TRUE
  )
  report <- epi_eda_render_db_report(root)
  html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(html, "No rows were produced for this component.", fixed = TRUE)
  expect_match(html, "The source exceeded max_map_points.", fixed = TRUE)
  expect_match(html, "10001", fixed = TRUE)
  expect_match(html, "not_created", fixed = TRUE)
})

test_that("result objects contribute only output_dir and rendering never needs DBI", {
  db_report_skip()
  root <- db_report_fixture(with_plot = FALSE)
  result <- structure(
    list(
      output_dir = root,
      source = list(con = "CONNECTION_CANARY"),
      observations = "SOURCE_ROW_CANARY"
    ),
    class = c("epi_eda_db_run", "list")
  )
  report <- with_mocked_bindings(
    epi_eda_render_db_report(result),
    eda_validate_postgres_source = function(...) {
      stop("DATABASE_PREFLIGHT_CANARY", call. = FALSE)
    },
    eda_postgres_transaction = function(...) {
      stop("DATABASE_SNAPSHOT_CANARY", call. = FALSE)
    },
    .package = "episcout"
  )
  html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_false(grepl("CONNECTION_CANARY|SOURCE_ROW_CANARY", html))
})

test_that("bundle integrity failures are value-free and do not mutate the root", {
  root <- db_report_fixture()
  missing <- file.path(root, "missing.csv")
  write("ROW_VALUE_CANARY", missing, append = TRUE)
  before <- db_report_bytes(root)
  error <- tryCatch(epi_eda_render_db_report(root), error = identity)
  expect_match(conditionMessage(error), "checksums", fixed = TRUE)
  expect_false(grepl("ROW_VALUE_CANARY", conditionMessage(error), fixed = TRUE))
  expect_identical(db_report_bytes(root), before)

  root <- db_report_fixture()
  writeLines("unowned", file.path(root, "extra.txt"))
  expect_error(epi_eda_render_db_report(root), "exactly match")

  root <- db_report_fixture()
  dir.create(file.path(root, "unowned-empty-directory"))
  expect_error(epi_eda_render_db_report(root), "directories")

  root <- db_report_fixture()
  manifest <- utils::read.csv(file.path(root, "manifest.csv"), check.names = FALSE)
  manifest$sensitivity <- "restricted"
  utils::write.csv(manifest, file.path(root, "manifest.csv"), row.names = FALSE)
  expect_error(epi_eda_render_db_report(root), "removed sensitivity schema")
})

test_that("bundle roots and manifests are unambiguous and never symlinked", {
  expect_error(epi_eda_render_db_report(NULL), "one database-EDA result")
  expect_error(
    epi_eda_render_db_report(tempfile("missing-db-bundle-")),
    "does not exist"
  )

  root <- db_report_fixture()
  link <- tempfile("eda-db-root-link-")
  if (file.symlink(root, link)) {
    expect_error(epi_eda_render_db_report(link), "must not be a symbolic link")
  }

  root <- db_report_fixture()
  dir.create(file.path(root, "run_manifests"))
  file.copy(
    file.path(root, "manifest.csv"),
    file.path(root, "run_manifests", "manifest.csv")
  )
  expect_error(epi_eda_render_db_report(root), "unambiguous manifest")

  root <- db_report_fixture()
  expect_true(file.symlink(
    file.path(root, "missing.csv"), file.path(root, "linked.csv")
  ))
  expect_error(epi_eda_render_db_report(root), "symbolic links")
})

test_that("unsafe and ambiguous manifest paths fail before artifact access", {
  unsafe <- c("../missing.csv", "/missing.csv", "QA_QC\\missing.csv")
  for (path in unsafe) {
    root <- db_report_fixture()
    manifest_path <- file.path(root, "manifest.csv")
    manifest <- utils::read.csv(
      manifest_path,
      check.names = FALSE, stringsAsFactors = FALSE,
      na.strings = character()
    )
    manifest$path[manifest$artifact == "missing"] <- path
    utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
    expect_error(epi_eda_render_db_report(root), "unsafe relative path")
  }

  root <- db_report_fixture()
  manifest_path <- file.path(root, "manifest.csv")
  manifest <- utils::read.csv(
    manifest_path,
    check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = character()
  )
  manifest$path[manifest$artifact == "messages"] <- "MISSING.csv"
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  expect_error(epi_eda_render_db_report(root), "five-column contract")
})

test_that("checksum-valid but inconsistent aggregates fail closed", {
  root <- db_report_fixture()
  metadata_path <- file.path(root, "run_metadata.csv")
  metadata <- utils::read.csv(metadata_path, check.names = FALSE)
  metadata$status <- "ROW_VALUE_CANARY"
  utils::write.csv(metadata, metadata_path, row.names = FALSE, na = "")
  db_report_refresh_checksum(root, "run_metadata")
  error <- tryCatch(epi_eda_render_db_report(root), error = identity)
  expect_match(conditionMessage(error), "incomplete or inconsistent", fixed = TRUE)
  expect_false(grepl("ROW_VALUE_CANARY", conditionMessage(error), fixed = TRUE))

  root <- db_report_fixture()
  inventory_path <- file.path(root, "plot_inventory.csv")
  inventory <- utils::read.csv(
    inventory_path,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  inventory$status <- "not_created"
  inventory$path <- NA_character_
  utils::write.csv(inventory, inventory_path, row.names = FALSE, na = "")
  db_report_refresh_checksum(root, "plot_inventory")
  expect_error(epi_eda_render_db_report(root), "incomplete or inconsistent")
})

test_that("a failed publication swap restores every original byte", {
  db_report_skip()
  root <- db_report_fixture()
  before <- db_report_bytes(root)
  calls <- 0L
  expect_error(
    with_mocked_bindings(
      epi_eda_render_db_report(root),
      intake_rename = function(from, to) {
        calls <<- calls + 1L
        if (calls == 2L) {
          return(FALSE)
        }
        file.rename(from, to)
      },
      .package = "episcout"
    ),
    "prior bundle was restored"
  )
  expect_identical(db_report_bytes(root), before)
})

test_that("render failures preserve every original bundle byte", {
  db_report_skip()
  root <- db_report_fixture()
  before <- db_report_bytes(root)
  expect_error(
    with_mocked_bindings(
      epi_eda_render_db_report(root),
      eda_db_render_staged_bundle = function(...) {
        stop("RENDER_CANARY", call. = FALSE)
      },
      .package = "episcout"
    ),
    "RENDER_CANARY"
  )
  expect_identical(db_report_bytes(root), before)
})

test_that("delivery registry separates QA, manifests, and compact plot data", {
  db_report_skip()
  root <- db_report_fixture(layout = "delivery")
  parsed <- getFromNamespace("eda_db_read_bundle", "episcout")(root)
  expect_identical(parsed$layout, "delivery")
  expect_true(file.exists(file.path(root, "QA_QC", "missing.csv")))
  expect_true(file.exists(file.path(
    root, "run_manifests", "delivery_metadata.csv"
  )))
  plot_data <- parsed$manifest[
    parsed$manifest$type == "plot_data", "path",
    drop = TRUE
  ]
  expect_identical(plot_data, "plot_data/001-histogram.csv")
  compact <- utils::read.csv(file.path(root, plot_data), check.names = FALSE)
  expect_identical(compact$count, c(20L, 20L))
  expect_false(any(grepl("coordinate|theme|source_row", names(compact))))
  report <- epi_eda_render_db_report(root)
  expect_true(file.exists(report))
  rendered <- getFromNamespace("eda_db_read_bundle", "episcout")(root)
  expect_identical(
    rendered$manifest$path[rendered$manifest$artifact == "manifest"],
    "run_manifests/manifest.csv"
  )
})

test_that("delivery reports validate and display enriched frequency companions", {
  db_report_skip()
  root <- db_report_fixture(layout = "delivery", categorical = TRUE)
  parsed <- getFromNamespace("eda_db_read_bundle", "episcout")(root)
  params <- getFromNamespace("eda_db_report_params", "episcout")(parsed)

  expect_named(params$frequency_companions, "Status")
  companion <- params$frequency_companions$Status
  expect_named(
    companion,
    getFromNamespace("eda_frequency_companion_names", "episcout")()
  )
  expect_identical(companion$count, c(25L, 15L))
  expect_identical(companion$numerator, companion$count)
  expect_identical(companion$denominator, c(40L, 40L))
  expect_equal(companion$proportion, c(25 / 40, 15 / 40))

  report <- epi_eda_render_db_report(root)
  html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(html, "Categorical percentage companions", fixed = TRUE)
  expect_match(html, "compatibility", fixed = TRUE)
  expect_match(html, "0.625", fixed = TRUE)
  expect_false(grepl("SOURCE_ROW_CANARY|SELECT |postgresql://", html))
})

test_that("legacy frequency companions are enriched in memory only", {
  db_report_skip()
  root <- db_report_fixture(
    layout = "delivery", categorical = TRUE,
    plot_data_contract = "compact-plot-data-1"
  )
  path <- file.path(root, "plot_data", "001-frequency.csv")
  before <- readBin(path, "raw", n = file.info(path)$size)
  parsed <- getFromNamespace("eda_db_read_bundle", "episcout")(root)
  params <- getFromNamespace("eda_db_report_params", "episcout")(parsed)

  expect_identical(names(utils::read.csv(path)), c(
    "level", "count", "display_order", "remainder"
  ))
  expect_named(
    params$frequency_companions$Status,
    getFromNamespace("eda_frequency_companion_names", "episcout")()
  )
  report <- epi_eda_render_db_report(root)
  expect_true(file.exists(report))
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
})

test_that("inconsistent frequency companions fail before publication", {
  root <- db_report_fixture(layout = "delivery", categorical = TRUE)
  path <- file.path(root, "plot_data", "001-frequency.csv")
  companion <- utils::read.csv(path, check.names = FALSE)
  companion$denominator[[1]] <- 999L
  utils::write.csv(companion, path, row.names = FALSE, na = "")
  db_report_refresh_checksum(root, "plot_data_001_frequency")
  before <- db_report_bytes(root)

  error <- tryCatch(epi_eda_render_db_report(root), error = identity)
  expect_match(conditionMessage(error), "frequency companion is inconsistent")
  expect_false(grepl("999", conditionMessage(error), fixed = TRUE))
  expect_identical(db_report_bytes(root), before)
  expect_false(file.exists(file.path(root, "reports", "eda-report.html")))
})
