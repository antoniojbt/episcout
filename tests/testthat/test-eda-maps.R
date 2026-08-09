context("integrated data-frame point maps")

library(testthat)
library(episcout)

eda_map_fixture <- function(n = 3L) {
  data <- data.frame(
    lon = seq(-10, 10, length.out = n),
    lat = seq(-5, 5, length.out = n),
    numeric_theme = seq_len(n),
    category_theme = factor(
      rep(c("A", "B", "MISSING"), length.out = n),
      levels = c("A", "B", "MISSING")
    ),
    text_theme = rep(c("one", "two", "MISSING"), length.out = n),
    observed_date = as.Date("2024-01-01") + seq_len(n) - 1L,
    stringsAsFactors = FALSE
  )
  spec <- epi_eda_spec_scaffold(data)
  spec$geo_role[spec$name %in% c("lon", "lat")] <- c("x", "y")
  spec$geo_pair[spec$name %in% c("lon", "lat")] <- "site"
  spec$geo_crs[spec$name %in% c("lon", "lat")] <- "EPSG:4326"
  spec$missing_codes[spec$name %in% c("category_theme", "text_theme")] <- "MISSING"
  list(data = data, spec = spec)
}

test_that("all public runners expose consistent map controls", {
  runners <- list(
    epi_eda_run,
    epi_eda_intake_run,
    epi_eda_db_run,
    epi_eda_render_report
  )
  for (runner in runners) {
    observed <- formals(runner)
    expect_true(all(c("maps", "map_vars", "max_map_points") %in% names(observed)))
    expect_identical(observed$maps, FALSE)
    expect_identical(observed$map_vars, quote(character()))
    expect_identical(observed$max_map_points, 10000L)
  }
})

test_that("maps disabled returns exact empty components and writes no map directory", {
  fixture <- eda_map_fixture()
  output_dir <- tempfile("eda-maps-disabled-")
  dir.create(output_dir)

  observed <- epi_eda_run(
    fixture$data, fixture$spec, output_dir = output_dir
  )

  expect_identical(observed$maps, stats::setNames(vector("list", 0L), character()))
  expect_identical(
    observed$map_inventory,
    getFromNamespace("eda_map_empty_inventory", "episcout")()
  )
  expect_false(dir.exists(file.path(output_dir, "maps")))
  expect_error(
    epi_eda_run(fixture$data, fixture$spec, map_vars = "numeric_theme"),
    "requires maps = TRUE"
  )
})

test_that("map selectors and bounds fail before profiling", {
  fixture <- eda_map_fixture()
  invalid <- list(
    list(map_vars = c("numeric_theme", "numeric_theme"), message = "unique"),
    list(map_vars = "absent", message = "not declared"),
    list(map_vars = "observed_date", message = "supports only"),
    list(map_vars = NA_character_, message = "non-empty"),
    list(map_vars = 1, message = "character vector")
  )
  for (case in invalid) {
    expect_error(
      epi_eda_run(
        fixture$data, fixture$spec, maps = TRUE,
        map_vars = case$map_vars
      ),
      case$message
    )
  }
  for (limit in list(0L, -1L, 1.5, Inf, NA_real_, .Machine$integer.max)) {
    expect_error(
      epi_eda_run(
        fixture$data, fixture$spec, maps = TRUE,
        max_map_points = limit
      ),
      "positive whole number below"
    )
  }
})

test_that("ready pairs produce deterministic geometry and thematic cross-products", {
  fixture <- eda_map_fixture()
  second <- fixture$spec[fixture$spec$name %in% c("lon", "lat"), ]
  second$name <- c("east", "north")
  second$label <- second$name
  second$geo_pair <- "projected"
  second$geo_crs <- "EPSG:3857"
  spec <- rbind(fixture$spec, second)
  data <- transform(fixture$data, east = c(1, 2, 3), north = c(4, 5, 6))
  original <- data

  observed <- epi_eda_run(
    data,
    spec,
    maps = TRUE,
    map_vars = c("numeric_theme", "category_theme", "text_theme")
  )

  expected_ids <- c(
    "map-p001-geometry", "map-p001-v003", "map-p001-v004", "map-p001-v005",
    "map-p002-geometry", "map-p002-v003", "map-p002-v004", "map-p002-v005"
  )
  expect_identical(observed$map_inventory$map_id, expected_ids)
  expect_identical(names(observed$maps), expected_ids)
  expect_true(all(observed$map_inventory$status == "created"))
  expect_true(all(observed$map_inventory$n_source_rows == 3L))
  expect_true(all(observed$map_inventory$n_mapped == 3L))
  expect_identical(
    observed$map_inventory$path,
    paste0("maps/", expected_ids, ".svg")
  )
  expect_identical(data, original)

  category <- observed$maps[["map-p001-v004"]]$data$category_theme
  text <- observed$maps[["map-p001-v005"]]$data$text_theme
  expect_equal(sum(is.na(category)), 1L)
  expect_equal(sum(is.na(text)), 1L)
  expect_setequal(as.character(stats::na.omit(category)), c("A", "B"))
  expect_setequal(stats::na.omit(text), c("one", "two"))
})

test_that("failed QC and point limits skip whole pairs without partial maps", {
  fixture <- eda_map_fixture()
  failed <- fixture$data
  failed$lon[[1]] <- NA_real_
  observed <- epi_eda_run(
    failed, fixture$spec, maps = TRUE, map_vars = "numeric_theme"
  )

  expect_length(observed$maps, 0L)
  expect_true(all(observed$map_inventory$status == "not_created"))
  expect_true(all(observed$map_inventory$reason == "incomplete_pairs"))
  expect_true(all(observed$map_inventory$n_mapped == 0L))
  expect_true(all(observed$map_inventory$path == ""))
  expect_true(all(observed$summaries$variables$status == "summarised"))

  zero <- eda_map_fixture(0L)
  zero_result <- epi_eda_run(zero$data, zero$spec, maps = TRUE)
  expect_identical(zero_result$map_inventory$reason, "no_rows")
  expect_identical(zero_result$map_inventory$n_mapped, 0L)

  exact <- eda_map_fixture(10000L)
  exact_result <- epi_eda_run(
    exact$data, exact$spec, maps = TRUE, max_map_points = 10000L
  )
  expect_identical(exact_result$map_inventory$status, "created")
  expect_identical(exact_result$map_inventory$n_mapped, 10000L)

  over <- eda_map_fixture(10001L)
  over_result <- epi_eda_run(
    over$data, over$spec, maps = TRUE, max_map_points = 10000L
  )
  expect_length(over_result$maps, 0L)
  expect_identical(over_result$map_inventory$status, "not_created")
  expect_identical(over_result$map_inventory$reason, "max_map_points_exceeded")
  expect_identical(over_result$map_inventory$n_mapped, 0L)
})

test_that("created SVG paths are deterministic and no skipped file is written", {
  fixture <- eda_map_fixture()
  output_dir <- tempfile("eda-maps-created-")
  dir.create(output_dir)
  observed <- epi_eda_run(
    fixture$data,
    fixture$spec,
    output_dir = output_dir,
    maps = TRUE,
    map_vars = c("numeric_theme", "category_theme")
  )

  expect_true(all(file.exists(file.path(output_dir, observed$map_inventory$path))))
  expect_identical(
    sort(list.files(file.path(output_dir, "maps"))),
    sort(paste0(observed$map_inventory$map_id, ".svg"))
  )

  skipped_dir <- tempfile("eda-maps-skipped-")
  dir.create(skipped_dir)
  skipped <- epi_eda_run(
    fixture$data, fixture$spec, output_dir = skipped_dir,
    maps = TRUE, max_map_points = 2L
  )
  expect_identical(skipped$map_inventory$status, "not_created")
  expect_false(dir.exists(file.path(skipped_dir, "maps")))
})
