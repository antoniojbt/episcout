skip_if_not_installed("sf")

geo_points <- function() {
  sf::st_sf(
    id = c("alpha", "beta", "gamma"),
    score = c(1, NA, 3),
    group = factor(c("north", "south", "north")),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 1)),
      crs = 4326
    )
  )
}

geo_polygons <- function() {
  first <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  second <- matrix(c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0), ncol = 2, byrow = TRUE)
  sf::st_sf(
    area = c("west", NA),
    rate = c(2.5, 5),
    geometry = sf::st_sfc(sf::st_polygon(list(first)), sf::st_polygon(list(second)), crs = 4326)
  )
}

geo_temp_path <- function(extension = ".gpkg") {
  tempfile(pattern = "episcout-geo-test-", fileext = extension)
}

test_that("all geo entry points retain an optional sf boundary", {
  functions <- c(
    "epi_geo_read", "epi_geo_from_coords", "epi_geo_describe",
    "epi_geo_transform", "epi_geo_map", "epi_geo_write"
  )
  boundaries <- vapply(functions, function(name) {
    source <- paste(deparse(body(get(name, envir = asNamespace("episcout")))), collapse = "\n")
    grepl("epi_geo_require\\(\"sf\"\\)", source)
  }, logical(1))
  expect_true(all(boundaries))

  testthat::local_mocked_bindings(
    epi_geo_namespace_available = function(package) FALSE,
    .package = "episcout"
  )
  expect_error(
    epi_geo_from_coords(data.frame(x = 0, y = 0), "x", "y", 4326),
    "sf package is required"
  )
})

test_that("reviewed coordinate conversion is ordered and all-or-nothing", {
  input <- data.frame(
    record = c("first", "second"),
    `east west` = c(-1, 1),
    north = c(2, 3),
    check.names = FALSE
  )
  result <- epi_geo_from_coords(input, "east west", "north", 4326)
  expect_named(result, c("data", "audit", "metadata"))
  expect_s3_class(result$data, "sf")
  expect_identical(result$data$record, input$record)
  expect_named(result$data, c("record", "east west", "north", "geometry"))
  expect_equal(sf::st_coordinates(result$data), cbind(X = c(-1, 1), Y = c(2, 3)))
  expect_identical(result$audit$rows, 2L)
  expect_identical(result$audit$complete_pairs, 2L)
  expect_true(result$audit$eligible)
  expect_identical(result$metadata$crs_epsg, 4326L)

  removed <- epi_geo_from_coords(input, "east west", "north", "EPSG:4326", remove = TRUE)
  expect_named(removed$data, c("record", "geometry"))
  expect_true(removed$metadata$source_columns_removed)

  blocked <- epi_geo_from_coords(
    data.frame(
      x = c(NA, 0, NA, NaN, 1, 181, 0, 0),
      y = c(0, NA, NA, 1, Inf, 0, 91, 0)
    ),
    "x",
    "y",
    4326
  )
  expect_null(blocked$data)
  expect_false(blocked$audit$eligible)
  expect_identical(blocked$audit$rows, 8L)
  expect_identical(blocked$audit$complete_pairs, 3L)
  expect_identical(blocked$audit$missing_x, 1L)
  expect_identical(blocked$audit$missing_y, 1L)
  expect_identical(blocked$audit$both_missing, 1L)
  expect_identical(blocked$audit$non_finite, 2L)
  expect_identical(blocked$audit$range_failures, 2L)

  boundaries <- epi_geo_from_coords(
    data.frame(x = c(-180, 180), y = c(-90, 90)),
    "x",
    "y",
    4326
  )
  expect_true(boundaries$audit$eligible)
  expect_equal(sf::st_coordinates(boundaries$data), cbind(X = c(-180, 180), Y = c(-90, 90)))

  projected <- epi_geo_from_coords(data.frame(x = 1000, y = 1000), "x", "y", 3857)
  expect_true(projected$audit$eligible)

  empty <- epi_geo_from_coords(data.frame(x = numeric(), y = numeric()), "x", "y", 4326)
  expect_s3_class(empty$data, "sf")
  expect_equal(nrow(empty$data), 0L)
  expect_identical(sf::st_crs(empty$data)$epsg, 4326L)
})

test_that("coordinate conversion rejects ambiguous inputs without values in conditions", {
  duplicate <- structure(data.frame(a = 1, b = 2), names = c("same", "same"))
  expect_error(epi_geo_from_coords(duplicate, "same", "same", 4326), "unique")
  expect_error(epi_geo_from_coords(data.frame(x = 1, y = 2), "x", "x", 4326), "distinct")
  expect_error(epi_geo_from_coords(data.frame(x = 1, y = 2), "x", "absent", 4326), "existing")
  expect_error(epi_geo_from_coords(data.frame(x = "secret", y = 2), "x", "y", 4326), "must be numeric")
  expect_error(epi_geo_from_coords(data.frame(x = 1, y = 2), "x", "y", "not-a-crs"), "could not be resolved")
  expect_error(epi_geo_from_coords(data.frame(x = 1, y = 2), "x", "y", 4326.5), "one EPSG integer")
  expect_error(epi_geo_from_coords(data.frame(x = 1, y = 2), "x", "y", NA), "one EPSG integer")
  expect_error(epi_geo_from_coords(data.frame(x = 1, y = 2), "x", "y", 4326, remove = NA), "TRUE or FALSE")
  expect_error(epi_geo_from_coords(1:3, "x", "y", 4326), "data frame")

  message <- tryCatch(
    epi_geo_from_coords(data.frame(x = "private-place", y = 2), "x", "y", 4326),
    error = conditionMessage
  )
  expect_false(grepl("private-place", message, fixed = TRUE))
})

test_that("aggregate description reconciles hand-authored geometry", {
  geometry <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(),
    sf::st_point(c(2, 1)),
    crs = 4326
  )
  object <- sf::st_sf(label = c("one", "empty", "two"), geometry = geometry)
  description <- epi_geo_describe(object)
  expect_named(description, c("dataset", "geometry_types", "validity", "messages"))
  expect_identical(description$dataset$features, 3L)
  expect_identical(description$dataset$attributes, 1L)
  expect_identical(description$dataset$geometry_column, "geometry")
  expect_identical(description$dataset$dimension, "XY")
  expect_identical(description$dataset$crs_epsg, 4326L)
  expect_true(description$dataset$geographic)
  expect_equal(description$dataset$bounding_box, c(xmin = 0, ymin = 0, xmax = 2, ymax = 1))
  expect_equal(
    description$geometry_types,
    data.frame(
      geometry_type = c("EMPTY", "POINT"),
      count = c(1L, 2L),
      proportion = c(1 / 3, 2 / 3),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(description$validity$valid, 2L)
  expect_identical(description$validity$empty, 1L)
  expect_match(description$messages, "empty geometry")
  expect_true(all(c("GEOS", "GDAL", "PROJ") %in% names(description$dataset$external_libraries)))

  all_empty <- epi_geo_describe(object[c(2L, 2L), ])
  expect_true(all(is.na(all_empty$dataset$bounding_box)))
  expect_equal(
    all_empty$geometry_types,
    data.frame(geometry_type = "EMPTY", count = 2L, proportion = 1, stringsAsFactors = FALSE)
  )
  expect_identical(all_empty$validity$empty, 2L)

  bowtie <- matrix(c(0, 0, 1, 1, 0, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE)
  invalid <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(bowtie)), crs = 3857))
  invalid_description <- epi_geo_describe(invalid)
  expect_identical(invalid_description$validity$invalid, 1L)
  expect_match(invalid_description$messages, "invalid geometry")
})

test_that("all supported XY vector families retain aggregate identity", {
  line <- matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)
  ring <- matrix(c(0, 0, 1, 0, 1, 1, 0, 0), ncol = 2, byrow = TRUE)
  geometry <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_multipoint(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(line),
    sf::st_multilinestring(list(line)),
    sf::st_polygon(list(ring)),
    sf::st_multipolygon(list(list(ring))),
    crs = 3857
  )
  object <- sf::st_sf(sequence = seq_along(geometry), geometry = geometry)
  description <- epi_geo_describe(object)
  expect_setequal(
    description$geometry_types$geometry_type,
    c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
  )
  expect_true(all(description$geometry_types$count == 1L))
  expect_identical(description$validity$valid, 6L)
  expect_match(description$messages, "mixed geometry types")

  gpkg <- geo_temp_path()
  on.exit(unlink(gpkg), add = TRUE)
  epi_geo_write(object, gpkg, "all_families")
  reread <- epi_geo_read(gpkg, "all_families")
  expect_identical(reread$sequence, object$sequence)
  expect_setequal(epi_geo_describe(reread)$geometry_types$geometry_type, description$geometry_types$geometry_type)
})

test_that("typed zero-feature layers remain usable", {
  empty_geometry <- sf::st_sfc(sf::st_point(), crs = 4326)[FALSE]
  empty <- sf::st_sf(label = character(), geometry = empty_geometry)
  description <- epi_geo_describe(empty)
  expect_identical(description$dataset$features, 0L)
  expect_true(all(is.na(description$dataset$bounding_box)))
  expect_match(description$messages, "no features")

  gpkg <- geo_temp_path()
  on.exit(unlink(gpkg), add = TRUE)
  epi_geo_write(empty, gpkg, "empty_points")
  reread <- epi_geo_read(gpkg, "empty_points")
  expect_equal(nrow(reread), 0L)
  expect_identical(class(sf::st_geometry(reread))[[1L]], "sfc_POINT")
  expect_identical(sf::st_crs(reread)$epsg, 4326L)
})

test_that("unsupported spatial structures fail before downstream work", {
  missing_crs <- sf::st_set_crs(geo_points(), NA)
  z_geometry <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(0, 0, 1), dim = "XYZ"), crs = 4979)
  )
  collection <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_geometrycollection(list(sf::st_point(c(0, 0)))),
      crs = 4326
    )
  )
  multiple <- geo_points()
  multiple$second_geometry <- sf::st_geometry(multiple)

  expect_error(epi_geo_describe(missing_crs), "explicit coordinate reference")
  expect_error(epi_geo_describe(data.frame(x = 1)), "sf object")
  expect_error(epi_geo_describe(z_geometry), "Only XY")
  expect_error(epi_geo_describe(collection), "unsupported geometry")
  expect_error(epi_geo_describe(multiple), "exactly one active geometry")

  duplicate_names <- geo_points()
  names(duplicate_names)[1:2] <- "duplicate"
  expect_error(epi_geo_describe(duplicate_names), "unique column names")
})

test_that("transformation matches an independent EPSG control", {
  point <- sf::st_sf(
    name = "neutral-control",
    geometry = sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326)
  )
  transformed <- epi_geo_transform(point, 3857)
  expect_identical(transformed$name, point$name)
  expect_identical(names(transformed), names(point))
  expect_identical(sf::st_crs(transformed)$epsg, 3857L)
  expect_equal(
    as.numeric(sf::st_coordinates(transformed)[1, ]),
    c(111319.490793, 222684.208506),
    tolerance = 1e-6
  )

  identity <- epi_geo_transform(point, 4326)
  expect_equal(sf::st_coordinates(identity), sf::st_coordinates(point))
  empty <- epi_geo_transform(point[FALSE, ], 3857)
  expect_equal(nrow(empty), 0L)
  expect_identical(sf::st_crs(empty)$epsg, 3857L)
  expect_error(epi_geo_transform(sf::st_set_crs(point, NA), 3857), "explicit coordinate")
  expect_error(epi_geo_transform(point, "not-a-crs"), "could not be resolved")

  unprojectable <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(0, 100)), crs = 4326)
  )
  expect_error(epi_geo_transform(unprojectable, 3857), "did not preserve")

  testthat::with_mocked_bindings(
    expect_error(epi_geo_transform(point, 3857), "unavailable"),
    epi_geo_can_transform = function(source, target) FALSE,
    .package = "episcout"
  )

  testthat::with_mocked_bindings(
    expect_error(epi_geo_transform(point, 3857), "could not be completed"),
    epi_geo_transform_geometry = function(x, target) stop("forced"),
    .package = "episcout"
  )
})

test_that("static maps use geometry-aware aesthetics and remain extensible", {
  skip_if_not_installed("ggplot2")
  points <- geo_points()
  point_map <- epi_geo_map(points, value = "group", na_colour = "orange")
  expect_s3_class(point_map, "ggplot")
  expect_s3_class(point_map$coordinates, "CoordSf")
  expect_identical(point_map$scales$get_scales("colour")$na.value, "orange")
  point_build <- ggplot2::ggplot_build(point_map)
  expect_equal(nrow(point_build$data[[1L]]), 3L)

  lines <- sf::st_sf(
    kind = c("first", "second"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 1, 1, 0), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )
  line_map <- epi_geo_map(lines, value = "kind")
  expect_identical(line_map$scales$get_scales("colour")$aesthetics, "colour")
  expect_equal(nrow(ggplot2::ggplot_build(line_map)$data[[1L]]), 2L)

  polygons <- geo_polygons()
  polygon_map <- epi_geo_map(polygons, value = "rate", geometry_colour = "white")
  expect_s3_class(polygon_map, "ggplot")
  expect_identical(polygon_map$scales$get_scales("fill")$na.value, "grey80")
  expect_equal(nrow(ggplot2::ggplot_build(polygon_map)$data[[1L]]), 2L)
  expect_s3_class(polygon_map + ggplot2::theme_minimal(), "ggplot")

  geometry_only <- epi_geo_map(polygons, geometry_fill = "grey70")
  expect_identical(geometry_only$layers[[1L]]$aes_params$fill, "grey70")
  point_only <- epi_geo_map(points, geometry_fill = "white")
  expect_identical(point_only$layers[[1L]]$aes_params$colour, "grey30")
  expect_identical(point_only$layers[[1L]]$aes_params$fill, "white")
  polygon_groups <- epi_geo_map(polygons, value = "area")
  expect_identical(polygon_groups$scales$get_scales("fill")$aesthetics, "fill")
  point_scores <- epi_geo_map(points, value = "score", geometry_fill = "white")
  expect_identical(point_scores$scales$get_scales("colour")$aesthetics, "colour")
  expect_identical(point_scores$layers[[1L]]$aes_params$fill, "white")
  expect_error(epi_geo_map(polygons, value = "rate", geometry_fill = "red"), "cannot be set")
  expect_error(epi_geo_map(points, value = "group", geometry_colour = "red"), "cannot be set")
  expect_error(epi_geo_map(points, value = "id"), NA)
  expect_error(epi_geo_map(points, value = "geometry"), "non-geometry")
  expect_error(epi_geo_map(points, geometry_colour = ""), "non-empty character")
  dates <- points
  dates$when <- as.Date("2026-01-01") + 0:2
  expect_error(epi_geo_map(dates, value = "when"), "numeric or categorical")

  mixed <- sf::st_sf(
    value = 1:2,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  expect_error(epi_geo_map(mixed), "Mixed polygonal")
})

test_that("file reading selects reviewed GeoPackage and Shapefile layers", {
  points <- geo_points()
  gpkg <- geo_temp_path()
  on.exit(unlink(gpkg), add = TRUE)
  sf::st_write(points, gpkg, layer = "points", quiet = TRUE)
  read_points <- epi_geo_read(gpkg)
  expect_identical(read_points$id, points$id)
  expect_equal(sf::st_coordinates(read_points), sf::st_coordinates(points))

  sf::st_write(data.frame(note = "non-spatial"), gpkg, layer = "notes", append = NA, quiet = TRUE)
  expect_identical(epi_geo_read(gpkg)$id, points$id)

  sf::st_write(geo_polygons(), gpkg, layer = "areas", append = NA, quiet = TRUE)
  expect_error(epi_geo_read(gpkg), "multiple spatial layers")
  expect_s3_class(epi_geo_read(gpkg, layer = "areas"), "sf")
  expect_error(epi_geo_read(gpkg, layer = "absent"), "readable spatial layer")

  shapefile <- geo_temp_path(".shp")
  shapefile_stem <- tools::file_path_sans_ext(shapefile)
  on.exit(unlink(paste0(shapefile_stem, ".*")), add = TRUE)
  sf::st_write(points, shapefile, quiet = TRUE)
  expect_identical(epi_geo_read(shapefile)$id, points$id)
  shapefile_layer <- tools::file_path_sans_ext(basename(shapefile))
  expect_identical(epi_geo_read(shapefile, shapefile_layer)$id, points$id)
  expect_error(epi_geo_read(shapefile, "absent"), "readable spatial layer")
  unlink(paste0(shapefile_stem, ".shx"))
  expect_error(epi_geo_read(shapefile), "sidecars")

  expect_error(epi_geo_read("https://example.test/private.gpkg"), "local file path")
  expect_error(epi_geo_read("/vsizip/private.gpkg"), "local file path")
  expect_error(epi_geo_read(geo_temp_path(".geojson")), "unsupported file extension")
  expect_error(epi_geo_read(geo_temp_path()), "existing file")
})

test_that("read failures remain deterministic for non-spatial or unreadable files", {
  plain <- geo_temp_path()
  on.exit(unlink(plain), add = TRUE)
  sf::st_write(data.frame(value = 1:2), plain, layer = "plain", quiet = TRUE)
  expect_error(epi_geo_read(plain), "no readable spatial layer")

  unreadable <- geo_temp_path()
  on.exit(unlink(unreadable), add = TRUE)
  writeLines("not a GeoPackage", unreadable)
  testthat::with_mocked_bindings(
    expect_error(epi_geo_read(unreadable), "could not be inspected"),
    epi_geo_inspect_layers = function(dsn) stop("forced inspection failure"),
    .package = "episcout"
  )

  testthat::with_mocked_bindings(
    expect_error(epi_geo_read(unreadable, "fake"), "could not be read"),
    epi_geo_layer_inventory = function(dsn) {
      list(
        names = "fake",
        inventory = structure(
          data.frame(name = "fake", stringsAsFactors = FALSE),
          geomtype = list("Point")
        )
      )
    },
    epi_geo_read_layer = function(dsn, layer) stop("forced read failure"),
    .package = "episcout"
  )
})

test_that("read refuses missing CRS and unsupported geometry", {
  no_crs <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))))
  gpkg <- geo_temp_path()
  on.exit(unlink(gpkg), add = TRUE)
  suppressWarnings(sf::st_write(no_crs, gpkg, layer = "unknown", quiet = TRUE))
  expect_error(epi_geo_read(gpkg), "explicit coordinate reference")

  collection <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_geometrycollection(list(sf::st_point(c(0, 0)))),
      crs = 4326
    )
  )
  collection_file <- geo_temp_path()
  on.exit(unlink(collection_file), add = TRUE)
  sf::st_write(collection, collection_file, layer = "collection", quiet = TRUE)
  expect_error(epi_geo_read(collection_file), "unsupported geometry")

  generic <- sf::st_sf(label = character(), geometry = sf::st_sfc(crs = 4326))
  generic_file <- geo_temp_path()
  on.exit(unlink(generic_file), add = TRUE)
  sf::st_write(generic, generic_file, layer = "generic", quiet = TRUE)
  expect_error(epi_geo_read(generic_file), "unsupported geometry")
})

test_that("GeoPackage writing refuses collisions and preserves unrelated layers", {
  points <- geo_points()
  gpkg <- geo_temp_path()
  on.exit(unlink(gpkg), add = TRUE)
  expect_identical(epi_geo_write(points, gpkg, "points"), gpkg)
  expect_error(epi_geo_write(points, gpkg, "points"), "already exists")

  other <- sf::st_sf(
    label = "retained",
    geometry = sf::st_sfc(sf::st_point(c(10, 10)), crs = 4326)
  )
  epi_geo_write(other, gpkg, "other")
  updated <- points[1:2, ]
  updated$id <- c("replacement-one", "replacement-two")
  epi_geo_write(updated, gpkg, "points", overwrite = TRUE)

  expect_setequal(sf::st_layers(gpkg)$name, c("points", "other"))
  expect_identical(epi_geo_read(gpkg, "points")$id, updated$id)
  expect_identical(epi_geo_read(gpkg, "other")$label, "retained")
  expect_identical(as.character(epi_geo_read(gpkg, "points")$group), as.character(updated$group))
  expect_false(any(grepl("^\\.episcout-gpkg-", list.files(dirname(gpkg), all.files = TRUE))))
})

test_that("staging failures preserve the previous GeoPackage", {
  gpkg <- geo_temp_path()
  on.exit(unlink(gpkg), add = TRUE)
  points <- geo_points()
  epi_geo_write(points, gpkg, "points")
  before <- unname(tools::md5sum(gpkg))

  testthat::local_mocked_bindings(
    epi_geo_write_stage = function(...) stop("private-value-should-not-escape"),
    .package = "episcout"
  )
  expect_error(
    epi_geo_write(points[1:2, ], gpkg, "points", overwrite = TRUE),
    "could not be written to staging"
  )
  expect_identical(unname(tools::md5sum(gpkg)), before)
  expect_false(any(grepl("^\\.episcout-gpkg-stage-", list.files(dirname(gpkg), all.files = TRUE))))
})

test_that("reconciliation and publication failures leave destination unchanged", {
  gpkg <- geo_temp_path()
  on.exit(unlink(gpkg), add = TRUE)
  points <- geo_points()
  epi_geo_write(points, gpkg, "points")
  before <- unname(tools::md5sum(gpkg))

  testthat::with_mocked_bindings(
    expect_error(
      epi_geo_write(points[1:2, ], gpkg, "points", overwrite = TRUE),
      "did not reconcile"
    ),
    epi_geo_reconciles = function(expected, actual) FALSE,
    .package = "episcout"
  )
  expect_identical(unname(tools::md5sum(gpkg)), before)

  testthat::with_mocked_bindings(
    expect_error(
      epi_geo_write(points[1:2, ], gpkg, "points", overwrite = TRUE),
      "forced publication failure"
    ),
    epi_geo_publish_stage = function(stage, dsn) stop("forced publication failure"),
    .package = "episcout"
  )
  expect_identical(unname(tools::md5sum(gpkg)), before)
})

test_that("GeoPackage writing rejects unsafe targets and attribute classes", {
  points <- geo_points()
  expect_error(epi_geo_write(points, geo_temp_path(".shp"), "points"), "unsupported file extension")
  expect_error(epi_geo_write(points, file.path(geo_temp_path(), "out.gpkg"), "points"), "directory must already exist")
  expect_error(epi_geo_write(points, geo_temp_path(), "points", overwrite = NA), "TRUE or FALSE")

  target <- geo_temp_path()
  symlink <- geo_temp_path()
  on.exit(unlink(c(target, symlink)), add = TRUE)
  writeLines("target", target)
  if (file.symlink(target, symlink)) {
    expect_error(epi_geo_write(points, symlink, "points"), "symbolic link")
  }

  unsupported <- points
  unsupported$nested <- I(list(list(a = 1), list(a = 2), list(a = 3)))
  expect_error(epi_geo_write(unsupported, geo_temp_path(), "points"), "unsupported by the GeoPackage contract")

  classed_numeric <- points
  classed_numeric$score <- structure(classed_numeric$score, class = "reviewed_measure")
  expect_error(epi_geo_write(classed_numeric, geo_temp_path(), "points"), "unsupported by the GeoPackage contract")

  supported <- points
  supported$logical_value <- c(TRUE, FALSE, NA)
  supported$integer_value <- 1:3
  supported$date_value <- as.Date("2026-01-01") + 0:2
  supported$datetime_value <- as.POSIXct("2026-01-01", tz = "UTC") + 0:2
  supported_file <- geo_temp_path()
  on.exit(unlink(supported_file), add = TRUE)
  expect_identical(epi_geo_write(supported, supported_file, "supported"), supported_file)
})

test_that("reconciliation rejects each structural mismatch", {
  expected <- geo_points()
  expect_false(episcout:::epi_geo_reconciles(expected, expected[1:2, ]))

  renamed <- expected
  names(renamed)[[1L]] <- "renamed"
  expect_false(episcout:::epi_geo_reconciles(expected, renamed))

  regrouped <- expected
  regrouped$score <- as.integer(regrouped$score)
  expect_false(episcout:::epi_geo_reconciles(expected, regrouped))

  changed_crs <- suppressWarnings(sf::st_set_crs(expected, 3857))
  expect_false(episcout:::epi_geo_reconciles(expected, changed_crs))

  changed_type <- expected
  sf::st_geometry(changed_type) <- sf::st_cast(sf::st_geometry(changed_type), "MULTIPOINT")
  expect_false(episcout:::epi_geo_reconciles(expected, changed_type))

  moved <- expected
  sf::st_geometry(moved) <- sf::st_geometry(moved) + c(1, 0)
  expect_false(episcout:::epi_geo_reconciles(expected, moved))
})

test_that("publication recovery branches report deterministic conditions", {
  stage <- geo_temp_path()
  destination <- geo_temp_path()
  on.exit(unlink(c(stage, destination)), add = TRUE)
  writeLines("stage", stage)

  testthat::with_mocked_bindings(
    expect_error(episcout:::epi_geo_publish_stage(stage, destination), "could not be published"),
    epi_geo_file_rename = function(from, to) FALSE,
    .package = "episcout"
  )

  writeLines("destination", destination)
  testthat::with_mocked_bindings(
    expect_error(episcout:::epi_geo_publish_stage(stage, destination), "could not be secured"),
    epi_geo_file_rename = function(from, to) FALSE,
    .package = "episcout"
  )

  rename_state <- new.env(parent = emptyenv())
  rename_state$calls <- 0L
  testthat::with_mocked_bindings(
    expect_error(episcout:::epi_geo_publish_stage(stage, destination), "was restored"),
    epi_geo_file_rename = function(from, to) {
      rename_state$calls <- rename_state$calls + 1L
      rename_state$calls != 2L
    },
    .package = "episcout"
  )

  rename_state$calls <- 0L
  testthat::with_mocked_bindings(
    expect_error(episcout:::epi_geo_publish_stage(stage, destination), "recovery backup"),
    epi_geo_file_rename = function(from, to) {
      rename_state$calls <- rename_state$calls + 1L
      rename_state$calls == 1L
    },
    .package = "episcout"
  )

  testthat::with_mocked_bindings(
    expect_warning(episcout:::epi_geo_publish_stage(stage, destination), "backup could not be removed"),
    epi_geo_file_rename = function(from, to) TRUE,
    epi_geo_file_remove = function(path) 1L,
    .package = "episcout"
  )
})

test_that("a staging-copy failure leaves the existing file unchanged", {
  gpkg <- geo_temp_path()
  on.exit(unlink(gpkg), add = TRUE)
  points <- geo_points()
  epi_geo_write(points, gpkg, "points")
  before <- unname(tools::md5sum(gpkg))
  testthat::with_mocked_bindings(
    expect_error(epi_geo_write(points, gpkg, "new_layer"), "could not be copied"),
    epi_geo_file_copy = function(from, to) FALSE,
    .package = "episcout"
  )
  expect_identical(unname(tools::md5sum(gpkg)), before)
})
