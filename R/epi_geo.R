epi_geo_namespace_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

epi_geo_require <- function(package) {
  if (!epi_geo_namespace_available(package)) {
    stop(
      "The ", package, " package is required for episcout geospatial functions. Install it before continuing.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

epi_geo_scalar_character <- function(x, name, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(name, " must be one non-empty character value.", call. = FALSE)
  }
  x
}

epi_geo_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

epi_geo_crs <- function(crs) {
  valid_numeric <- is.numeric(crs) && length(crs) == 1L && !is.na(crs) &&
    is.finite(crs) && crs >= 1 && crs <= .Machine$integer.max && crs == floor(crs)
  valid_character <- is.character(crs) && length(crs) == 1L &&
    !is.na(crs) && nzchar(crs)
  if (!valid_numeric && !valid_character) {
    stop("crs must be one EPSG integer or one non-empty CRS character value.", call. = FALSE)
  }
  resolved <- tryCatch(
    suppressMessages(suppressWarnings(sf::st_crs(crs))),
    error = function(error) NULL
  )
  if (is.null(resolved) || is.na(resolved)) {
    stop("crs could not be resolved by sf.", call. = FALSE)
  }
  resolved
}

epi_geo_geometry_types <- function(x) {
  geometry <- sf::st_geometry(x)
  if (length(geometry) == 0L) {
    container <- grep("^sfc_", class(geometry), value = TRUE)
    return(sub("^sfc_", "", container[[1L]]))
  }
  as.character(sf::st_geometry_type(x, by_geometry = TRUE))
}

epi_geo_crs_is_missing <- function(crs) {
  is.na(crs) || (
    is.character(crs$input) && length(crs$input) == 1L &&
      grepl("^Undefined .*unknown unit$", crs$input, ignore.case = TRUE)
  )
}

epi_geo_validate_sf <- function(x) {
  if (!inherits(x, "sf")) {
    stop("x must be an sf object.", call. = FALSE)
  }
  geometry_columns <- names(x)[vapply(x, inherits, logical(1), what = "sfc")]
  if (length(geometry_columns) != 1L || !identical(attr(x, "sf_column"), geometry_columns[[1L]])) {
    stop("x must contain exactly one active geometry column.", call. = FALSE)
  }
  if (anyDuplicated(names(x))) {
    stop("x must have unique column names.", call. = FALSE)
  }
  if (epi_geo_crs_is_missing(sf::st_crs(x))) {
    stop("x must have an explicit coordinate reference system.", call. = FALSE)
  }
  geometry <- sf::st_geometry(x)
  dimensions <- unique(vapply(geometry, function(item) class(item)[[1L]], character(1)))
  if (length(dimensions) > 0L && any(dimensions != "XY")) {
    stop("Only XY geometry is supported in this phase.", call. = FALSE)
  }
  types <- epi_geo_geometry_types(x)
  supported <- c(
    "POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING",
    "POLYGON", "MULTIPOLYGON"
  )
  if (length(types) > 0L && any(!types %in% supported)) {
    stop("x contains an unsupported geometry type.", call. = FALSE)
  }
  invisible(x)
}

epi_geo_validate_local_path <- function(path, extensions, must_exist) {
  path <- epi_geo_scalar_character(path, "dsn")
  if (grepl("^[[:alpha:]][[:alnum:]+.-]*://", path) || startsWith(path, "/vsi")) {
    stop("dsn must be a local file path.", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(path))
  if (!extension %in% extensions) {
    stop("dsn has an unsupported file extension.", call. = FALSE)
  }
  if (must_exist && (!file.exists(path) || dir.exists(path))) {
    stop("dsn must identify an existing file.", call. = FALSE)
  }
  path
}

epi_geo_shp_sidecars_exist <- function(dsn) {
  stem <- tolower(tools::file_path_sans_ext(basename(dsn)))
  available <- tolower(list.files(dirname(dsn), all.files = TRUE, no.. = TRUE))
  all(paste0(stem, c(".shp", ".shx", ".dbf")) %in% available)
}

epi_geo_inspect_layers <- function(dsn) {
  suppressMessages(suppressWarnings(sf::st_layers(dsn, do_count = FALSE)))
}

epi_geo_layer_inventory <- function(dsn) {
  inventory <- tryCatch(
    epi_geo_inspect_layers(dsn),
    error = function(error) NULL
  )
  if (is.null(inventory)) {
    stop("The spatial dataset could not be inspected.", call. = FALSE)
  }
  geometry_types <- as.character(inventory[["geomtype"]])
  spatial <- !is.na(geometry_types)
  list(names = as.character(inventory[["name"]])[spatial], inventory = inventory)
}

epi_geo_read_layer <- function(dsn, layer) {
  suppressMessages(suppressWarnings(sf::st_read(
    dsn,
    layer = layer,
    quiet = TRUE,
    promote_to_multi = FALSE,
    stringsAsFactors = FALSE
  )))
}

epi_geo_restore_empty_type <- function(x, inventory, layer) {
  geometry <- sf::st_geometry(x)
  container <- grep("^sfc_", class(geometry), value = TRUE)
  if (nrow(x) != 0L || !identical(container[[1L]], "sfc_GEOMETRY")) {
    return(x)
  }
  index <- match(layer, as.character(inventory[["name"]]))
  declared <- toupper(gsub("[^[:alpha:]]", "", as.character(inventory[["geomtype"]][[index]])))
  supported <- c(
    "POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING",
    "POLYGON", "MULTIPOLYGON"
  )
  if (!declared %in% supported) {
    return(x)
  }
  sf::st_geometry(x) <- sf::st_cast(geometry, declared)
  x
}

#' Read a reviewed local vector layer
#'
#' Read one spatial layer from a local GeoPackage or Shapefile into an
#' [sf::sf] object. A GeoPackage with more than one spatial layer requires an
#' explicit layer selection. Shapefile input requires its `.shp`, `.shx` and
#' `.dbf` files to remain together.
#'
#' @param dsn One existing local `.gpkg` or `.shp` path. URLs, GDAL virtual
#'   file-system paths and database connections are not accepted.
#' @param layer `NULL` for an unambiguous dataset, or one exact spatial layer
#'   name.
#'
#' @return An `sf` object with feature order, attributes, geometry and CRS
#'   retained from the selected layer.
#'
#' @details Inputs must have one active XY geometry column, a non-missing CRS
#'   and supported point, line or polygon geometry. Shapefile is an import
#'   compatibility format and may have driver-dependent field-name, encoding
#'   and geometry limitations; GeoPackage is preferred.
#'
#' @export
epi_geo_read <- function(dsn, layer = NULL) {
  epi_geo_require("sf")
  dsn <- epi_geo_validate_local_path(dsn, c("gpkg", "shp"), must_exist = TRUE)
  layer <- epi_geo_scalar_character(layer, "layer", allow_null = TRUE)
  extension <- tolower(tools::file_ext(dsn))
  if (extension == "shp" && !epi_geo_shp_sidecars_exist(dsn)) {
    stop("Shapefile input requires .shp, .shx and .dbf sidecars.", call. = FALSE)
  }
  layer_inventory <- epi_geo_layer_inventory(dsn)
  layers <- layer_inventory$names
  if (length(layers) == 0L) {
    stop("The dataset contains no readable spatial layer.", call. = FALSE)
  }
  if (is.null(layer)) {
    if (length(layers) != 1L) {
      stop("layer is required when a dataset contains multiple spatial layers.", call. = FALSE)
    }
    layer <- layers[[1L]]
  } else if (!layer %in% layers) {
    stop("layer must identify one readable spatial layer.", call. = FALSE)
  }
  result <- tryCatch(
    epi_geo_read_layer(dsn, layer),
    error = function(error) NULL
  )
  if (is.null(result) || !inherits(result, "sf")) {
    stop("The selected spatial layer could not be read.", call. = FALSE)
  }
  result <- epi_geo_restore_empty_type(result, layer_inventory$inventory, layer)
  epi_geo_validate_sf(result)
  result
}

#' Convert reviewed coordinate columns to simple features
#'
#' Audit an explicitly selected numeric x/y pair and convert the complete data
#' frame to point geometry only when every row is eligible.
#'
#' @param data A data frame with unique column names.
#' @param x,y Exact distinct names of numeric coordinate columns, in x/y or
#'   longitude/latitude order as established by the reviewed data dictionary.
#' @param crs One EPSG integer or character CRS value resolvable by [sf::st_crs()].
#' @param remove Whether to remove the source coordinate columns after a
#'   successful conversion. Defaults to `FALSE` for auditability.
#'
#' @return A list with `data`, `audit` and `metadata`. `data` is an `sf` object
#'   only when every row has a complete finite pair (and an in-range pair for
#'   EPSG:4326); otherwise it is `NULL`. The other components contain aggregate
#'   counts and reviewed metadata, never coordinate values.
#'
#' @details CRS assignment gives existing values their reviewed meaning; it is
#'   not a coordinate transformation. Missing, non-finite and out-of-range rows
#'   block all conversion and are never dropped silently.
#'
#' @export
epi_geo_from_coords <- function(data, x, y, crs, remove = FALSE) {
  epi_geo_require("sf")
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  if (anyDuplicated(names(data))) {
    stop("data must have unique column names.", call. = FALSE)
  }
  x <- epi_geo_scalar_character(x, "x")
  y <- epi_geo_scalar_character(y, "y")
  if (identical(x, y)) {
    stop("x and y must identify distinct columns.", call. = FALSE)
  }
  if (!all(c(x, y) %in% names(data))) {
    stop("x and y must identify existing columns.", call. = FALSE)
  }
  if (!is.numeric(data[[x]]) || !is.numeric(data[[y]])) {
    stop("x and y columns must be numeric.", call. = FALSE)
  }
  remove <- epi_geo_scalar_logical(remove, "remove")
  resolved_crs <- epi_geo_crs(crs)
  x_values <- data[[x]]
  y_values <- data[[y]]
  x_missing <- is.na(x_values) & !is.nan(x_values)
  y_missing <- is.na(y_values) & !is.nan(y_values)
  non_finite <- (!x_missing & !is.finite(x_values)) |
    (!y_missing & !is.finite(y_values))
  finite_pair <- is.finite(x_values) & is.finite(y_values)
  range_failure <- rep(FALSE, nrow(data))
  if (isTRUE(resolved_crs$epsg == 4326L)) {
    range_failure[finite_pair] <- x_values[finite_pair] < -180 |
      x_values[finite_pair] > 180 | y_values[finite_pair] < -90 |
      y_values[finite_pair] > 90
  }
  blocked <- x_missing | y_missing | non_finite | range_failure
  audit <- data.frame(
    rows = as.integer(nrow(data)),
    complete_pairs = as.integer(sum(finite_pair)),
    missing_x = as.integer(sum(x_missing & !y_missing)),
    missing_y = as.integer(sum(y_missing & !x_missing)),
    both_missing = as.integer(sum(x_missing & y_missing)),
    non_finite = as.integer(sum(non_finite)),
    range_failures = as.integer(sum(range_failure)),
    eligible = !any(blocked),
    stringsAsFactors = FALSE
  )
  metadata <- list(
    x = x,
    y = y,
    crs_input = resolved_crs$input,
    crs_name = resolved_crs$Name,
    crs_epsg = resolved_crs$epsg,
    source_columns_removed = remove
  )
  if (any(blocked)) {
    return(list(data = NULL, audit = audit, metadata = metadata))
  }
  converted <- suppressMessages(suppressWarnings(
    sf::st_as_sf(data, coords = c(x, y), crs = resolved_crs, remove = remove, na.fail = TRUE)
  ))
  list(data = converted, audit = audit, metadata = metadata)
}

epi_geo_bounds <- function(x) {
  geometry <- sf::st_geometry(x)
  if (length(geometry) == 0L || all(sf::st_is_empty(geometry))) {
    return(c(xmin = NA_real_, ymin = NA_real_, xmax = NA_real_, ymax = NA_real_))
  }
  bounds <- suppressWarnings(sf::st_bbox(x))
  result <- as.numeric(bounds)
  names(result) <- names(bounds)
  result
}

#' Describe aggregate spatial structure
#'
#' Summarise geometry families, validity, CRS and bounds without returning
#' feature identifiers or invalidity coordinates. This function never repairs
#' geometry.
#'
#' @param x A reviewed `sf` object supported by the phase-A spatial contract.
#'
#' @return A fixed list with `dataset`, `geometry_types`, `validity` and
#'   `messages` components.
#'
#' @details Geometry and bounding boxes are value-bearing information. A narrow
#'   bound or rare geometry/attribute combination may reveal a sensitive
#'   location; successful technical validation is not disclosure approval or
#'   evidence that the geometry is epidemiologically meaningful.
#'
#' @export
epi_geo_describe <- function(x) {
  epi_geo_require("sf")
  epi_geo_validate_sf(x)
  geometry <- sf::st_geometry(x)
  empty <- if (length(geometry) == 0L) logical() else sf::st_is_empty(geometry)
  types <- if (length(geometry) == 0L) character() else epi_geo_geometry_types(x)
  displayed_types <- ifelse(empty, "EMPTY", types)
  type_counts <- if (length(displayed_types) == 0L) integer() else table(displayed_types)
  geometry_types <- data.frame(
    geometry_type = names(type_counts),
    count = as.integer(type_counts),
    proportion = if (nrow(x) == 0L) numeric() else as.numeric(type_counts) / nrow(x),
    stringsAsFactors = FALSE
  )
  validity_values <- tryCatch(
    suppressMessages(suppressWarnings(sf::st_is_valid(geometry, NA_on_exception = TRUE))),
    error = function(error) rep(NA, length(geometry))
  )
  validity <- data.frame(
    valid = as.integer(sum(!empty & validity_values %in% TRUE)),
    invalid = as.integer(sum(!empty & validity_values %in% FALSE)),
    missing_or_exception = as.integer(sum(!empty & is.na(validity_values))),
    empty = as.integer(sum(empty)),
    stringsAsFactors = FALSE
  )
  crs <- sf::st_crs(x)
  dimensions <- unique(vapply(geometry, function(item) class(item)[[1L]], character(1)))
  dataset <- list(
    features = as.integer(nrow(x)),
    attributes = as.integer(ncol(x) - 1L),
    geometry_column = attr(x, "sf_column"),
    dimension = if (length(dimensions) == 0L) "XY" else paste(sort(dimensions), collapse = ","),
    crs_input = crs$input,
    crs_name = crs$Name,
    crs_epsg = crs$epsg,
    geographic = isTRUE(sf::st_is_longlat(x)),
    coordinate_units = crs$units_gdal,
    bounding_box = epi_geo_bounds(x),
    external_libraries = as.list(sf::sf_extSoftVersion())
  )
  messages <- character()
  if (nrow(x) == 0L) {
    messages <- c(messages, "The object contains no features.")
  }
  non_empty_types <- unique(types[!empty])
  if (length(non_empty_types) > 1L) {
    messages <- c(messages, "The object contains mixed geometry types.")
  }
  if (any(empty)) {
    messages <- c(messages, "The object contains empty geometry.")
  }
  if (any(validity_values %in% FALSE, na.rm = TRUE)) {
    messages <- c(messages, "The object contains invalid geometry.")
  }
  list(
    dataset = dataset,
    geometry_types = geometry_types,
    validity = validity,
    messages = messages
  )
}

#' Transform simple features to an explicit CRS
#'
#' Transform coordinates through the `sf`/PROJ path while retaining feature
#' and attribute order. This function never replaces CRS metadata to imitate a
#' transformation, repairs geometry or drops empty features.
#'
#' @param x A reviewed `sf` object with an explicit source CRS.
#' @param crs One EPSG integer or character target CRS resolvable by
#'   [sf::st_crs()].
#'
#' @return The input `sf` object with transformed geometry and the target CRS.
#'
#' @export
epi_geo_transform <- function(x, crs) {
  epi_geo_require("sf")
  epi_geo_validate_sf(x)
  target <- epi_geo_crs(crs)
  can_transform <- tryCatch(
    epi_geo_can_transform(sf::st_crs(x), target),
    error = function(error) FALSE
  )
  if (!isTRUE(can_transform)) {
    stop("The requested coordinate transformation is unavailable.", call. = FALSE)
  }
  result <- tryCatch(
    epi_geo_transform_geometry(x, target),
    error = function(error) NULL
  )
  if (is.null(result)) {
    stop("The requested coordinate transformation could not be completed.", call. = FALSE)
  }
  if (nrow(result) == 0L) {
    input_type <- epi_geo_geometry_types(x)
    sf::st_geometry(result) <- sf::st_cast(sf::st_geometry(result), input_type[[1L]])
  }
  epi_geo_validate_sf(result)
  input_empty <- sf::st_is_empty(sf::st_geometry(x))
  result_empty <- sf::st_is_empty(sf::st_geometry(result))
  result_coordinates <- sf::st_coordinates(sf::st_geometry(result))
  finite_coordinates <- nrow(result_coordinates) == 0L ||
    all(is.finite(result_coordinates[, c("X", "Y"), drop = FALSE]))
  structure_preserved <- nrow(result) == nrow(x) &&
    identical(names(result), names(x)) &&
    identical(epi_geo_geometry_types(result), epi_geo_geometry_types(x)) &&
    identical(result_empty, input_empty) && finite_coordinates
  if (!structure_preserved) {
    stop("The coordinate transformation did not preserve feature and attribute structure.", call. = FALSE)
  }
  result
}

epi_geo_can_transform <- function(source, target) {
  sf::st_can_transform(source, target)
}

epi_geo_transform_geometry <- function(x, target) {
  suppressMessages(suppressWarnings(sf::st_transform(x, target, partial = FALSE)))
}

epi_geo_optional_colour <- function(value, name) {
  if (is.null(value)) {
    return(NULL)
  }
  epi_geo_scalar_character(value, name)
}

epi_geo_is_polygonal <- function(types) {
  types %in% c("POLYGON", "MULTIPOLYGON")
}

#' Create a reviewed static simple-feature map
#'
#' Create a geometry-only map or map one numeric or categorical attribute with
#' the existing [ggplot2::geom_sf()] path. The returned object can be extended
#' with ordinary `ggplot2` layers, scales and themes.
#'
#' @param x A reviewed `sf` object supported by the phase-A spatial contract.
#' @param value `NULL` for geometry only, or one exact non-geometry attribute
#'   name containing numeric, character, factor or logical values.
#' @param geometry_colour,geometry_fill Optional constant geometry colours.
#'   A constant cannot be supplied for the aesthetic mapped by `value`.
#' @param na_colour One colour used for missing mapped values.
#'
#' @return An ordinary `ggplot` object containing one simple-feature layer.
#'
#' @details Polygon value maps use fill; point and line value maps use colour.
#'   Mixed polygonal and non-polygonal objects require explicit user-composed
#'   `ggplot2` layers. The function does not add a basemap, classify continuous
#'   values, suppress locations or make a disclosure-safety claim.
#'
#' @export
epi_geo_map <- function(x,
                        value = NULL,
                        geometry_colour = NULL,
                        geometry_fill = NULL,
                        na_colour = "grey80") {
  epi_geo_require("sf")
  epi_geo_require("ggplot2")
  epi_geo_validate_sf(x)
  value <- epi_geo_scalar_character(value, "value", allow_null = TRUE)
  geometry_colour <- epi_geo_optional_colour(geometry_colour, "geometry_colour")
  geometry_fill <- epi_geo_optional_colour(geometry_fill, "geometry_fill")
  na_colour <- epi_geo_scalar_character(na_colour, "na_colour")
  geometry_name <- attr(x, "sf_column")
  if (!is.null(value) && (!value %in% names(x) || identical(value, geometry_name))) {
    stop("value must identify one non-geometry attribute.", call. = FALSE)
  }
  if (!is.null(value)) {
    supported_value <- is.numeric(x[[value]]) || is.character(x[[value]]) ||
      is.factor(x[[value]]) || is.logical(x[[value]])
    if (!supported_value) {
      stop("value must contain numeric or categorical data.", call. = FALSE)
    }
  }
  types <- unique(epi_geo_geometry_types(x))
  polygonal <- epi_geo_is_polygonal(types)
  if (any(polygonal) && any(!polygonal)) {
    stop("Mixed polygonal and non-polygonal geometry requires explicit ggplot2 composition.", call. = FALSE)
  }
  use_fill <- length(types) > 0L && all(polygonal)
  if (!is.null(value) && use_fill && !is.null(geometry_fill)) {
    stop("geometry_fill cannot be set when value is mapped to fill.", call. = FALSE)
  }
  if (!is.null(value) && !use_fill && !is.null(geometry_colour)) {
    stop("geometry_colour cannot be set when value is mapped to colour.", call. = FALSE)
  }
  mapping <- NULL
  arguments <- list()
  scale <- NULL
  if (is.null(value)) {
    if (use_fill) {
      arguments$fill <- if (is.null(geometry_fill)) "grey80" else geometry_fill
      arguments$colour <- if (is.null(geometry_colour)) "grey30" else geometry_colour
    } else {
      arguments$colour <- if (is.null(geometry_colour)) "grey30" else geometry_colour
      if (!is.null(geometry_fill)) {
        arguments$fill <- geometry_fill
      }
    }
  } else if (use_fill) {
    mapping <- ggplot2::aes(fill = .data[[value]])
    if (!is.null(geometry_colour)) {
      arguments$colour <- geometry_colour
    }
    scale <- if (is.numeric(x[[value]])) {
      ggplot2::scale_fill_continuous(na.value = na_colour)
    } else {
      ggplot2::scale_fill_discrete(na.value = na_colour)
    }
  } else {
    mapping <- ggplot2::aes(colour = .data[[value]])
    if (!is.null(geometry_fill)) {
      arguments$fill <- geometry_fill
    }
    scale <- if (is.numeric(x[[value]])) {
      ggplot2::scale_colour_continuous(na.value = na_colour)
    } else {
      ggplot2::scale_colour_discrete(na.value = na_colour)
    }
  }
  layer <- do.call(ggplot2::geom_sf, c(list(mapping = mapping), arguments))
  plot <- ggplot2::ggplot(x) +
    layer +
    ggplot2::coord_sf(crs = sf::st_crs(x))
  if (!is.null(scale)) {
    plot <- plot + scale
  }
  plot
}

epi_geo_attribute_group <- function(x) {
  if (inherits(x, "Date")) {
    return("date")
  }
  if (inherits(x, "POSIXct")) {
    return("datetime")
  }
  if (is.logical(x) && !is.object(x)) {
    return("logical")
  }
  if (is.integer(x) && !is.object(x)) {
    return("integer")
  }
  if (is.numeric(x) && !is.object(x)) {
    return("numeric")
  }
  if ((is.character(x) && !is.object(x)) || is.factor(x)) {
    return("text")
  }
  NA_character_
}

epi_geo_attribute_groups <- function(x) {
  attributes <- sf::st_drop_geometry(x)
  vapply(attributes, epi_geo_attribute_group, character(1))
}

epi_geo_reconciles <- function(expected, actual) {
  if (nrow(expected) != nrow(actual)) {
    return(FALSE)
  }
  expected_attributes <- sf::st_drop_geometry(expected)
  actual_attributes <- sf::st_drop_geometry(actual)
  if (!identical(names(expected_attributes), names(actual_attributes))) {
    return(FALSE)
  }
  expected_groups <- epi_geo_attribute_groups(expected)
  actual_groups <- epi_geo_attribute_groups(actual)
  if (!identical(unname(expected_groups), unname(actual_groups))) {
    return(FALSE)
  }
  if (!isTRUE(sf::st_crs(expected) == sf::st_crs(actual))) {
    return(FALSE)
  }
  expected_types <- sort(table(epi_geo_geometry_types(expected)))
  actual_types <- sort(table(epi_geo_geometry_types(actual)))
  if (!identical(expected_types, actual_types)) {
    return(FALSE)
  }
  isTRUE(all.equal(
    epi_geo_bounds(expected), epi_geo_bounds(actual),
    tolerance = 1e-8, check.attributes = TRUE
  ))
}

epi_geo_write_stage <- function(x, stage, layer, replace_layer) {
  suppressMessages(suppressWarnings(sf::st_write(
    x,
    dsn = stage,
    layer = layer,
    driver = "GPKG",
    append = if (replace_layer) FALSE else NA,
    delete_layer = replace_layer,
    quiet = TRUE
  )))
}

epi_geo_file_rename <- function(from, to) {
  file.rename(from, to)
}

epi_geo_file_remove <- function(path) {
  unlink(path)
}

epi_geo_file_copy <- function(from, to) {
  file.copy(from, to, copy.mode = TRUE, copy.date = TRUE)
}

epi_geo_publish_stage <- function(stage, dsn) {
  if (!file.exists(dsn)) {
    if (!epi_geo_file_rename(stage, dsn)) {
      stop("The staged GeoPackage could not be published.", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  backup <- tempfile(
    pattern = ".episcout-gpkg-backup-",
    tmpdir = dirname(dsn),
    fileext = ".gpkg"
  )
  if (!epi_geo_file_rename(dsn, backup)) {
    stop("The existing GeoPackage could not be secured before publication.", call. = FALSE)
  }
  if (!epi_geo_file_rename(stage, dsn)) {
    restored <- epi_geo_file_rename(backup, dsn)
    if (!restored) {
      stop("Publication failed; the previous GeoPackage remains in a recovery backup beside the destination.", call. = FALSE)
    }
    stop("Publication failed; the previous GeoPackage was restored.", call. = FALSE)
  }
  if (epi_geo_file_remove(backup) != 0L) {
    warning("The GeoPackage was published, but its owned backup could not be removed.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Write a reviewed layer to GeoPackage safely
#'
#' Stage, re-read and reconcile one named layer before publishing a local
#' GeoPackage. Existing layers are never replaced silently, and replacement
#' authority applies only to the named layer.
#'
#' @param x A reviewed `sf` object supported by the phase-A spatial contract.
#' @param dsn One local `.gpkg` destination path whose parent directory exists.
#' @param layer One non-empty layer name.
#' @param overwrite Whether an existing layer with the exact name may be
#'   replaced. Unrelated layers are retained.
#'
#' @return `dsn`, invisibly, after successful publication.
#'
#' @details Publication uses a same-directory staging file and verifies feature
#'   count, attribute names and supported types, geometry types, CRS and bounds
#'   after re-reading. Factor attributes are written and reconciled as text.
#'   Failure before publication leaves the previous destination unchanged;
#'   failure during replacement restores the previous complete file when the
#'   file system permits it. Shapefile export and append semantics are not
#'   supported.
#'
#' @export
epi_geo_write <- function(x, dsn, layer, overwrite = FALSE) {
  epi_geo_require("sf")
  epi_geo_validate_sf(x)
  dsn <- epi_geo_validate_local_path(dsn, "gpkg", must_exist = FALSE)
  layer <- epi_geo_scalar_character(layer, "layer")
  overwrite <- epi_geo_scalar_logical(overwrite, "overwrite")
  if (!dir.exists(dirname(dsn))) {
    stop("The destination directory must already exist.", call. = FALSE)
  }
  symlink_target <- Sys.readlink(dsn)
  is_symlink <- !is.na(symlink_target) && nzchar(symlink_target)
  if (dir.exists(dsn) || is_symlink) {
    stop("dsn must identify a regular GeoPackage file, not a directory or symbolic link.", call. = FALSE)
  }
  attribute_groups <- epi_geo_attribute_groups(x)
  if (anyNA(attribute_groups)) {
    stop("x contains an attribute class unsupported by the GeoPackage contract.", call. = FALSE)
  }
  existing_layers <- character()
  if (file.exists(dsn)) {
    existing_layers <- epi_geo_layer_inventory(dsn)$names
  }
  collision <- layer %in% existing_layers
  if (collision && !overwrite) {
    stop("The named GeoPackage layer already exists; set overwrite = TRUE to replace it.", call. = FALSE)
  }
  stage <- tempfile(
    pattern = ".episcout-gpkg-stage-",
    tmpdir = dirname(dsn),
    fileext = ".gpkg"
  )
  on.exit(if (file.exists(stage)) unlink(stage), add = TRUE)
  if (file.exists(dsn) && !epi_geo_file_copy(dsn, stage)) {
    stop("The existing GeoPackage could not be copied to staging.", call. = FALSE)
  }
  written <- tryCatch(
    epi_geo_write_stage(x, stage, layer, replace_layer = collision),
    error = function(error) NULL
  )
  if (is.null(written) || !file.exists(stage)) {
    stop("The GeoPackage layer could not be written to staging.", call. = FALSE)
  }
  staged <- tryCatch(
    epi_geo_read(stage, layer = layer),
    error = function(error) NULL
  )
  if (is.null(staged) || !epi_geo_reconciles(x, staged)) {
    stop("The staged GeoPackage layer did not reconcile with the input.", call. = FALSE)
  }
  epi_geo_publish_stage(stage, dsn)
  invisible(dsn)
}
