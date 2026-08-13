#' Profile declared coordinate pairs without collecting geometry
#'
#' Calculate aggregate map-readiness QA for coordinate pairs declared explicitly
#' in an EDA specification. Coordinate values, row identifiers, geometry,
#' bounds and maps are never returned.
#'
#' @param data A data frame or an [epi_eda_postgres_source()].
#' @param spec An EDA specification data frame or CSV path.
#'
#' @return A data frame with one aggregate row per declared coordinate pair.
#'   A specification without coordinate metadata returns a typed zero-row data
#'   frame.
#'
#' @details Map readiness confirms only that every row has a complete, finite
#'   pair and, for EPSG:4326, lies within the declared longitude/latitude
#'   limits. It is not geometry construction or evidence that coordinates
#'   represent the intended place, person, time or unit.
#'
#' @export
epi_eda_profile_geo <- function(data, spec) {
  spec <- epi_eda_spec(spec)
  if (inherits(data, "epi_eda_postgres_source")) {
    return(eda_postgres_transaction(
      data,
      eda_postgres_geo_inside(data, spec)
    ))
  }
  if (!is.data.frame(data)) {
    stop("Data must be a data frame or an epi_eda_postgres_source.", call. = FALSE)
  }
  eda_geo_profile_frame(data, spec)
}

eda_geo_empty <- function() {
  data.frame(
    geo_pair = character(), x_name = character(), y_name = character(),
    geo_crs = character(), crs_epsg = integer(), n = integer(),
    complete_pairs = integer(), missing_x = integer(), missing_y = integer(),
    both_missing = integer(), non_finite = integer(),
    range_failures = integer(), map_ready = logical(), status = character(),
    reason = character(), stringsAsFactors = FALSE
  )
}

eda_geo_pair_rows <- function(spec) {
  if (!all(eda_geo_spec_fields() %in% names(spec))) {
    return(list())
  }
  declared <- which(spec$geo_role %in% c("x", "y"))
  if (length(declared) == 0L) {
    return(list())
  }
  pairs <- unique(spec$geo_pair[declared])
  lapply(pairs, function(pair) {
    rows <- declared[spec$geo_pair[declared] == pair]
    list(
      pair = pair,
      x_row = rows[spec$geo_role[rows] == "x"][[1L]],
      y_row = rows[spec$geo_role[rows] == "y"][[1L]],
      crs = spec$geo_crs[rows][[1L]]
    )
  })
}

eda_geo_missing_mask <- function(values, missing_codes) {
  missing <- is.na(values) & !is.nan(values)
  if (length(missing_codes) == 0L) {
    return(missing)
  }
  text <- as.character(values)
  missing | (!is.na(text) & text %in% as.character(missing_codes))
}

eda_geo_status <- function(n, missing_x, missing_y, both_missing,
                           non_finite, range_failures) {
  if (n == 0L) {
    return(list(map_ready = FALSE, status = "not_ready", reason = "no_rows"))
  }
  reasons <- character()
  if (missing_x + missing_y + both_missing > 0L) {
    reasons <- c(reasons, "incomplete_pairs")
  }
  if (non_finite > 0L) {
    reasons <- c(reasons, "non_finite_coordinates")
  }
  if (range_failures > 0L) {
    reasons <- c(reasons, "declared_crs_range_failure")
  }
  if (length(reasons) == 0L) {
    return(list(
      map_ready = TRUE, status = "ready", reason = "all_rows_map_ready"
    ))
  }
  list(
    map_ready = FALSE, status = "not_ready",
    reason = paste(reasons, collapse = ";")
  )
}

eda_geo_result_row <- function(pair, x_name, y_name, crs, counts) {
  resolved <- eda_geo_resolve_crs(crs)
  status <- eda_geo_status(
    counts$n, counts$missing_x, counts$missing_y, counts$both_missing,
    counts$non_finite, counts$range_failures
  )
  data.frame(
    geo_pair = pair,
    x_name = x_name,
    y_name = y_name,
    geo_crs = crs,
    crs_epsg = if (is.null(resolved$epsg)) NA_integer_ else as.integer(resolved$epsg),
    n = as.integer(counts$n),
    complete_pairs = as.integer(counts$complete_pairs),
    missing_x = as.integer(counts$missing_x),
    missing_y = as.integer(counts$missing_y),
    both_missing = as.integer(counts$both_missing),
    non_finite = as.integer(counts$non_finite),
    range_failures = as.integer(counts$range_failures),
    map_ready = status$map_ready,
    status = status$status,
    reason = status$reason,
    stringsAsFactors = FALSE
  )
}

eda_geo_profile_frame <- function(data, spec) {
  pairs <- eda_geo_pair_rows(spec)
  if (length(pairs) == 0L) {
    return(eda_geo_empty())
  }
  rows <- lapply(pairs, function(pair) {
    x_name <- spec$name[[pair$x_row]]
    y_name <- spec$name[[pair$y_row]]
    if (!all(c(x_name, y_name) %in% names(data))) {
      stop("Declared coordinate variables are missing from the data.", call. = FALSE)
    }
    x <- data[[x_name]]
    y <- data[[y_name]]
    numeric_storage <- is.numeric(x) && is.numeric(y) &&
      !inherits(x, c("Date", "POSIXt", "IDate")) &&
      !inherits(y, c("Date", "POSIXt", "IDate"))
    if (!numeric_storage) {
      stop("Declared coordinate variables require numeric storage.", call. = FALSE)
    }
    x_missing <- eda_geo_missing_mask(x, eda_missing_codes(spec, x_name))
    y_missing <- eda_geo_missing_mask(y, eda_missing_codes(spec, y_name))
    x_non_finite <- !x_missing & !is.finite(x)
    y_non_finite <- !y_missing & !is.finite(y)
    non_finite <- x_non_finite | y_non_finite
    complete <- !x_missing & !y_missing & !non_finite
    resolved <- eda_geo_resolve_crs(pair$crs)
    range_failure <- rep(FALSE, nrow(data))
    if (isTRUE(resolved$epsg == 4326L)) {
      range_failure[complete] <- x[complete] < -180 | x[complete] > 180 |
        y[complete] < -90 | y[complete] > 90
    }
    eda_geo_result_row(pair$pair, x_name, y_name, pair$crs, list(
      n = nrow(data),
      complete_pairs = sum(complete),
      missing_x = sum(x_missing & !y_missing),
      missing_y = sum(y_missing & !x_missing),
      both_missing = sum(x_missing & y_missing),
      non_finite = sum(non_finite),
      range_failures = sum(range_failure)
    ))
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

eda_geo_pg_missing_contract <- function(source, column, expected_type, codes,
                                        offset = 0L) {
  column_sql <- eda_postgres_column_sql(source, column$name[[1]])
  standard <- paste0("(", column_sql, " IS NULL)")
  if (length(codes) == 0L) {
    return(list(sql = standard, params = list(), valid = TRUE, reason = NA_character_))
  }
  family <- eda_postgres_storage_family(column)
  parsed <- eda_postgres_parse_sentinels(codes, family, expected_type, column)
  if (!parsed$valid) {
    return(list(sql = standard, params = list(), valid = FALSE, reason = parsed$reason))
  }
  predicates <- vapply(seq_along(parsed$values), function(index) {
    placeholder <- paste0("$", offset + index)
    cast <- if (family == "integer") "::bigint" else "::double precision"
    paste0(column_sql, cast, " = ", placeholder, cast)
  }, character(1))
  list(
    sql = paste0("(", standard, " OR ", paste(predicates, collapse = " OR "), ")"),
    params = as.list(parsed$values), valid = TRUE, reason = NA_character_
  )
}

eda_geo_pg_non_finite <- function(source, column) {
  if (eda_postgres_storage_family(column) == "integer") {
    return("FALSE")
  }
  column_sql <- eda_postgres_column_sql(source, column$name[[1]])
  paste0("(", column_sql, "::text IN ('NaN', 'Infinity', '-Infinity'))")
}

eda_postgres_geo_pair <- function(source, spec, pair, timing_env) {
  x_name <- spec$name[[pair$x_row]]
  y_name <- spec$name[[pair$y_row]]
  x_column <- eda_postgres_column(source, x_name)
  y_column <- eda_postgres_column(source, y_name)
  if (is.null(x_column) || is.null(y_column)) {
    stop("Declared coordinate variables are missing from the PostgreSQL source.", call. = FALSE)
  }
  x_type <- spec$analysis_type[[pair$x_row]]
  y_type <- spec$analysis_type[[pair$y_row]]
  compatible <- eda_pg_type_compatibility(x_column, x_type)$status != "incompatible" &&
    eda_pg_type_compatibility(y_column, y_type)$status != "incompatible"
  if (!compatible) {
    stop("Declared coordinate variables require compatible PostgreSQL numeric storage.", call. = FALSE)
  }
  x_missing <- eda_geo_pg_missing_contract(
    source, x_column, x_type, eda_missing_codes(spec, x_name)
  )
  y_missing <- eda_geo_pg_missing_contract(
    source, y_column, y_type, eda_missing_codes(spec, y_name),
    length(x_missing$params)
  )
  if (!x_missing$valid || !y_missing$valid) {
    stop("A declared coordinate missing sentinel cannot be represented safely in PostgreSQL.", call. = FALSE)
  }
  x_sql <- eda_postgres_value_expression(source, x_column, "numeric")
  y_sql <- eda_postgres_value_expression(source, y_column, "numeric")
  x_non_finite <- eda_geo_pg_non_finite(source, x_column)
  y_non_finite <- eda_geo_pg_non_finite(source, y_column)
  params <- c(x_missing$params, y_missing$params)
  resolved <- eda_geo_resolve_crs(pair$crs)
  range_sql <- "FALSE"
  if (isTRUE(resolved$epsg == 4326L)) {
    first <- length(params) + 1L
    range_sql <- paste0(
      "(complete AND (x_value < $", first, "::double precision OR x_value > $",
      first + 1L, "::double precision OR y_value < $", first + 2L,
      "::double precision OR y_value > $", first + 3L, "::double precision))"
    )
    params <- c(params, list(-180, 180, -90, 90))
  }
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH classified AS (SELECT ", x_sql, " AS x_value, ", y_sql,
      " AS y_value, ", x_missing$sql, " AS x_missing, ", y_missing$sql,
      " AS y_missing, ", x_non_finite, " AS x_non_finite, ", y_non_finite,
      " AS y_non_finite FROM ", eda_postgres_table_sql(source), "), flags AS (",
      "SELECT *, (NOT x_missing AND NOT y_missing AND NOT x_non_finite AND NOT y_non_finite) AS complete FROM classified) ",
      "SELECT count(*)::text AS n, count(*) FILTER (WHERE complete)::text AS complete_pairs, ",
      "count(*) FILTER (WHERE x_missing AND NOT y_missing)::text AS missing_x, ",
      "count(*) FILTER (WHERE y_missing AND NOT x_missing)::text AS missing_y, ",
      "count(*) FILTER (WHERE x_missing AND y_missing)::text AS both_missing, ",
      "count(*) FILTER (WHERE (NOT x_missing AND x_non_finite) OR (NOT y_missing AND y_non_finite))::text AS non_finite, ",
      "count(*) FILTER (WHERE ", range_sql, ")::text AS range_failures FROM flags"
    ),
    params = params,
    query_kind = "geo_pair_qa",
    limit = 1L,
    timing_env = timing_env,
    variable_index = pair$x_row,
    name = pair$pair
  )
  fields <- c(
    "n", "complete_pairs", "missing_x", "missing_y", "both_missing",
    "non_finite", "range_failures"
  )
  counts <- lapply(observed[fields], eda_checked_count)
  eda_geo_result_row(pair$pair, x_name, y_name, pair$crs, counts)
}

eda_postgres_geo_inside <- function(source, spec, timing_env = NULL) {
  pairs <- eda_geo_pair_rows(spec)
  if (length(pairs) == 0L) {
    return(eda_geo_empty())
  }
  rows <- lapply(pairs, function(pair) {
    eda_postgres_geo_pair(source, spec, pair, timing_env)
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

eda_geo_reconcile <- function(geo, n_total) {
  if (nrow(geo) == 0L) {
    return(invisible(TRUE))
  }
  count_names <- c(
    "complete_pairs", "missing_x", "missing_y", "both_missing",
    "non_finite", "range_failures"
  )
  counts <- geo[count_names]
  missing_total <- geo$missing_x + geo$missing_y + geo$both_missing
  expected_status <- lapply(seq_len(nrow(geo)), function(index) {
    eda_geo_status(
      geo$n[[index]], geo$missing_x[[index]], geo$missing_y[[index]],
      geo$both_missing[[index]], geo$non_finite[[index]],
      geo$range_failures[[index]]
    )
  })
  valid <- all(geo$n == n_total) && !anyNA(counts) &&
    all(vapply(counts, is.integer, logical(1))) &&
    all(unlist(counts, use.names = FALSE) >= 0L) &&
    all(unlist(counts, use.names = FALSE) <= rep(geo$n, times = length(count_names))) &&
    all(missing_total <= geo$n) &&
    all(geo$complete_pairs <= geo$n - missing_total) &&
    all(geo$range_failures <= geo$complete_pairs) &&
    identical(geo$map_ready, vapply(expected_status, `[[`, logical(1), "map_ready")) &&
    identical(geo$status, vapply(expected_status, `[[`, character(1), "status")) &&
    identical(geo$reason, vapply(expected_status, `[[`, character(1), "reason"))
  if (!isTRUE(valid)) {
    stop("EDA coordinate-pair QA counts did not reconcile.", call. = FALSE)
  }
  invisible(TRUE)
}
