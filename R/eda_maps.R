eda_map_options <- function(spec,
                            maps = FALSE,
                            map_vars = character(),
                            max_map_points = 10000L) {
  options <- eda_map_option_values(maps, map_vars, max_map_points)
  map_vars <- options$map_vars
  maps <- options$maps
  max_map_points <- options$max_map_points
  missing <- setdiff(map_vars, spec$name)
  if (length(missing) > 0L) {
    stop("map_vars contains variables not declared in the EDA specification: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  supported <- c("numeric", "integer", "categorical", "binary", "text")
  unsupported <- map_vars[!spec$type[match(map_vars, spec$name)] %in% supported]
  if (length(unsupported) > 0L) {
    stop("map_vars supports only numeric, integer, categorical, binary, or text variables: ", paste(unsupported, collapse = ", "), ".", call. = FALSE)
  }
  options
}

eda_map_option_values <- function(maps = FALSE,
                                  map_vars = character(),
                                  max_map_points = 10000L) {
  intake_validate_flag(maps, "maps")
  if (!is.character(map_vars) || anyNA(map_vars) || any(!nzchar(map_vars))) {
    stop("map_vars must be a character vector of non-empty declared variable names.", call. = FALSE)
  }
  if (anyDuplicated(map_vars)) {
    stop("map_vars must contain unique variable names.", call. = FALSE)
  }
  if (length(map_vars) > 0L && !maps) {
    stop("map_vars requires maps = TRUE.", call. = FALSE)
  }
  valid_limit <- is.numeric(max_map_points) && length(max_map_points) == 1L &&
    !is.na(max_map_points) && is.finite(max_map_points) &&
    max_map_points == floor(max_map_points) && max_map_points >= 1 &&
    max_map_points < .Machine$integer.max
  if (!valid_limit) {
    stop("max_map_points must be a positive whole number below the R integer limit.", call. = FALSE)
  }
  list(
    maps = maps,
    map_vars = map_vars,
    max_map_points = as.integer(max_map_points)
  )
}

eda_map_empty_inventory <- function() {
  data.frame(
    map_id = character(),
    geo_pair = character(),
    value = character(),
    status = character(),
    reason = character(),
    n_source_rows = integer(),
    n_mapped = integer(),
    path = character(),
    stringsAsFactors = FALSE
  )
}

eda_map_empty_result <- function() {
  list(
    maps = stats::setNames(vector("list", 0L), character()),
    map_inventory = eda_map_empty_inventory()
  )
}

eda_validate_map_columns <- function(available_names, options) {
  if (!options$maps) {
    return(invisible(TRUE))
  }
  missing <- setdiff(options$map_vars, available_names)
  if (length(missing) > 0L) {
    stop("Requested map variables are not available in the data source: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

eda_map_candidates <- function(spec, geo, map_vars) {
  if (nrow(geo) == 0L) {
    return(eda_map_empty_inventory())
  }
  rows <- list()
  for (pair_index in seq_len(nrow(geo))) {
    pair <- geo$geo_pair[[pair_index]]
    requests <- c("", map_vars)
    for (value in requests) {
      map_id <- if (!nzchar(value)) {
        sprintf("map-p%03d-geometry", pair_index)
      } else {
        sprintf("map-p%03d-v%03d", pair_index, match(value, spec$name))
      }
      rows[[length(rows) + 1L]] <- data.frame(
        map_id = map_id,
        geo_pair = pair,
        value = value,
        status = "pending",
        reason = "",
        n_source_rows = as.integer(geo$n[[pair_index]]),
        n_mapped = 0L,
        path = "",
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

eda_map_normalise_theme <- function(values, missing_codes) {
  missing <- summary_missing_mask(values, missing_codes)
  if (any(missing)) {
    values[missing] <- NA
  }
  values
}

eda_map_reconcile_conversion <- function(converted, geo_row) {
  audit <- converted$audit
  valid <- nrow(audit) == 1L && isTRUE(audit$eligible[[1]]) &&
    identical(as.integer(audit$rows[[1]]), as.integer(geo_row$n[[1]])) &&
    identical(as.integer(audit$missing_x[[1]]), as.integer(geo_row$missing_x[[1]])) &&
    identical(as.integer(audit$missing_y[[1]]), as.integer(geo_row$missing_y[[1]])) &&
    identical(as.integer(audit$both_missing[[1]]), as.integer(geo_row$both_missing[[1]])) &&
    identical(as.integer(audit$non_finite[[1]]), as.integer(geo_row$non_finite[[1]])) &&
    identical(as.integer(audit$range_failures[[1]]), as.integer(geo_row$range_failures[[1]]))
  if (!valid || is.null(converted$data)) {
    stop("EDA map conversion did not reconcile with coordinate-pair QA.", call. = FALSE)
  }
  invisible(TRUE)
}

eda_data_frame_maps <- function(data, spec, geo, options) {
  if (!options$maps) {
    return(eda_map_empty_result())
  }
  inventory <- eda_map_candidates(spec, geo, options$map_vars)
  if (nrow(inventory) == 0L) {
    return(list(
      maps = stats::setNames(vector("list", 0L), character()),
      map_inventory = inventory
    ))
  }
  created <- list()
  for (pair_index in seq_len(nrow(geo))) {
    geo_row <- geo[pair_index, , drop = FALSE]
    inventory_rows <- inventory$geo_pair == geo_row$geo_pair[[1]]
    if (!isTRUE(geo_row$map_ready[[1]])) {
      inventory$status[inventory_rows] <- "not_created"
      inventory$reason[inventory_rows] <- geo_row$reason[[1]]
      next
    }
    if (geo_row$n[[1]] > options$max_map_points) {
      inventory$status[inventory_rows] <- "not_created"
      inventory$reason[inventory_rows] <- "max_map_points_exceeded"
      next
    }
    private_names <- unique(c(
      geo_row$x_name[[1]], geo_row$y_name[[1]], options$map_vars
    ))
    private <- data[private_names]
    for (value in options$map_vars) {
      private[[value]] <- eda_map_normalise_theme(
        private[[value]], eda_missing_codes(spec, value)
      )
    }
    converted <- epi_geo_from_coords(
      private,
      x = geo_row$x_name[[1]],
      y = geo_row$y_name[[1]],
      crs = eda_geo_crs_value(geo_row$geo_crs[[1]]),
      remove = FALSE
    )
    eda_map_reconcile_conversion(converted, geo_row)
    for (row_index in which(inventory_rows)) {
      value <- inventory$value[[row_index]]
      plot <- epi_geo_map(
        converted$data,
        value = if (nzchar(value)) value else NULL
      )
      map_id <- inventory$map_id[[row_index]]
      created[[map_id]] <- plot
      inventory$status[[row_index]] <- "created"
      inventory$reason[[row_index]] <- ""
      inventory$n_mapped[[row_index]] <- as.integer(geo_row$n[[1]])
      inventory$path[[row_index]] <- paste0("maps/", map_id, ".svg")
    }
  }
  list(maps = created, map_inventory = inventory)
}

eda_postgres_map_data_inside <- function(source,
                                         spec,
                                         geo,
                                         options,
                                         timing_env = NULL,
                                         n_total = NULL) {
  if (!options$maps || nrow(geo) == 0L) {
    return(data.frame())
  }
  ready <- geo$map_ready & geo$n <= options$max_map_points
  if (!any(ready)) {
    return(data.frame())
  }
  if (is.null(n_total)) {
    n_total <- eda_postgres_row_count(source, timing_env)
  }
  if (n_total > options$max_map_points) {
    return(data.frame())
  }
  selected <- unique(c(
    as.character(geo$x_name[ready]),
    as.character(geo$y_name[ready]),
    options$map_vars
  ))
  expressions <- character(length(selected))
  params <- list()
  for (index in seq_along(selected)) {
    name <- selected[[index]]
    spec_index <- match(name, spec$name)
    column <- eda_postgres_column(source, name)
    if (is.na(spec_index) || is.null(column)) {
      stop("A requested map column is unavailable from the PostgreSQL source.", call. = FALSE)
    }
    compatibility <- eda_pg_type_compatibility(
      column, spec$type[[spec_index]], eda_spec_levels(spec$levels[[spec_index]])
    )
    if (compatibility$status == "incompatible") {
      stop("A requested map column has incompatible PostgreSQL storage.", call. = FALSE)
    }
    contract <- eda_postgres_missing_contract(
      source,
      column,
      spec$type[[spec_index]],
      eda_missing_codes(spec, name),
      offset = length(params)
    )
    if (!contract$valid) {
      stop("A requested map missing-value declaration cannot be represented safely in PostgreSQL.", call. = FALSE)
    }
    value <- eda_postgres_value_expression(
      source, column, spec$type[[spec_index]]
    )
    alias <- eda_postgres_column_sql(source, name)
    expressions[[index]] <- paste0(
      "CASE WHEN ", contract$sql, " THEN NULL ELSE ", value, " END AS ", alias
    )
    params <- c(params, contract$params)
  }
  defensive_limit <- options$max_map_points + 1L
  order_sql <- paste(seq_along(expressions), collapse = ", ")
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "SELECT ", paste(expressions, collapse = ", "),
      " FROM ", eda_postgres_table_sql(source),
      " ORDER BY ", order_sql,
      " LIMIT ", defensive_limit
    ),
    params = params,
    query_kind = "map_collection",
    limit = defensive_limit,
    timing_env = timing_env,
    name = "declared_map_columns"
  )
  if (nrow(observed) != n_total) {
    stop("PostgreSQL map collection did not reconcile with the snapshot row count.", call. = FALSE)
  }
  observed
}

eda_write_maps <- function(maps, inventory, output_dir, context = "EDA") {
  created <- which(inventory$status == "created")
  if (length(created) == 0L) {
    return(invisible(TRUE))
  }
  maps_dir <- file.path(output_dir, "maps")
  if (!dir.exists(maps_dir) && !dir.create(maps_dir, showWarnings = FALSE)) {
    stop("The ", context, " map directory could not be created.", call. = FALSE)
  }
  for (index in created) {
    map_id <- inventory$map_id[[index]]
    path <- file.path(output_dir, inventory$path[[index]])
    tryCatch(
      ggplot2::ggsave(
        path,
        plot = maps[[map_id]],
        device = grDevices::svg,
        width = 8,
        height = 5,
        units = "in"
      ),
      error = function(error) {
        stop("An ", context, " map SVG could not be rendered safely.", call. = FALSE)
      }
    )
  }
  invisible(TRUE)
}
