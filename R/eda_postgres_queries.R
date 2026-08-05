eda_db_fetch <- function(con,
                         statement,
                         params = list(),
                         query_kind,
                         limit,
                         timing_env = NULL,
                         variable_index = NA_integer_,
                         name = NA_character_) {
  started <- proc.time()[["elapsed"]]
  result <- NULL
  conditioned <- eda_db_observe_conditions({
    result <- DBI::dbSendQuery(con, statement)
    if (length(params) > 0L) {
      DBI::dbBind(result, params)
    }
    value <- DBI::dbFetch(result, n = -1L)
    if (is.finite(limit) && nrow(value) > limit) {
      stop("fetch limit exceeded", call. = FALSE)
    }
    as.data.frame(value, stringsAsFactors = FALSE)
  })
  observed <- conditioned$value
  if (!eda_db_cleanup_result(result)) {
    observed <- simpleError("result cleanup failed")
  }
  elapsed <- proc.time()[["elapsed"]] - started
  status <- if (inherits(observed, "error")) "error" else "complete"
  rows <- if (inherits(observed, "error")) NA_integer_ else nrow(observed)
  eda_db_record_timing(
    timing_env, query_kind, variable_index, name, elapsed, rows,
    if (is.finite(limit)) as.integer(limit) else NA_integer_, status
  )
  eda_db_signal_conditions(conditioned, "query")
  if (inherits(observed, "error")) {
    stop("PostgreSQL EDA query failed at ", query_kind, "; review restricted database logs.", call. = FALSE)
  }
  observed
}

eda_db_statement <- function(con, statement, query_kind, timing_env = NULL) {
  started <- proc.time()[["elapsed"]]
  result <- NULL
  conditioned <- eda_db_observe_conditions({
    result <- DBI::dbSendStatement(con, statement)
    DBI::dbGetRowsAffected(result)
  })
  observed <- conditioned$value
  if (!eda_db_cleanup_result(result)) {
    observed <- simpleError("result cleanup failed")
  }
  elapsed <- proc.time()[["elapsed"]] - started
  eda_db_record_timing(
    timing_env, query_kind, NA_integer_, NA_character_, elapsed, 0L, 0L,
    if (inherits(observed, "error")) "error" else "complete"
  )
  eda_db_signal_conditions(conditioned, "transaction setup")
  if (inherits(observed, "error")) {
    stop("PostgreSQL EDA transaction setup failed; review restricted database logs.", call. = FALSE)
  }
  invisible(observed)
}

eda_db_clear_result <- function(result) {
  DBI::dbClearResult(result)
}

eda_db_observe_conditions <- function(action) {
  conditions <- new.env(parent = emptyenv())
  conditions$message <- FALSE
  conditions$warning <- FALSE
  value <- tryCatch(
    withCallingHandlers(
      force(action),
      message = function(condition) {
        conditions$message <- TRUE
        tryInvokeRestart("muffleMessage")
      },
      warning = function(condition) {
        conditions$warning <- TRUE
        tryInvokeRestart("muffleWarning")
      }
    ),
    error = function(error) error
  )
  list(
    value = value,
    message = conditions$message,
    warning = conditions$warning
  )
}

eda_db_signal_conditions <- function(observed, context) {
  if (isTRUE(observed$message)) {
    message(
      "PostgreSQL EDA ", context,
      " emitted a database message; details are available in restricted database logs."
    )
  }
  if (isTRUE(observed$warning)) {
    warning(
      "PostgreSQL EDA ", context,
      " emitted a database warning; details are available in restricted database logs.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

eda_db_cleanup_result <- function(result) {
  if (is.null(result)) {
    return(TRUE)
  }
  valid_observed <- eda_db_observe_conditions(DBI::dbIsValid(result))
  valid <- if (inherits(valid_observed$value, "error")) NA else valid_observed$value
  eda_db_signal_conditions(valid_observed, "result cleanup")
  if (isFALSE(valid)) {
    return(TRUE)
  }
  if (!isTRUE(valid)) {
    return(FALSE)
  }
  clear_observed <- eda_db_observe_conditions(eda_db_clear_result(result))
  eda_db_signal_conditions(clear_observed, "result cleanup")
  !inherits(clear_observed$value, "error")
}

eda_db_record_timing <- function(timing_env,
                                 query_kind,
                                 variable_index,
                                 name,
                                 elapsed,
                                 rows,
                                 limit,
                                 status) {
  if (is.null(timing_env)) {
    return(invisible(NULL))
  }
  timing_env$rows[[length(timing_env$rows) + 1L]] <- data.frame(
    stage = eda_db_query_stage(query_kind),
    variable_index = as.integer(variable_index),
    name = as.character(name),
    query_kind = as.character(query_kind),
    elapsed_seconds = as.numeric(elapsed),
    rows_returned = as.integer(rows),
    bounded_limit = as.integer(limit),
    status = as.character(status),
    stringsAsFactors = FALSE
  )
  invisible(NULL)
}

eda_db_query_stage <- function(query_kind) {
  if (grepl("catalogue|server_version", query_kind)) {
    return("preflight")
  }
  if (grepl("plot", query_kind)) {
    return("plot_preparation")
  }
  if (grepl("transaction", query_kind)) {
    return("snapshot")
  }
  "aggregate_profiling"
}

eda_checked_count <- function(value, field = "PostgreSQL count") {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !grepl("^[0-9]+$", value)) {
    stop(field, " was not returned as exact decimal text.", call. = FALSE)
  }
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.finite(numeric_value) || numeric_value > .Machine$integer.max) {
    stop(field, " exceeds the canonical R integer range.", call. = FALSE)
  }
  as.integer(numeric_value)
}

eda_postgres_table_sql <- function(source) {
  as.character(DBI::dbQuoteIdentifier(
    source$con,
    DBI::Id(schema = source$schema, table = source$relation)
  ))
}

eda_postgres_column_sql <- function(source, name) {
  as.character(DBI::dbQuoteIdentifier(source$con, name))
}

eda_postgres_column <- function(source, name) {
  index <- match(name, source$columns$name)
  if (is.na(index)) {
    return(NULL)
  }
  source$columns[index, , drop = FALSE]
}

eda_postgres_storage_family <- function(column) {
  if (is.null(column)) {
    return("absent")
  }
  base <- as.character(column$base_udt_name[[1]])
  typtype <- as.character(column$typtype[[1]])
  if (typtype == "e") {
    return("enum")
  }
  if (base %in% c("int2", "int4", "int8")) {
    return("integer")
  }
  if (base %in% c("numeric", "float4", "float8")) {
    return("numeric")
  }
  if (base %in% c("text", "varchar", "bpchar")) {
    return("text")
  }
  if (base == "bool") {
    return("boolean")
  }
  if (base == "date") {
    return("date")
  }
  if (base == "timestamptz") {
    return("datetime")
  }
  if (base == "timestamp") {
    return("local_datetime")
  }
  "unsupported"
}

eda_postgres_observed_type <- function(column) {
  family <- eda_postgres_storage_family(column)
  mapped <- c(
    integer = "numeric", numeric = "numeric", text = "text",
    enum = "categorical", boolean = "binary", date = "date",
    datetime = "datetime", local_datetime = "datetime"
  )
  if (family %in% names(mapped)) return(unname(mapped[[family]]))
  as.character(column$formatted_type[[1]])
}

eda_pg_type_compatibility <- function(column, expected_type, declared_levels = character()) {
  if (is.null(column)) {
    return(eda_type_result("not_applicable", "Observed variable is not present."))
  }
  family <- eda_postgres_storage_family(column)
  deterministic <- isTRUE(column$collation_deterministic[[1]])
  if (expected_type == "numeric" && family %in% c("integer", "numeric")) {
    return(eda_type_result("compatible", "PostgreSQL numeric storage is compatible with numeric."))
  }
  if (expected_type == "integer" && family == "integer") {
    return(eda_type_result("compatible", "PostgreSQL integral storage is compatible with integer."))
  }
  if (expected_type %in% c("categorical", "binary")) {
    allowed <- family %in% c("text", "enum", "boolean", "integer")
    if (allowed && family %in% c("text", "enum") && !deterministic) {
      return(eda_type_result("incompatible", "PostgreSQL text equality uses a nondeterministic collation; use a reviewed deterministic view cast."))
    }
    if (expected_type == "binary" && family != "boolean" && length(declared_levels) != 2L) {
      return(eda_type_result("incompatible", "Non-boolean binary storage requires exactly two reviewed levels."))
    }
    if (allowed) {
      status <- if (family %in% c("text", "enum", "boolean")) "compatible" else "coercible"
      return(eda_type_result(status, "PostgreSQL storage is compatible with the reviewed categorical declaration."))
    }
  }
  if (expected_type == "text" && family %in% c("text", "enum")) {
    if (!deterministic) {
      return(eda_type_result("incompatible", "PostgreSQL text equality uses a nondeterministic collation; use a reviewed deterministic view cast."))
    }
    return(eda_type_result("compatible", "PostgreSQL text storage is compatible with text."))
  }
  if (expected_type == "date" && family == "date") {
    return(eda_type_result("compatible", "PostgreSQL date storage is compatible with date."))
  }
  if (expected_type == "datetime" && family == "datetime") {
    return(eda_type_result("compatible", "PostgreSQL timestamp with time zone storage is compatible with UTC datetime."))
  }
  if (expected_type == "datetime" && family == "local_datetime") {
    return(eda_type_result("incompatible", "PostgreSQL timestamp without time zone has no reviewed instant or DST meaning; use a reviewed view cast to timestamp with time zone."))
  }
  eda_type_result(
    "incompatible",
    paste0("PostgreSQL storage type ", column$formatted_type[[1]], " is incompatible with reviewed type ", expected_type, ".")
  )
}

eda_postgres_missing_contract <- function(source, column, expected_type, codes) {
  column_sql <- eda_postgres_column_sql(source, column$name[[1]])
  family <- eda_postgres_storage_family(column)
  standard <- paste0("(", column_sql, " IS NULL)")
  if (family == "numeric") {
    standard <- paste0("(", column_sql, " IS NULL OR ", column_sql, "::text = 'NaN')")
  }
  if (length(codes) == 0L) {
    return(list(sql = standard, params = list(), valid = TRUE, reason = NA_character_))
  }
  parsed <- eda_postgres_parse_sentinels(codes, family, expected_type, column)
  if (!parsed$valid) {
    return(list(sql = standard, params = list(), valid = FALSE, reason = parsed$reason))
  }
  predicates <- vapply(seq_along(parsed$values), function(index) {
    placeholder <- paste0("$", index)
    if (family == "integer") {
      return(paste0(column_sql, " = ", placeholder, "::bigint"))
    }
    if (family == "numeric") {
      return(paste0(column_sql, "::double precision = ", placeholder, "::double precision"))
    }
    if (family == "boolean") {
      return(paste0(column_sql, " = ", placeholder, "::boolean"))
    }
    if (family == "date") {
      return(paste0(column_sql, " = ", placeholder, "::date"))
    }
    if (family == "datetime") {
      return(paste0(column_sql, " = ", placeholder, "::timestamptz"))
    }
    paste0(column_sql, "::text = ", placeholder, "::text")
  }, character(1))
  list(
    sql = paste0("(", standard, " OR ", paste(predicates, collapse = " OR "), ")"),
    params = as.list(parsed$values),
    valid = TRUE,
    reason = NA_character_
  )
}

eda_postgres_parse_sentinels <- function(codes, family, expected_type, column) {
  codes <- as.character(codes)
  invalid <- FALSE
  values <- codes
  if (family == "integer") {
    invalid <- any(!grepl("^[+-]?[0-9]+$", codes))
    numeric_values <- suppressWarnings(as.numeric(codes))
    limits_by_type <- list(
      int2 = c(-32768, 32767),
      int4 = c(-2147483648, 2147483647),
      int8 = c(-9007199254740991, 9007199254740991)
    )
    limits <- limits_by_type[[as.character(column$base_udt_name[[1]])]]
    if (is.null(limits)) limits <- c(NA_real_, NA_real_)
    invalid <- invalid || any(!is.finite(numeric_values)) ||
      any(numeric_values < limits[[1]] | numeric_values > limits[[2]])
  } else if (family == "numeric") {
    numeric_values <- suppressWarnings(as.numeric(codes))
    invalid <- any(is.na(numeric_values) & toupper(codes) != "NAN")
  } else if (family == "boolean") {
    invalid <- any(!(tolower(codes) %in% c("true", "false", "t", "f", "1", "0")))
  } else if (family == "date") {
    invalid <- !eda_all_iso_dates(codes)
  } else if (family == "datetime") {
    has_zone <- grepl("(Z|[+-][0-9]{2}:?[0-9]{2})$", codes)
    parsed <- summary_parse_datetime_chr(codes)
    invalid <- any(!has_zone | is.na(parsed))
  } else if (!(family %in% c("text", "enum"))) {
    invalid <- TRUE
  }
  if (invalid) {
    return(list(
      valid = FALSE,
      values = character(),
      reason = paste0("A reviewed missing sentinel cannot be represented safely for PostgreSQL ", expected_type, " storage.")
    ))
  }
  list(valid = TRUE, values = values, reason = NA_character_)
}

eda_postgres_value_expression <- function(source, column, expected_type) {
  column_sql <- eda_postgres_column_sql(source, column$name[[1]])
  family <- eda_postgres_storage_family(column)
  if (expected_type %in% c("numeric", "integer")) {
    return(paste0(column_sql, "::double precision"))
  }
  if (expected_type == "date") {
    return(paste0("(", column_sql, " - DATE '1970-01-01')::double precision"))
  }
  if (expected_type == "datetime") {
    return(paste0("extract(epoch FROM ", column_sql, ")::double precision"))
  }
  if (expected_type == "binary" && family == "boolean") {
    return(paste0("CASE WHEN ", column_sql, " THEN 'TRUE' ELSE 'FALSE' END"))
  }
  paste0(column_sql, "::text")
}

eda_postgres_row_count <- function(source, timing_env = NULL) {
  observed <- eda_db_fetch(
    source$con,
    paste0("SELECT count(*)::text AS n FROM ", eda_postgres_table_sql(source)),
    query_kind = "row_count",
    limit = 1L,
    timing_env = timing_env
  )
  eda_checked_count(observed$n[[1]], "PostgreSQL relation row count")
}

eda_postgres_schema_inside <- function(source, spec, timing_env = NULL) {
  expected <- lapply(seq_len(nrow(spec)), function(index) {
    column <- eda_postgres_column(source, spec$name[[index]])
    levels <- if ("levels" %in% names(spec)) eda_spec_levels(spec$levels[[index]]) else character()
    compatibility <- eda_pg_type_compatibility(column, spec$type[[index]], levels)
    present <- !is.null(column)
    data.frame(
      name = spec$name[[index]],
      expected_type = spec$type[[index]],
      observed_type = if (present) eda_postgres_observed_type(column) else NA_character_,
      expected_present = TRUE,
      observed_present = present,
      status = if (present) "present" else "missing",
      type_status = compatibility$status,
      type_reason = compatibility$reason,
      stringsAsFactors = FALSE
    )
  })
  expected <- if (length(expected) == 0L) {
    data.frame(
      name = character(), expected_type = character(), observed_type = character(),
      expected_present = logical(), observed_present = logical(), status = character(),
      type_status = character(), type_reason = character(), stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, expected)
  }
  unexpected_names <- setdiff(source$columns$name, spec$name)
  if (length(unexpected_names) > 0L) {
    unexpected <- lapply(unexpected_names, function(name) {
      column <- eda_postgres_column(source, name)
      data.frame(
        name = name, expected_type = NA_character_,
        observed_type = eda_postgres_observed_type(column),
        expected_present = FALSE, observed_present = TRUE, status = "unexpected",
        type_status = "not_applicable",
        type_reason = "Variable is not declared in the EDA specification.",
        stringsAsFactors = FALSE
      )
    })
    expected <- rbind(expected, do.call(rbind, unexpected))
  }
  row.names(expected) <- NULL
  expected[, schema_columns(), drop = FALSE]
}

eda_postgres_missing_inside <- function(source, spec, timing_env = NULL, n_total = NULL) {
  if (is.null(n_total)) n_total <- eda_postgres_row_count(source, timing_env)
  n_missing <- vapply(seq_len(nrow(spec)), function(index) {
    name <- spec$name[[index]]
    column <- eda_postgres_column(source, name)
    if (is.null(column)) {
      return(NA_integer_)
    }
    contract <- eda_postgres_missing_contract(
      source, column, spec$type[[index]], eda_missing_codes(spec, name)
    )
    if (!contract$valid) {
      return(NA_integer_)
    }
    observed <- eda_db_fetch(
      source$con,
      paste0(
        "SELECT count(*) FILTER (WHERE ", contract$sql, ")::text AS n_missing FROM ",
        eda_postgres_table_sql(source)
      ),
      params = contract$params,
      query_kind = "missing_scalar",
      limit = 1L,
      timing_env = timing_env,
      variable_index = index,
      name = name
    )
    eda_checked_count(observed$n_missing[[1]], "PostgreSQL missing count")
  }, integer(1))
  data.frame(
    name = spec$name,
    n = rep(as.integer(n_total), nrow(spec)),
    n_missing = n_missing,
    p_missing = if (n_total > 0L) n_missing / n_total else rep(NA_real_, nrow(spec)),
    stringsAsFactors = FALSE
  )
}

eda_postgres_basic_counts <- function(source, column, contract, expression, index, timing_env) {
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
      eda_postgres_table_sql(source), ") ",
      "SELECT count(*) FILTER (WHERE missing)::text AS n_missing, ",
      "count(*) FILTER (WHERE NOT missing)::text AS n_observed, ",
      "count(DISTINCT value) FILTER (WHERE NOT missing)::text AS n_unique FROM v"
    ),
    params = contract$params,
    query_kind = "variable_counts",
    limit = 1L,
    timing_env = timing_env,
    variable_index = index,
    name = column$name[[1]]
  )
  c(
    n_missing = eda_checked_count(observed$n_missing[[1]], "PostgreSQL missing count"),
    n_observed = eda_checked_count(observed$n_observed[[1]], "PostgreSQL observed count"),
    n_unique = eda_checked_count(observed$n_unique[[1]], "PostgreSQL distinct count")
  )
}

eda_postgres_numeric_summary <- function(source, column, contract, index, timing_env) {
  expression <- eda_postgres_value_expression(source, column, "numeric")
  finite <- "value NOT IN ('Infinity'::double precision, '-Infinity'::double precision)"
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
      eda_postgres_table_sql(source), ") ",
      "SELECT count(*) FILTER (WHERE missing)::text AS n_missing, ",
      "count(*) FILTER (WHERE NOT missing)::text AS n_observed, ",
      "count(DISTINCT value) FILTER (WHERE NOT missing)::text AS n_unique, ",
      "count(*) FILTER (WHERE NOT missing AND NOT (", finite, "))::text AS n_infinite, ",
      "count(*) FILTER (WHERE NOT missing AND ", finite, ")::text AS n_finite, ",
      "sum(value) FILTER (WHERE NOT missing AND ", finite, ") AS sum, ",
      "min(value) FILTER (WHERE NOT missing AND ", finite, ") AS min, ",
      "percentile_cont(0.25) WITHIN GROUP (ORDER BY value) FILTER (WHERE NOT missing AND ", finite, ") AS q1, ",
      "avg(value) FILTER (WHERE NOT missing AND ", finite, ") AS mean, ",
      "percentile_cont(0.5) WITHIN GROUP (ORDER BY value) FILTER (WHERE NOT missing AND ", finite, ") AS median, ",
      "percentile_cont(0.75) WITHIN GROUP (ORDER BY value) FILTER (WHERE NOT missing AND ", finite, ") AS q3, ",
      "max(value) FILTER (WHERE NOT missing AND ", finite, ") AS max, ",
      "stddev_samp(value) FILTER (WHERE NOT missing AND ", finite, ") AS sd, ",
      "var_samp(value) FILTER (WHERE NOT missing AND ", finite, ") AS variance ",
      "FROM v"
    ),
    params = contract$params,
    query_kind = "numeric_first_pass",
    limit = 1L,
    timing_env = timing_env,
    variable_index = index,
    name = column$name[[1]]
  )
  counts <- lapply(observed[c("n_missing", "n_observed", "n_unique", "n_infinite", "n_finite")], eda_checked_count)
  n_finite <- counts$n_finite
  numbers <- lapply(observed[c("sum", "min", "q1", "mean", "median", "q3", "max", "sd", "variance")], function(value) {
    if (length(value) == 0L || is.na(value[[1]])) NA_real_ else as.numeric(value[[1]])
  })
  iqr <- if (n_finite > 0L) numbers$q3 - numbers$q1 else NA_real_
  lower <- if (n_finite > 0L) numbers$q1 - 1.5 * iqr else NA_real_
  upper <- if (n_finite > 0L) numbers$q3 + 1.5 * iqr else NA_real_
  moment <- c(m2 = NA_real_, m3 = NA_real_, m4 = NA_real_)
  if (n_finite > 0L) {
    moment_row <- eda_db_fetch(
      source$con,
      paste0(
        "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
        eda_postgres_table_sql(source), ") ",
        "SELECT sum(power(value - $", length(contract$params) + 1L, ", 2)) AS m2, ",
        "sum(power(value - $", length(contract$params) + 1L, ", 3)) AS m3, ",
        "sum(power(value - $", length(contract$params) + 1L, ", 4)) AS m4 ",
        "FROM v WHERE NOT missing AND ", finite
      ),
      params = c(contract$params, list(numbers$mean)),
      query_kind = "numeric_moments",
      limit = 1L,
      timing_env = timing_env,
      variable_index = index,
      name = column$name[[1]]
    )
    moment <- vapply(moment_row[c("m2", "m3", "m4")], function(value) as.numeric(value[[1]]), numeric(1))
  }
  fences <- c(n_below = 0L, n_above = 0L)
  if (n_finite > 0L) {
    lower_index <- length(contract$params) + 1L
    upper_index <- lower_index + 1L
    fence_row <- eda_db_fetch(
      source$con,
      paste0(
        "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
        eda_postgres_table_sql(source), ") ",
        "SELECT count(*) FILTER (WHERE value < $", lower_index, ")::text AS n_below, ",
        "count(*) FILTER (WHERE value > $", upper_index, ")::text AS n_above ",
        "FROM v WHERE NOT missing AND ", finite
      ),
      params = c(contract$params, list(lower, upper)),
      query_kind = "numeric_fences",
      limit = 1L,
      timing_env = timing_env,
      variable_index = index,
      name = column$name[[1]]
    )
    fences <- c(
      n_below = eda_checked_count(fence_row$n_below[[1]], "PostgreSQL lower outlier count"),
      n_above = eda_checked_count(fence_row$n_above[[1]], "PostgreSQL upper outlier count")
    )
  }
  has_variation <- n_finite >= 2L && !is.na(numbers$sd) && numbers$sd > 0
  skewness <- kurtosis <- NA_real_
  if (n_finite >= 3L && has_variation) {
    population_sd <- sqrt(moment[["m2"]] / n_finite)
    correction <- 1 - 1 / n_finite
    skewness <- ((moment[["m3"]] / n_finite) / population_sd^3) * correction^(3 / 2)
    kurtosis <- ((moment[["m4"]] / n_finite) / population_sd^4) * correction^2 - 3
  }
  shapiro <- NA_real_
  if (n_finite > 3L && n_finite < 5000L && has_variation) {
    vector <- eda_db_fetch(
      source$con,
      paste0(
        "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
        eda_postgres_table_sql(source), ") SELECT value FROM v WHERE NOT missing AND ",
        finite, " ORDER BY value"
      ),
      params = contract$params,
      query_kind = "shapiro_vector",
      limit = 4999L,
      timing_env = timing_env,
      variable_index = index,
      name = column$name[[1]]
    )
    shapiro <- summary_safe_scalar(stats::shapiro.test(as.numeric(vector$value))$p.value)
  }
  outliers <- as.integer(fences[["n_below"]] + fences[["n_above"]])
  data <- data.frame(
    n_finite = as.integer(n_finite), sum = numbers$sum, min = numbers$min,
    q1 = numbers$q1, mean = numbers$mean, median = numbers$median,
    q3 = numbers$q3, max = numbers$max, iqr = iqr, sd = numbers$sd,
    variance = numbers$variance,
    sem = if (n_finite >= 2L) numbers$sd / sqrt(n_finite) else NA_real_,
    cv = if (!is.na(numbers$mean) && numbers$mean != 0) numbers$sd / numbers$mean else NA_real_,
    skewness = skewness, kurtosis = kurtosis, shapiro_p = shapiro,
    lower_fence = lower, upper_fence = upper,
    n_below_lower = as.integer(fences[["n_below"]]),
    n_above_upper = as.integer(fences[["n_above"]]),
    outlier_count = outliers,
    outlier_percentage = summary_safe_proportion(outliers * 100, n_finite),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  list(data = data, counts = counts)
}

eda_pg_categorical_summary <- function(source, column, contract, spec_row, index, timing_env) {
  expression <- eda_postgres_value_expression(source, column, as.character(spec_row$type[[1]]))
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
      eda_postgres_table_sql(source), ") ",
      "SELECT value AS level, count(*)::text AS n FROM v WHERE NOT missing GROUP BY value"
    ),
    params = contract$params,
    query_kind = "categorical_frequency",
    limit = Inf,
    timing_env = timing_env,
    variable_index = index,
    name = column$name[[1]]
  )
  counts <- if (nrow(observed) == 0L) {
    integer()
  } else {
    vapply(
      observed$n, eda_checked_count, integer(1),
      field = "PostgreSQL categorical count"
    )
  }
  names(counts) <- as.character(observed$level)
  declared <- if ("levels" %in% names(spec_row)) eda_spec_levels(spec_row$levels) else character()
  has_declared <- length(declared) > 0L
  unexpected <- sort(setdiff(names(counts), declared), method = "radix")
  levels_out <- if (has_declared) c(declared, unexpected) else sort(names(counts), method = "radix")
  level_counts <- unname(counts[levels_out])
  level_counts[is.na(level_counts)] <- 0L
  n_observed <- sum(level_counts)
  n_total <- eda_postgres_row_count(source, timing_env = timing_env)
  is_declared <- if (has_declared) levels_out %in% declared else rep(NA, length(levels_out))
  data <- data.frame(
    level = levels_out,
    n = as.integer(level_counts),
    p_total = summary_safe_proportion(level_counts, n_total),
    p_observed = summary_safe_proportion(level_counts, n_observed),
    is_declared = is_declared,
    is_unexpected = if (has_declared) !is_declared else rep(FALSE, length(levels_out)),
    stringsAsFactors = FALSE
  )
  list(
    data = data,
    counts = list(
      n_missing = as.integer(n_total - n_observed),
      n_observed = as.integer(n_observed),
      n_unique = as.integer(length(counts)),
      n_infinite = 0L
    )
  )
}

eda_postgres_text_summary <- function(source, column, contract, index, timing_env) {
  expression <- eda_postgres_value_expression(source, column, "text")
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
      eda_postgres_table_sql(source), ") ",
      "SELECT count(*)::text AS n, count(*) FILTER (WHERE missing)::text AS n_missing, ",
      "count(*) FILTER (WHERE NOT missing)::text AS n_observed, ",
      "count(DISTINCT value) FILTER (WHERE NOT missing)::text AS n_unique, ",
      "count(*) FILTER (WHERE NOT missing AND value = '')::text AS n_empty, ",
      "count(*) FILTER (WHERE NOT missing AND value <> '' AND btrim(value, E' \\t\\r\\n') = '')::text AS n_whitespace, ",
      "min(char_length(value)) FILTER (WHERE NOT missing) AS min_length, ",
      "max(char_length(value)) FILTER (WHERE NOT missing) AS max_length FROM v"
    ),
    params = contract$params,
    query_kind = "text_aggregate",
    limit = 1L,
    timing_env = timing_env,
    variable_index = index,
    name = column$name[[1]]
  )
  count_names <- c("n", "n_missing", "n_observed", "n_unique", "n_empty", "n_whitespace")
  counts <- lapply(observed[count_names], eda_checked_count)
  data <- data.frame(
    n = counts$n, n_missing = counts$n_missing, n_observed = counts$n_observed,
    n_unique = counts$n_unique, n_empty = counts$n_empty,
    n_whitespace = counts$n_whitespace,
    min_length = if (is.na(observed$min_length[[1]])) NA_integer_ else as.integer(observed$min_length[[1]]),
    max_length = if (is.na(observed$max_length[[1]])) NA_integer_ else as.integer(observed$max_length[[1]]),
    stringsAsFactors = FALSE
  )
  list(
    data = data,
    counts = list(
      n_missing = counts$n_missing,
      n_observed = counts$n_observed,
      n_unique = counts$n_unique,
      n_infinite = 0L
    )
  )
}

eda_postgres_temporal_summary <- function(source, column, contract, type, index, timing_env) {
  expression <- eda_postgres_value_expression(source, column, type)
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
      eda_postgres_table_sql(source), ") ",
      "SELECT count(*)::text AS n, count(*) FILTER (WHERE missing)::text AS n_missing, ",
      "count(*) FILTER (WHERE NOT missing)::text AS n_observed, ",
      "count(DISTINCT value) FILTER (WHERE NOT missing)::text AS n_unique, ",
      "min(value) FILTER (WHERE NOT missing) AS min, ",
      "percentile_cont(0.25) WITHIN GROUP (ORDER BY value) FILTER (WHERE NOT missing) AS q1, ",
      "percentile_cont(0.5) WITHIN GROUP (ORDER BY value) FILTER (WHERE NOT missing) AS median, ",
      "percentile_cont(0.75) WITHIN GROUP (ORDER BY value) FILTER (WHERE NOT missing) AS q3, ",
      "max(value) FILTER (WHERE NOT missing) AS max FROM v"
    ),
    params = contract$params,
    query_kind = "temporal_aggregate",
    limit = 1L,
    timing_env = timing_env,
    variable_index = index,
    name = column$name[[1]]
  )
  counts <- lapply(observed[c("n", "n_missing", "n_observed", "n_unique")], eda_checked_count)
  quantiles <- vapply(observed[c("min", "q1", "median", "q3", "max")], function(value) {
    if (is.na(value[[1]])) NA_real_ else as.numeric(value[[1]])
  }, numeric(1))
  formatter <- if (type == "date") {
    function(value) ifelse(is.na(value), NA_character_, format(as.Date(value, origin = "1970-01-01"), "%Y-%m-%d"))
  } else {
    summary_format_datetime
  }
  data <- data.frame(
    source_class = if (type == "date") "Date" else "POSIXct/POSIXt",
    timezone = if (type == "date") NA_character_ else "UTC",
    n = counts$n, n_missing = counts$n_missing, n_observed = counts$n_observed,
    n_unique = counts$n_unique,
    min = formatter(quantiles[["min"]]), q1 = formatter(quantiles[["q1"]]),
    median = formatter(quantiles[["median"]]), q3 = formatter(quantiles[["q3"]]),
    max = formatter(quantiles[["max"]]),
    range_value = if (counts$n_observed > 0L) quantiles[["max"]] - quantiles[["min"]] else NA_real_,
    range_unit = if (type == "date") "days" else "seconds",
    stringsAsFactors = FALSE
  )
  list(
    data = data,
    counts = list(
      n_missing = counts$n_missing,
      n_observed = counts$n_observed,
      n_unique = counts$n_unique,
      n_infinite = 0L
    )
  )
}

eda_postgres_integer_exact <- function(source, column, contract, index, timing_env) {
  if (!identical(as.character(column$base_udt_name[[1]]), "int8")) {
    return(TRUE)
  }
  column_sql <- eda_postgres_column_sql(source, column$name[[1]])
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "SELECT max(abs(", column_sql, "::numeric)) FILTER (WHERE NOT ", contract$sql, ")::text AS maximum FROM ",
      eda_postgres_table_sql(source)
    ),
    params = contract$params,
    query_kind = "integer_exactness",
    limit = 1L,
    timing_env = timing_env,
    variable_index = index,
    name = column$name[[1]]
  )
  if (is.na(observed$maximum[[1]])) {
    return(TRUE)
  }
  suppressWarnings(as.numeric(observed$maximum[[1]])) <= 9007199254740991
}

eda_postgres_summaries_inside <- function(source, spec, timing_env = NULL, n_total = NULL) {
  if (is.null(n_total)) n_total <- eda_postgres_row_count(source, timing_env)
  outputs <- list(
    variables = list(), numeric = list(), categorical = list(), text = list(),
    temporal = list(), skipped = list()
  )
  for (index in seq_len(nrow(spec))) {
    row <- spec[index, , drop = FALSE]
    name <- as.character(row$name[[1]])
    label <- as.character(row$label[[1]])
    role <- as.character(row$role[[1]])
    required <- if ("required" %in% names(row)) as.logical(row$required[[1]]) else NA
    type <- as.character(row$type[[1]])
    column <- eda_postgres_column(source, name)
    if (is.null(column)) {
      reason <- missing_variable_reason(required)
      outputs$variables[[length(outputs$variables) + 1L]] <- canonical_variable_row(
        name, label, type, role, required, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, "skipped", reason
      )
      outputs$skipped[[length(outputs$skipped) + 1L]] <- canonical_skipped_row(name, type, NA_character_, reason)
      next
    }
    levels <- if ("levels" %in% names(row)) eda_spec_levels(row$levels) else character()
    compatibility <- eda_pg_type_compatibility(column, type, levels)
    contract <- eda_postgres_missing_contract(source, column, type, eda_missing_codes(spec, name))
    reason <- if (!contract$valid) contract$reason else if (compatibility$status == "incompatible") compatibility$reason else NA_character_
    expression <- eda_postgres_value_expression(source, column, type)
    counts <- tryCatch(
      eda_postgres_basic_counts(source, column, contract, expression, index, timing_env),
      error = function(error) c(n_missing = NA_integer_, n_observed = NA_integer_, n_unique = NA_integer_)
    )
    identifier <- trimws(tolower(role)) %in% c("id", "identifier")
    if (identifier) {
      reason <- "Variable was skipped by the explicit identifier-role policy."
    }
    if (type == "integer" && is.na(reason) && !eda_postgres_integer_exact(source, column, contract, index, timing_env)) {
      reason <- "PostgreSQL bigint values exceed the exact R double integer range."
    }
    if (!is.na(reason)) {
      outputs$variables[[length(outputs$variables) + 1L]] <- canonical_variable_row(
        name, label, type, role, required, n_total, counts[["n_missing"]],
        counts[["n_observed"]], counts[["n_unique"]], 0L, "skipped", reason
      )
      outputs$skipped[[length(outputs$skipped) + 1L]] <- canonical_skipped_row(
        name, type, as.character(column$formatted_type[[1]]), reason
      )
      next
    }
    result <- if (type %in% c("numeric", "integer")) {
      eda_postgres_numeric_summary(source, column, contract, index, timing_env)
    } else if (type %in% c("categorical", "binary")) {
      eda_pg_categorical_summary(source, column, contract, row, index, timing_env)
    } else if (type == "text") {
      eda_postgres_text_summary(source, column, contract, index, timing_env)
    } else {
      eda_postgres_temporal_summary(source, column, contract, type, index, timing_env)
    }
    counts <- result$counts
    n_infinite <- if (type %in% c("numeric", "integer")) counts$n_infinite else 0L
    outputs$variables[[length(outputs$variables) + 1L]] <- canonical_variable_row(
      name, label, type, role, required, n_total, counts$n_missing,
      counts$n_observed, counts$n_unique, n_infinite, "summarised", NA_character_
    )
    component <- if (type %in% c("numeric", "integer")) "numeric" else if (type %in% c("categorical", "binary")) "categorical" else if (type == "text") "text" else "temporal"
    outputs[[component]][[length(outputs[[component]]) + 1L]] <- cbind(
      data.frame(name = rep(name, nrow(result$data)), stringsAsFactors = FALSE),
      result$data
    )
  }
  list(
    variables = bind_or_empty(outputs$variables, empty_eda_variables()),
    numeric = bind_or_empty(outputs$numeric, empty_eda_numeric()),
    categorical = bind_or_empty(outputs$categorical, empty_eda_categorical()),
    text = bind_or_empty(outputs$text, empty_eda_text()),
    temporal = bind_or_empty(outputs$temporal, empty_eda_temporal()),
    skipped = bind_or_empty(outputs$skipped, empty_eda_skipped())
  )
}

eda_pg_empty_identifier_qa <- function() {
  data.frame(
    name = character(), observed_type = character(), n = integer(),
    n_missing = integer(), n_observed = integer(), n_distinct = integer(),
    n_repeated_values = integer(), duplicate_excess = integer(),
    max_frequency = integer(), status = character(), reason = character(),
    stringsAsFactors = FALSE
  )
}

eda_pg_identifier_qa_inside <- function(source, spec, timing_env = NULL, n_total = NULL) {
  role <- trimws(tolower(as.character(spec$role)))
  rows <- which(role %in% c("id", "identifier"))
  if (length(rows) == 0L) {
    return(eda_pg_empty_identifier_qa())
  }
  if (is.null(n_total)) n_total <- eda_postgres_row_count(source, timing_env)
  results <- lapply(rows, function(index) {
    name <- spec$name[[index]]
    column <- eda_postgres_column(source, name)
    if (is.null(column)) {
      return(data.frame(
        name = name, observed_type = NA_character_, n = n_total,
        n_missing = NA_integer_, n_observed = NA_integer_, n_distinct = NA_integer_,
        n_repeated_values = NA_integer_, duplicate_excess = NA_integer_,
        max_frequency = NA_integer_, status = "skipped",
        reason = missing_variable_reason(if ("required" %in% names(spec)) spec$required[[index]] else NA),
        stringsAsFactors = FALSE
      ))
    }
    contract <- eda_postgres_missing_contract(
      source, column, spec$type[[index]], eda_missing_codes(spec, name)
    )
    if (!contract$valid) {
      return(data.frame(
        name = name, observed_type = as.character(column$formatted_type[[1]]), n = n_total,
        n_missing = NA_integer_, n_observed = NA_integer_, n_distinct = NA_integer_,
        n_repeated_values = NA_integer_, duplicate_excess = NA_integer_,
        max_frequency = NA_integer_, status = "skipped", reason = contract$reason,
        stringsAsFactors = FALSE
      ))
    }
    expression <- eda_postgres_value_expression(source, column, spec$type[[index]])
    observed <- eda_db_fetch(
      source$con,
      paste0(
        "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
        eda_postgres_table_sql(source), "), f AS (",
        "SELECT value, count(*) AS frequency FROM v WHERE NOT missing GROUP BY value), a AS (",
        "SELECT count(*) FILTER (WHERE frequency > 1)::text AS n_repeated_values, ",
        "COALESCE(sum(frequency - 1) FILTER (WHERE frequency > 1), 0)::text AS duplicate_excess, ",
        "COALESCE(max(frequency), 0)::text AS max_frequency FROM f) ",
        "SELECT count(*) FILTER (WHERE missing)::text AS n_missing, ",
        "count(*) FILTER (WHERE NOT missing)::text AS n_observed, ",
        "count(DISTINCT value) FILTER (WHERE NOT missing)::text AS n_distinct, ",
        "a.n_repeated_values, a.duplicate_excess, a.max_frequency FROM v CROSS JOIN a GROUP BY a.n_repeated_values, a.duplicate_excess, a.max_frequency"
      ),
      params = contract$params,
      query_kind = "identifier_qa",
      limit = 1L,
      timing_env = timing_env,
      variable_index = index,
      name = name
    )
    fields <- c("n_missing", "n_observed", "n_distinct", "n_repeated_values", "duplicate_excess", "max_frequency")
    values <- lapply(observed[fields], eda_checked_count)
    data.frame(
      name = name, observed_type = as.character(column$formatted_type[[1]]), n = n_total,
      n_missing = values$n_missing, n_observed = values$n_observed,
      n_distinct = values$n_distinct, n_repeated_values = values$n_repeated_values,
      duplicate_excess = values$duplicate_excess, max_frequency = values$max_frequency,
      status = "summarised", reason = NA_character_, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, results)
  row.names(out) <- NULL
  out
}

eda_postgres_histogram_counts <- function(source,
                                          column,
                                          contract,
                                          expression,
                                          minimum,
                                          maximum,
                                          index,
                                          name,
                                          timing_env,
                                          finite_only = FALSE) {
  if (is.na(minimum) || is.na(maximum)) {
    return(data.frame(bin = integer(), count = integer(), stringsAsFactors = FALSE))
  }
  lower_index <- length(contract$params) + 1L
  upper_index <- lower_index + 1L
  finite_sql <- if (finite_only) {
    " AND value NOT IN ('Infinity'::double precision, '-Infinity'::double precision)"
  } else {
    ""
  }
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH v AS (SELECT ", expression, " AS value, ", contract$sql, " AS missing FROM ",
      eda_postgres_table_sql(source), "), b AS (SELECT CASE WHEN $", lower_index,
      "::double precision = $", upper_index, "::double precision THEN 15 ELSE ",
      "least(30, greatest(1, width_bucket(value, $", lower_index,
      "::double precision, $", upper_index, "::double precision, 30))) END AS bin ",
      "FROM v WHERE NOT missing", finite_sql, ") ",
      "SELECT bin::integer AS bin, count(*)::text AS count FROM b GROUP BY bin ORDER BY bin"
    ),
    params = c(contract$params, list(as.numeric(minimum), as.numeric(maximum))),
    query_kind = "plot_histogram",
    limit = 30L,
    timing_env = timing_env,
    variable_index = index,
    name = name
  )
  if (nrow(observed) == 0L) {
    return(data.frame(bin = integer(), count = integer(), stringsAsFactors = FALSE))
  }
  data.frame(
    bin = as.integer(observed$bin),
    count = vapply(observed$count, eda_checked_count, integer(1), field = "PostgreSQL histogram count"),
    stringsAsFactors = FALSE
  )
}

eda_postgres_plot_data_inside <- function(source,
                                          spec,
                                          summaries,
                                          max_plot_levels,
                                          timing_env = NULL) {
  entries <- lapply(seq_len(nrow(spec)), function(index) {
    name <- spec$name[[index]]
    type <- spec$type[[index]]
    label <- if (!is.na(spec$label[[index]]) && nzchar(spec$label[[index]])) spec$label[[index]] else name
    variable <- summaries$variables[summaries$variables$name == name, , drop = FALSE]
    if (eda_identifier_role(spec$role[[index]])) {
      return(eda_plot_entry(name, label, type, "identifier", NULL, variable$n, variable$n_missing, 0L, 0L, "not_created", "Variable was skipped by the explicit identifier-role policy."))
    }
    if (nrow(variable) != 1L || variable$status[[1]] != "summarised") {
      reason <- if (nrow(variable) == 1L) variable$reason[[1]] else "Variable summary was unavailable."
      return(eda_plot_entry(name, label, type, "not_created", NULL, if (nrow(variable)) variable$n[[1]] else NA_integer_, if (nrow(variable)) variable$n_missing[[1]] else NA_integer_, 0L, 0L, "not_created", reason))
    }
    column <- eda_postgres_column(source, name)
    contract <- eda_postgres_missing_contract(source, column, type, eda_missing_codes(spec, name))
    if (type %in% c("categorical", "binary")) {
      frequency <- summaries$categorical[summaries$categorical$name == name, , drop = FALSE]
      compact <- eda_collapse_frequencies(frequency[, setdiff(names(frequency), "name"), drop = FALSE], max_plot_levels)
      entry <- eda_plot_entry(name, label, type, "frequency", compact, variable$n, variable$n_missing, variable$n_observed, 0L)
      entry$n_displayed_levels <- nrow(compact)
      entry$n_collapsed_levels <- max(0L, nrow(frequency) - min(nrow(frequency), max_plot_levels))
      return(entry)
    }
    if (type %in% c("numeric", "integer")) {
      summary <- summaries$numeric[summaries$numeric$name == name, , drop = FALSE]
      minimum <- summary$min[[1]]
      maximum <- summary$max[[1]]
      expression <- eda_postgres_value_expression(source, column, type)
      counts <- eda_postgres_histogram_counts(source, column, contract, expression, minimum, maximum, index, name, timing_env, TRUE)
      entry <- eda_plot_entry(name, label, type, "histogram", eda_histogram_from_counts(minimum, maximum, counts), variable$n, variable$n_missing, summary$n_finite[[1]], variable$n_infinite[[1]])
      entry$box_data <- data.frame(
        min = summary$min, q1 = summary$q1, median = summary$median,
        q3 = summary$q3, max = summary$max,
        lower_fence = summary$lower_fence, upper_fence = summary$upper_fence,
        n_below_lower = summary$n_below_lower,
        n_above_upper = summary$n_above_upper,
        stringsAsFactors = FALSE
      )
      return(entry)
    }
    if (type == "text") {
      summary <- summaries$text[summaries$text$name == name, , drop = FALSE]
      expression <- paste0("char_length(", eda_postgres_value_expression(source, column, type), ")::double precision")
      minimum <- as.numeric(summary$min_length[[1]])
      maximum <- as.numeric(summary$max_length[[1]])
      counts <- eda_postgres_histogram_counts(source, column, contract, expression, minimum, maximum, index, name, timing_env)
      return(eda_plot_entry(name, label, type, "text_length", eda_histogram_from_counts(minimum, maximum, counts), variable$n, variable$n_missing, variable$n_observed, 0L))
    }
    summary <- summaries$temporal[summaries$temporal$name == name, , drop = FALSE]
    minimum <- if (type == "date") as.numeric(as.Date(summary$min[[1]])) else as.numeric(summary_parse_datetime_chr(summary$min[[1]]))
    maximum <- if (type == "date") as.numeric(as.Date(summary$max[[1]])) else as.numeric(summary_parse_datetime_chr(summary$max[[1]]))
    expression <- eda_postgres_value_expression(source, column, type)
    counts <- eda_postgres_histogram_counts(source, column, contract, expression, minimum, maximum, index, name, timing_env)
    eda_plot_entry(name, label, type, "temporal", eda_histogram_from_counts(minimum, maximum, counts), variable$n, variable$n_missing, variable$n_observed, 0L)
  })
  names(entries) <- spec$name
  list(entries = entries, inventory = eda_plot_inventory(entries))
}
