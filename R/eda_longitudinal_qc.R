#' Audit longitudinal population membership and record keys in PostgreSQL
#'
#' Compare aggregate entity membership across explicitly ordered completed
#' periods and, when declared, inspect within-period record-key uniqueness.
#' Entity identifiers and record-key values remain inside PostgreSQL.
#'
#' @param sources A uniquely named list of at least two unmodified
#'   [epi_eda_postgres_source()] objects that share one caller-owned
#'   connection. List order defines period order.
#' @param entity_id One column name present in every source. Supported common
#'   PostgreSQL families are text/varchar, integral, and UUID.
#' @param record_key `NULL`, or one or more unique column names that together
#'   define within-period record uniqueness. Only complete keys are included
#'   in distinct and duplicate calculations.
#'
#' @return An `epi_eda_longitudinal_qc` list with value-free `metadata`,
#'   `period_summary`, `adjacent_membership`, `pairwise_overlap`,
#'   `history_summary`, and typed `issues` components.
#'
#' @details All sources are validated before and inside one `REPEATABLE READ
#' READ ONLY` transaction. Null entity identifiers and blank text
#' representations are excluded from membership. Counts are returned as
#' doubles only after exact decimal results are checked not to exceed
#' `2^53 - 1`; proportions are `NA_real_` when their named denominator is
#' zero. Population changes, reappearance, and repeated entity rows are
#' descriptive and do not create findings. The only findings are warnings for
#' an empty period, invalid entity identifiers, missing record keys, or
#' duplicate complete record keys.
#'
#' The operation is aggregate-only, creates no database objects, and leaves
#' the connection open and idle. Any validation, snapshot, query, count-range,
#' or reconciliation failure is a hard error and returns no partial result.
#'
#' Entity columns must share one text/varchar, integral, or UUID family; text
#' collations must be deterministic. Every declared record-key column must use
#' the same base type in every source. Supported key base types are boolean,
#' integral, floating-point and numeric types, text/varchar/bpchar, date,
#' timestamp/timestamptz, time/timetz, and UUID; collatable key columns must use
#' deterministic equality.
#'
#' @section Result schema:
#' `metadata` has `contract_version`, integer `n_periods`, `entity_id`,
#' `record_key_declared`, integer `n_record_key_columns`, machine-readable
#' ordered `period_labels` and `source_fingerprints` list columns, and
#' `source_set_fingerprint_sha256`.
#'
#' `period_summary` has integer `period_index`, `period_label`, and double
#' `n_rows`, `n_entity_null`, `n_entity_blank`, `n_entity_nonblank`,
#' `n_valid_entity_rows`,
#' `n_distinct_entities`, `n_repeated_entity_rows`,
#' `n_repeated_entity_excess`, `max_entity_frequency`,
#' `n_missing_record_key`, `n_complete_record_key_rows`,
#' `n_distinct_record_keys`, `n_duplicate_record_key_groups`,
#' `n_duplicate_record_key_rows`, `n_duplicate_record_key_excess`, and
#' `max_record_key_frequency`. Record-key fields are `NA_real_` when no key is
#' declared.
#'
#' `adjacent_membership` has integer `from_period_index` and `to_period_index`,
#' their labels, double entity counts `n_from_entities`, `n_to_entities`,
#' `n_union`, `n_retained`, `n_exited`, and `n_entered`, explicit
#' `retention_numerator` and `entry_numerator`, and explicit double retention,
#' exit, and entry denominator/proportion pairs.
#'
#' `pairwise_overlap` has integer `left_period_index` and
#' `right_period_index`, their labels, double `n_left_entities`,
#' `n_right_entities`, `n_overlap`, `n_union`, `n_left_only`, and `n_right_only`, and
#' explicit double left/right overlap denominator/proportion pairs.
#'
#' `history_summary` has integer first/last positions and their labels, integer
#' `periods_observed` and `gap_periods`, logical `has_gap`, and double
#' `n_entities`, `proportion_denominator`, and `proportion`.
#'
#' `issues` always has character `issue_code` and `severity`, integer
#' `period_index`, character `period` and `variable`, double `n_affected`,
#' and character `message`, including in its typed zero-row form.
#'
#' @family EDA
#' @export
epi_eda_longitudinal_qc <- function(sources,
                                    entity_id,
                                    record_key = NULL) {
  inputs <- longitudinal_qc_inputs(sources, entity_id, record_key)
  eda_longitudinal_transaction(inputs$sources, {
    context <- longitudinal_qc_context(
      inputs$sources, inputs$period_labels, inputs$entity_id,
      inputs$record_key
    )
    period_summary <- longitudinal_qc_period_summary(context)
    pairwise_overlap <- longitudinal_qc_pairwise(context, period_summary)
    adjacent_membership <- longitudinal_qc_adjacent(
      pairwise_overlap, context$period_labels
    )
    history_summary <- longitudinal_qc_history(context)
    issues <- longitudinal_qc_issues(period_summary, inputs$entity_id, inputs$record_key)
    structure(
      list(
        metadata = longitudinal_qc_metadata(context),
        period_summary = period_summary,
        adjacent_membership = adjacent_membership,
        pairwise_overlap = pairwise_overlap,
        history_summary = history_summary,
        issues = issues
      ),
      class = c("epi_eda_longitudinal_qc", "list")
    )
  }, operation = "population QC")
}

#' @export
print.epi_eda_longitudinal_qc <- function(x, ...) {
  cat("<epi_eda_longitudinal_qc>\n")
  cat("  Periods: ", nrow(x$period_summary), "\n", sep = "")
  cat("  Technical warning findings: ", nrow(x$issues), "\n", sep = "")
  cat("  Entity and record-key values: not returned\n")
  invisible(x)
}

longitudinal_qc_inputs <- function(sources, entity_id, record_key) {
  valid_list <- is.list(sources) && !is.data.frame(sources) &&
    length(sources) >= 2L && !is.null(names(sources))
  if (!valid_list) {
    stop("sources must be a named list of at least two PostgreSQL EDA sources.", call. = FALSE)
  }
  period_labels <- names(sources)
  valid_labels <- is.character(period_labels) && !anyNA(period_labels) &&
    all(nzchar(trimws(period_labels))) && !any(grepl("[\r\n\t]", period_labels)) &&
    !anyDuplicated(period_labels)
  if (!valid_labels) {
    stop("sources names must be unique, non-empty period labels without control characters.", call. = FALSE)
  }
  valid_sources <- vapply(
    sources,
    function(source) identical(class(source), c("epi_eda_postgres_source", "list")),
    logical(1)
  )
  if (!all(valid_sources)) {
    stop("Every sources element must be an unmodified epi_eda_postgres_source object.", call. = FALSE)
  }
  shared_connection <- vapply(
    sources[-1L],
    function(source) identical(source$con, sources[[1L]]$con),
    logical(1)
  )
  if (!all(shared_connection)) {
    stop("Every longitudinal source must share one caller-owned PostgreSQL connection.", call. = FALSE)
  }
  entity_id <- eda_postgres_identifier(entity_id, "entity_id")
  if (!is.null(record_key)) {
    valid_key <- is.character(record_key) && length(record_key) >= 1L &&
      !anyNA(record_key) && !anyDuplicated(record_key)
    if (!valid_key) {
      stop("record_key must be NULL or one or more unique plain column identifiers.", call. = FALSE)
    }
    record_key <- vapply(
      record_key, eda_postgres_identifier, character(1), name = "record_key"
    )
    record_key <- unname(record_key)
  }
  list(
    sources = sources,
    period_labels = period_labels,
    entity_id = entity_id,
    record_key = record_key
  )
}

eda_longitudinal_source_inputs <- function(sources) {
  valid_list <- is.list(sources) && !is.data.frame(sources) && length(sources) >= 2L && !is.null(names(sources))
  if (!valid_list || anyNA(names(sources)) ||
        any(!nzchar(trimws(names(sources)))) ||
        any(grepl("[\r\n\t]", names(sources))) ||
        anyDuplicated(names(sources))) {
    stop("sources must be a uniquely named list of at least two PostgreSQL EDA sources.", call. = FALSE)
  }
  if (!all(vapply(sources, function(source) identical(class(source), c("epi_eda_postgres_source", "list")), logical(1)))) {
    stop("Every sources element must be an unmodified epi_eda_postgres_source object.", call. = FALSE)
  }
  if (!all(vapply(sources[-1L], function(source) identical(source$con, sources[[1L]]$con), logical(1)))) {
    stop("Every longitudinal source must share one caller-owned PostgreSQL connection.", call. = FALSE)
  }
  list(sources = sources, period_labels = names(sources))
}

eda_longitudinal_transaction <- function(sources, code, operation = "operation") {
  for (source in sources) {
    eda_validate_postgres_source(source, require_idle = TRUE)
  }
  con <- sources[[1L]]$con
  eda_db_lifecycle_call(
    eda_db_begin(con),
    paste0("PostgreSQL longitudinal ", operation, " transaction could not begin; review restricted database logs.")
  )
  finished <- FALSE
  on.exit(
    {
      if (!finished && DBI::dbIsValid(con)) {
        try(DBI::dbRollback(con), silent = TRUE)
      }
    },
    add = TRUE
  )
  eda_db_statement(
    con,
    "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY",
    query_kind = "transaction_setup"
  )
  for (source in sources) {
    eda_validate_postgres_source(source, require_idle = FALSE)
  }
  value <- force(code)
  eda_db_lifecycle_call(
    eda_db_commit(con),
    paste0("PostgreSQL longitudinal ", operation, " transaction could not commit safely; review restricted database logs.")
  )
  finished <- TRUE
  value
}

longitudinal_qc_transaction <- function(sources, code) {
  eda_longitudinal_transaction(sources, code, operation = "QC")
}

longitudinal_qc_context <- function(sources,
                                    period_labels,
                                    entity_id,
                                    record_key) {
  entity_columns <- lapply(sources, eda_postgres_column, name = entity_id)
  if (any(vapply(entity_columns, is.null, logical(1)))) {
    stop("entity_id must exist in every longitudinal source.", call. = FALSE)
  }
  entity_families <- vapply(entity_columns, function(column) {
    sec_identifier_family(as.character(column$base_udt_name[[1L]]))
  }, character(1))
  if (anyNA(entity_families) || length(unique(entity_families)) != 1L) {
    stop("entity_id must use one compatible text, integral, or UUID family across all periods.", call. = FALSE)
  }
  if (entity_families[[1L]] == "text" &&
        any(!vapply(entity_columns, longitudinal_qc_deterministic, logical(1)))) {
    stop("Textual entity_id equality requires deterministic PostgreSQL collations.", call. = FALSE)
  }

  key_columns <- vector("list", length(sources))
  if (!is.null(record_key)) {
    key_columns <- lapply(sources, function(source) {
      lapply(record_key, function(name) eda_postgres_column(source, name))
    })
    if (any(vapply(
      key_columns,
      function(columns) any(vapply(columns, is.null, logical(1))),
      logical(1)
    ))) {
      stop("Every record_key column must exist in every longitudinal source.", call. = FALSE)
    }
    allowed <- sec_comparable_udt_names()
    for (key_index in seq_along(record_key)) {
      columns <- lapply(key_columns, `[[`, key_index)
      base_types <- vapply(columns, function(column) {
        as.character(column$base_udt_name[[1L]])
      }, character(1))
      if (any(!base_types %in% allowed) || length(unique(base_types)) != 1L) {
        stop("Each record_key column must use one supported PostgreSQL base type across all periods.", call. = FALSE)
      }
      if (base_types[[1L]] %in% c("text", "varchar", "bpchar") &&
            any(!vapply(columns, longitudinal_qc_deterministic, logical(1)))) {
        stop("Textual record_key equality requires deterministic PostgreSQL collations.", call. = FALSE)
      }
    }
  }

  list(
    sources = sources,
    period_labels = period_labels,
    entity_id = entity_id,
    entity_family = entity_families[[1L]],
    entity_columns = entity_columns,
    record_key = record_key,
    key_columns = key_columns
  )
}

longitudinal_qc_deterministic <- function(column) {
  isTRUE(column$collation_deterministic[[1L]])
}

longitudinal_qc_metadata <- function(context) {
  source_fingerprints <- unname(vapply(
    context$sources, eda_pg_source_fingerprint, character(1)
  ))
  source_contract <- list(
    period_labels = context$period_labels,
    source_fingerprints = source_fingerprints,
    entity_id = context$entity_id,
    record_key = context$record_key,
    contract_version = "longitudinal-qc-1"
  )
  data.frame(
    contract_version = "longitudinal-qc-1",
    n_periods = as.integer(length(context$sources)),
    entity_id = context$entity_id,
    record_key_declared = !is.null(context$record_key),
    n_record_key_columns = as.integer(length(context$record_key)),
    period_labels = I(list(unname(context$period_labels))),
    source_fingerprints = I(list(source_fingerprints)),
    source_set_fingerprint_sha256 = eda_postgres_fingerprint(source_contract),
    stringsAsFactors = FALSE
  )
}

longitudinal_qc_entity_sql <- function(source, entity_id) {
  column <- eda_postgres_column_sql(source, entity_id)
  paste0("(", column, "::text COLLATE \"C\")")
}

longitudinal_entity_predicate <- function(source,
                                          entity_id,
                                          entity_family) {
  column <- eda_postgres_column_sql(source, entity_id)
  valid <- paste0(column, " IS NOT NULL")
  if (entity_family == "text") {
    valid <- paste0("(", valid, " AND btrim(", column, "::text) <> '')")
  }
  valid
}

longitudinal_key_predicate <- function(source, record_key, key_columns) {
  predicates <- vapply(seq_along(record_key), function(index) {
    column_sql <- eda_postgres_column_sql(source, record_key[[index]])
    base_type <- as.character(key_columns[[index]]$base_udt_name[[1L]])
    predicate <- paste0(column_sql, " IS NOT NULL")
    if (base_type %in% c("text", "varchar", "bpchar")) {
      predicate <- paste0("(", predicate, " AND btrim(", column_sql, "::text) <> '')")
    }
    predicate
  }, character(1))
  paste(predicates, collapse = " AND ")
}

longitudinal_qc_period_query <- function(context, period_index) {
  source <- context$sources[[period_index]]
  con <- source$con
  entity_column <- eda_postgres_column_sql(source, context$entity_id)
  null_alias <- as.character(DBI::dbQuoteIdentifier(con, "__episcout_entity_null"))
  blank_alias <- as.character(DBI::dbQuoteIdentifier(con, "__episcout_entity_blank"))
  blank_predicate <- if (context$entity_family == "text") {
    paste0(entity_column, " IS NOT NULL AND btrim(", entity_column,
           "::text) = ''")
  } else {
    "FALSE"
  }
  entity_alias <- as.character(DBI::dbQuoteIdentifier(con, "__episcout_entity"))
  valid_alias <- as.character(DBI::dbQuoteIdentifier(con, "__episcout_entity_valid"))
  classified_columns <- c(
    paste0(longitudinal_qc_entity_sql(source, context$entity_id), " AS ", entity_alias),
    paste0(entity_column, " IS NULL AS ", null_alias),
    paste0("(", blank_predicate, ") AS ", blank_alias),
    paste0(
      longitudinal_entity_predicate(
        source, context$entity_id, context$entity_family
      ),
      " AS ", valid_alias
    )
  )
  key_cte <- ""
  key_select <- ""
  if (!is.null(context$record_key)) {
    complete_alias <- as.character(DBI::dbQuoteIdentifier(
      con, "__episcout_key_complete"
    ))
    key_aliases <- vapply(seq_along(context$record_key), function(index) {
      as.character(DBI::dbQuoteIdentifier(
        con, paste0("__episcout_key_", index)
      ))
    }, character(1))
    key_values <- vapply(seq_along(context$record_key), function(index) {
      paste0(
        eda_postgres_column_sql(source, context$record_key[[index]]),
        " AS ", key_aliases[[index]]
      )
    }, character(1))
    classified_columns <- c(
      classified_columns,
      paste0(
        longitudinal_key_predicate(
          source, context$record_key, context$key_columns[[period_index]]
        ),
        " AS ", complete_alias
      ),
      key_values
    )
    key_cte <- paste0(
      ", key_frequencies AS (SELECT ", paste(key_aliases, collapse = ", "),
      ", COUNT(*)::bigint AS n FROM classified WHERE ", complete_alias,
      " GROUP BY ", paste(key_aliases, collapse = ", "), ")"
    )
    key_select <- paste0(
      ", COUNT(*) FILTER (WHERE NOT ", complete_alias,
      ")::text AS n_missing_record_key, COUNT(*) FILTER (WHERE ",
      complete_alias,
      ")::text AS n_complete_record_key_rows, (SELECT COUNT(*)::text FROM key_frequencies) AS n_distinct_record_keys, (SELECT COUNT(*)::text FROM key_frequencies WHERE n > 1) AS n_duplicate_record_key_groups, COALESCE((SELECT SUM(n) FROM key_frequencies WHERE n > 1), 0)::text AS n_duplicate_record_key_rows, COALESCE((SELECT SUM(n - 1) FROM key_frequencies WHERE n > 1), 0)::text AS n_duplicate_record_key_excess, COALESCE((SELECT MAX(n) FROM key_frequencies), 0)::text AS max_record_key_frequency"
    )
  }
  paste0(
    "WITH classified AS (SELECT ",
    paste(classified_columns, collapse = ", "), " FROM ",
    eda_postgres_table_sql(source),
    "), entity_frequencies AS (SELECT ", entity_alias,
    ", COUNT(*)::bigint AS n FROM classified WHERE ", valid_alias,
    " GROUP BY ", entity_alias, ")", key_cte,
    " SELECT COUNT(*)::text AS n_rows, COUNT(*) FILTER (WHERE ",
    null_alias, ")::text AS n_entity_null, COUNT(*) FILTER (WHERE ",
    blank_alias, ")::text AS n_entity_blank, COUNT(*) FILTER (WHERE ",
    valid_alias, ")::text AS n_entity_nonblank, COUNT(*) FILTER (WHERE ",
    valid_alias,
    ")::text AS n_valid_entity_rows, (SELECT COUNT(*)::text FROM entity_frequencies) AS n_distinct_entities, COALESCE((SELECT SUM(n) FROM entity_frequencies WHERE n > 1), 0)::text AS n_repeated_entity_rows, COALESCE((SELECT SUM(n - 1) FROM entity_frequencies WHERE n > 1), 0)::text AS n_repeated_entity_excess, COALESCE((SELECT MAX(n) FROM entity_frequencies), 0)::text AS max_entity_frequency",
    key_select, " FROM classified"
  )
}

longitudinal_qc_period_summary <- function(context) {
  rows <- lapply(seq_along(context$sources), function(period_index) {
    expected <- c(
      "n_rows", "n_entity_null", "n_entity_blank", "n_entity_nonblank",
      "n_valid_entity_rows",
      "n_distinct_entities", "n_repeated_entity_rows",
      "n_repeated_entity_excess", "max_entity_frequency"
    )
    if (!is.null(context$record_key)) {
      expected <- c(
        expected, "n_missing_record_key", "n_complete_record_key_rows",
        "n_distinct_record_keys", "n_duplicate_record_key_groups",
        "n_duplicate_record_key_rows", "n_duplicate_record_key_excess",
        "max_record_key_frequency"
      )
    }
    observed <- eda_db_fetch(
      context$sources[[period_index]]$con,
      longitudinal_qc_period_query(context, period_index),
      query_kind = "longitudinal_period_summary",
      limit = 1L
    )
    counts <- longitudinal_qc_count_row(observed, expected)
    if (is.null(context$record_key)) {
      counts[c(
        "n_missing_record_key", "n_complete_record_key_rows",
        "n_distinct_record_keys", "n_duplicate_record_key_groups",
        "n_duplicate_record_key_rows", "n_duplicate_record_key_excess",
        "max_record_key_frequency"
      )] <- NA_real_
    }
    longitudinal_reconcile_period(counts, !is.null(context$record_key))
    data.frame(
      period_index = as.integer(period_index),
      period_label = context$period_labels[[period_index]],
      as.data.frame(as.list(counts), stringsAsFactors = FALSE),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

longitudinal_qc_count_row <- function(observed, expected) {
  if (nrow(observed) != 1L || !identical(names(observed), expected)) {
    stop("PostgreSQL longitudinal QC returned an incomplete aggregate calculation.", call. = FALSE)
  }
  values <- vapply(seq_along(expected), function(index) {
    longitudinal_qc_checked_count(
      observed[[index]][[1L]], paste("longitudinal", expected[[index]])
    )
  }, numeric(1))
  names(values) <- expected
  values
}

longitudinal_qc_checked_count <- function(value,
                                          field = "PostgreSQL longitudinal count") {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !grepl("^[0-9]+$", value)) {
    stop(field, " was not returned as exact non-negative decimal text.", call. = FALSE)
  }
  normalised <- sub("^0+", "", value)
  if (!nzchar(normalised)) normalised <- "0"
  maximum <- "9007199254740991"
  too_large <- nchar(normalised) > nchar(maximum) ||
    (nchar(normalised) == nchar(maximum) && normalised > maximum)
  if (too_large) {
    stop(field, " exceeds the exact base-R double count range.", call. = FALSE)
  }
  numeric_value <- suppressWarnings(as.numeric(normalised))
  if (!is.finite(numeric_value)) {
    stop(field, " could not be converted to an exact base-R double.", call. = FALSE)
  }
  numeric_value
}

longitudinal_reconcile_period <- function(counts, key_declared) {
  entity_valid <- counts[["n_entity_null"]] + counts[["n_entity_blank"]] +
    counts[["n_valid_entity_rows"]] == counts[["n_rows"]] &&
    counts[["n_entity_nonblank"]] == counts[["n_valid_entity_rows"]] &&
    counts[["n_repeated_entity_excess"]] ==
      counts[["n_valid_entity_rows"]] - counts[["n_distinct_entities"]] &&
    counts[["n_repeated_entity_rows"]] >=
      counts[["n_repeated_entity_excess"]] &&
    counts[["n_distinct_entities"]] <= counts[["n_valid_entity_rows"]] &&
    ((counts[["n_distinct_entities"]] == 0 &&
        counts[["max_entity_frequency"]] == 0) ||
       (counts[["n_distinct_entities"]] > 0 &&
          counts[["max_entity_frequency"]] >= 1 &&
          counts[["max_entity_frequency"]] <= counts[["n_valid_entity_rows"]]))
  key_valid <- TRUE
  if (key_declared) {
    key_valid <- counts[["n_missing_record_key"]] +
      counts[["n_complete_record_key_rows"]] == counts[["n_rows"]] &&
      counts[["n_duplicate_record_key_excess"]] ==
        counts[["n_complete_record_key_rows"]] - counts[["n_distinct_record_keys"]] &&
      counts[["n_duplicate_record_key_groups"]] <=
        counts[["n_distinct_record_keys"]] &&
      counts[["n_duplicate_record_key_rows"]] >=
        counts[["n_duplicate_record_key_excess"]] &&
      ((counts[["n_distinct_record_keys"]] == 0 &&
          counts[["max_record_key_frequency"]] == 0) ||
         (counts[["n_distinct_record_keys"]] > 0 &&
            counts[["max_record_key_frequency"]] >= 1 &&
            counts[["max_record_key_frequency"]] <=
              counts[["n_complete_record_key_rows"]]))
  }
  if (!isTRUE(entity_valid) || !isTRUE(key_valid)) {
    stop("PostgreSQL longitudinal QC aggregate counts did not reconcile.", call. = FALSE)
  }
  invisible(TRUE)
}

longitudinal_qc_membership_sql <- function(context) {
  paste(vapply(seq_along(context$sources), function(period_index) {
    source <- context$sources[[period_index]]
    paste0(
      "SELECT ", period_index, "::integer AS period_index, ",
      longitudinal_qc_entity_sql(source, context$entity_id),
      " AS entity_value FROM ", eda_postgres_table_sql(source),
      " WHERE ", longitudinal_entity_predicate(
        source, context$entity_id, context$entity_family
      )
    )
  }, character(1)), collapse = " UNION ALL ")
}

longitudinal_qc_pairwise <- function(context, period_summary) {
  pair_matrix <- utils::combn(seq_along(context$sources), 2L)
  pair_values <- paste0(
    "(", pair_matrix[1L, ], "::integer, ",
    pair_matrix[2L, ], "::integer)"
  )
  query <- paste0(
    "WITH all_memberships AS (", longitudinal_qc_membership_sql(context),
    "), memberships AS (SELECT DISTINCT period_index, entity_value FROM all_memberships), pairs(left_period_index, right_period_index) AS (VALUES ",
    paste(pair_values, collapse = ", "),
    ") SELECT left_period_index, right_period_index, (SELECT COUNT(*)::text FROM memberships WHERE period_index = left_period_index) AS n_left_entities, (SELECT COUNT(*)::text FROM memberships WHERE period_index = right_period_index) AS n_right_entities, (SELECT COUNT(*)::text FROM memberships AS left_membership INNER JOIN memberships AS right_membership ON right_membership.entity_value = left_membership.entity_value WHERE left_membership.period_index = left_period_index AND right_membership.period_index = right_period_index) AS n_overlap, (SELECT COUNT(DISTINCT entity_value)::text FROM memberships WHERE period_index = left_period_index OR period_index = right_period_index) AS n_union FROM pairs ORDER BY left_period_index, right_period_index"
  )
  observed <- eda_db_fetch(
    context$sources[[1L]]$con,
    query,
    query_kind = "longitudinal_pairwise_overlap",
    limit = ncol(pair_matrix)
  )
  expected_names <- c(
    "left_period_index", "right_period_index", "n_left_entities",
    "n_right_entities", "n_overlap", "n_union"
  )
  observed_pairs <- cbind(
    as.integer(observed$left_period_index),
    as.integer(observed$right_period_index)
  )
  complete <- nrow(observed) == ncol(pair_matrix) &&
    identical(names(observed), expected_names) &&
    identical(observed_pairs, t(pair_matrix))
  if (!complete) {
    stop("PostgreSQL longitudinal QC returned incomplete pairwise calculations.", call. = FALSE)
  }
  rows <- lapply(seq_len(nrow(observed)), function(index) {
    counts <- longitudinal_qc_count_row(
      observed[index, 3:6, drop = FALSE], expected_names[3:6]
    )
    left <- observed_pairs[index, 1L]
    right <- observed_pairs[index, 2L]
    left_matches <- counts[["n_left_entities"]] ==
      period_summary$n_distinct_entities[[left]]
    right_matches <- counts[["n_right_entities"]] ==
      period_summary$n_distinct_entities[[right]]
    overlap_valid <- counts[["n_overlap"]] <= min(
      counts[["n_left_entities"]], counts[["n_right_entities"]]
    )
    union_valid <- counts[["n_union"]] >= max(
      counts[["n_left_entities"]], counts[["n_right_entities"]]
    ) &&
      counts[["n_union"]] - counts[["n_left_entities"]] ==
        counts[["n_right_entities"]] - counts[["n_overlap"]] &&
      counts[["n_union"]] - counts[["n_right_entities"]] ==
        counts[["n_left_entities"]] - counts[["n_overlap"]]
    if (!left_matches || !right_matches || !overlap_valid || !union_valid) {
      stop("PostgreSQL longitudinal QC pairwise counts did not reconcile.", call. = FALSE)
    }
    data.frame(
      left_period_index = as.integer(left),
      left_period_label = context$period_labels[[left]],
      right_period_index = as.integer(right),
      right_period_label = context$period_labels[[right]],
      n_left_entities = counts[["n_left_entities"]],
      n_right_entities = counts[["n_right_entities"]],
      n_overlap = counts[["n_overlap"]],
      n_union = counts[["n_union"]],
      n_left_only = counts[["n_left_entities"]] - counts[["n_overlap"]],
      n_right_only = counts[["n_right_entities"]] - counts[["n_overlap"]],
      left_overlap_denominator = counts[["n_left_entities"]],
      left_overlap_proportion = longitudinal_qc_proportion(
        counts[["n_overlap"]], counts[["n_left_entities"]]
      ),
      right_overlap_denominator = counts[["n_right_entities"]],
      right_overlap_proportion = longitudinal_qc_proportion(
        counts[["n_overlap"]], counts[["n_right_entities"]]
      ),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

longitudinal_qc_proportion <- function(numerator, denominator) {
  if (denominator == 0) NA_real_ else numerator / denominator
}

longitudinal_qc_adjacent <- function(pairwise_overlap, period_labels) {
  adjacent <- pairwise_overlap[
    pairwise_overlap$right_period_index ==
      pairwise_overlap$left_period_index + 1L,
    ,
    drop = FALSE
  ]
  if (nrow(adjacent) != length(period_labels) - 1L) {
    stop("PostgreSQL longitudinal QC returned incomplete adjacent calculations.", call. = FALSE)
  }
  result <- data.frame(
    from_period_index = as.integer(adjacent$left_period_index),
    from_period_label = adjacent$left_period_label,
    to_period_index = as.integer(adjacent$right_period_index),
    to_period_label = adjacent$right_period_label,
    n_from_entities = adjacent$n_left_entities,
    n_to_entities = adjacent$n_right_entities,
    n_union = adjacent$n_union,
    n_retained = adjacent$n_overlap,
    n_exited = adjacent$n_left_only,
    n_entered = adjacent$n_right_only,
    retention_numerator = adjacent$n_overlap,
    retention_denominator = adjacent$n_left_entities,
    retention_proportion = vapply(seq_len(nrow(adjacent)), function(index) {
      longitudinal_qc_proportion(
        adjacent$n_overlap[[index]], adjacent$n_left_entities[[index]]
      )
    }, numeric(1)),
    exit_denominator = adjacent$n_left_entities,
    exit_proportion = vapply(seq_len(nrow(adjacent)), function(index) {
      longitudinal_qc_proportion(
        adjacent$n_left_only[[index]], adjacent$n_left_entities[[index]]
      )
    }, numeric(1)),
    entry_numerator = adjacent$n_right_only,
    entry_denominator = adjacent$n_right_entities,
    entry_proportion = vapply(seq_len(nrow(adjacent)), function(index) {
      longitudinal_qc_proportion(
        adjacent$n_right_only[[index]], adjacent$n_right_entities[[index]]
      )
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL
  result
}

longitudinal_qc_history <- function(context) {
  query <- paste0(
    "WITH all_memberships AS (", longitudinal_qc_membership_sql(context),
    "), memberships AS (SELECT DISTINCT period_index, entity_value FROM all_memberships), histories AS (SELECT entity_value, MIN(period_index)::integer AS first_period_index, MAX(period_index)::integer AS last_period_index, COUNT(*)::integer AS periods_observed FROM memberships GROUP BY entity_value), totals AS (SELECT COUNT(*)::text AS n_total FROM histories), summary AS (SELECT first_period_index, last_period_index, periods_observed, (last_period_index - first_period_index + 1 - periods_observed)::integer AS gap_periods, COUNT(*)::text AS n_entities FROM histories GROUP BY first_period_index, last_period_index, periods_observed) SELECT first_period_index, last_period_index, periods_observed, gap_periods, n_entities, totals.n_total AS proportion_denominator FROM summary CROSS JOIN totals ORDER BY first_period_index, last_period_index, periods_observed"
  )
  observed <- eda_db_fetch(
    context$sources[[1L]]$con,
    query,
    query_kind = "longitudinal_history_summary",
    limit = longitudinal_history_limit(length(context$sources))
  )
  expected <- c(
    "first_period_index", "last_period_index", "periods_observed",
    "gap_periods", "n_entities", "proportion_denominator"
  )
  if (!identical(names(observed), expected)) {
    stop("PostgreSQL longitudinal QC returned incomplete history calculations.", call. = FALSE)
  }
  if (nrow(observed) == 0L) return(longitudinal_qc_empty_history())
  rows <- lapply(seq_len(nrow(observed)), function(index) {
    positions <- vapply(observed[index, 1:4, drop = FALSE], function(value) {
      suppressWarnings(as.integer(value[[1L]]))
    }, integer(1))
    first <- positions[["first_period_index"]]
    last <- positions[["last_period_index"]]
    periods <- positions[["periods_observed"]]
    gaps <- positions[["gap_periods"]]
    valid_positions <- !anyNA(positions) && first >= 1L &&
      last <= length(context$period_labels) && last >= first &&
      periods >= 1L && periods <= last - first + 1L &&
      gaps == last - first + 1L - periods
    if (!valid_positions) {
      stop("PostgreSQL longitudinal QC history positions did not reconcile.", call. = FALSE)
    }
    counts <- longitudinal_qc_count_row(
      observed[index, 5:6, drop = FALSE], expected[5:6]
    )
    data.frame(
      first_period_index = as.integer(first),
      first_period_label = context$period_labels[[first]],
      last_period_index = as.integer(last),
      last_period_label = context$period_labels[[last]],
      periods_observed = as.integer(periods),
      gap_periods = as.integer(gaps),
      has_gap = gaps > 0L,
      n_entities = counts[["n_entities"]],
      proportion_denominator = counts[["proportion_denominator"]],
      proportion = longitudinal_qc_proportion(
        counts[["n_entities"]], counts[["proportion_denominator"]]
      ),
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  denominators <- unique(result$proportion_denominator)
  if (length(denominators) != 1L ||
        sum(result$n_entities) != denominators[[1L]]) {
    stop("PostgreSQL longitudinal QC history counts did not reconcile.", call. = FALSE)
  }
  result
}

longitudinal_history_limit <- function(n_periods) {
  possible_groups <- choose(as.numeric(n_periods) + 2, 3)
  as.integer(min(possible_groups, .Machine$integer.max))
}

longitudinal_qc_empty_history <- function() {
  data.frame(
    first_period_index = integer(),
    first_period_label = character(),
    last_period_index = integer(),
    last_period_label = character(),
    periods_observed = integer(),
    gap_periods = integer(),
    has_gap = logical(),
    n_entities = numeric(),
    proportion_denominator = numeric(),
    proportion = numeric(),
    stringsAsFactors = FALSE
  )
}

longitudinal_qc_empty_issues <- function() {
  data.frame(
    issue_code = character(),
    severity = character(),
    period_index = integer(),
    period = character(),
    variable = character(),
    n_affected = numeric(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

longitudinal_qc_issue <- function(period_summary,
                                  period_index,
                                  issue_code,
                                  variable,
                                  n_affected,
                                  message) {
  data.frame(
    issue_code = issue_code,
    severity = "warning",
    period_index = as.integer(period_index),
    period = period_summary$period_label[[period_index]],
    variable = variable,
    n_affected = as.numeric(n_affected),
    message = message,
    stringsAsFactors = FALSE
  )
}

longitudinal_qc_issues <- function(period_summary, entity_id = "entity_id", record_key = NULL) {
  issues <- longitudinal_qc_empty_issues()
  for (period_index in seq_len(nrow(period_summary))) {
    period <- period_summary[period_index, , drop = FALSE]
    definitions <- list(
      list(
        period$n_rows[[1L]] == 0, "empty_period", NA_character_, 0,
        "The period contains no rows."
      ),
      list(
        period$n_entity_null[[1L]] + period$n_entity_blank[[1L]] > 0, "invalid_entity_id",
        entity_id, period$n_entity_null[[1L]] + period$n_entity_blank[[1L]],
        "Entity identifiers are null or have an applicable blank representation."
      ),
      list(
        !is.na(period$n_missing_record_key[[1L]]) &&
          period$n_missing_record_key[[1L]] > 0,
        "missing_record_key", paste(record_key, collapse = ","),
        period$n_missing_record_key[[1L]],
        "Declared record keys are incomplete."
      ),
      list(
        !is.na(period$n_duplicate_record_key_excess[[1L]]) &&
          period$n_duplicate_record_key_excess[[1L]] > 0,
        "duplicate_record_key", paste(record_key, collapse = ","),
        period$n_duplicate_record_key_excess[[1L]],
        "Complete declared record keys are duplicated within the period."
      )
    )
    for (definition in definitions) {
      if (isTRUE(definition[[1L]])) {
        issues <- rbind(
          issues,
          longitudinal_qc_issue(
            period_summary, period_index, definition[[2L]], definition[[3L]],
            definition[[4L]], definition[[5L]]
          )
        )
      }
    }
  }
  rownames(issues) <- NULL
  issues
}
