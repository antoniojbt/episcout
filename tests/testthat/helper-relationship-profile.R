library(DBI)

if (!methods::isClass("RelationshipProfileMockConnection")) {
  methods::setClass(
    "RelationshipProfileMockConnection",
    contains = "DBIConnection",
    slots = c(state = "environment")
  )
}

methods::setMethod(
  "dbIsValid",
  "RelationshipProfileMockConnection",
  function(dbObj, ...) { # nolint: object_name_linter. DBI generic argument.
    dbObj@state$valid
  }
)

methods::setMethod(
  "dbGetInfo",
  "RelationshipProfileMockConnection",
  function(dbObj, ...) { # nolint: object_name_linter. DBI generic argument.
    list(dbms.name = "PostgreSQL")
  }
)

methods::setMethod(
  "dbGetQuery",
  signature(
    conn = "RelationshipProfileMockConnection",
    statement = "character"
  ),
  function(conn, statement, ...) {
    conn@state$queries <- c(conn@state$queries, statement)
    markers <- c(
      preflight = "episcout_relationship_preflight",
      mappings = "episcout_relationship_mappings"
    )
    matched <- names(markers)[vapply(
      markers,
      grepl,
      logical(1),
      x = statement,
      fixed = TRUE
    )]
    if (length(matched) != 1L) {
      stop("Unexpected relationship mock query: ", statement)
    }
    kind <- matched[[1]]
    conn@state$indices[[kind]] <- conn@state$indices[[kind]] + 1L
    result <- conn@state$results[[kind]][[conn@state$indices[[kind]]]]
    if (kind == "mappings" &&
          !"observed_combinations" %in% names(result)) {
      result$observed_combinations <- rep(as.numeric(nrow(result)), nrow(result))
    }
    result
  }
)

methods::setMethod(
  "dbQuoteIdentifier",
  signature(conn = "RelationshipProfileMockConnection", x = "Id"),
  function(conn, x, ...) {
    DBI::SQL(paste(sprintf('"%s"', x@name), collapse = "."))
  }
)

methods::setMethod(
  "dbQuoteIdentifier",
  signature(
    conn = "RelationshipProfileMockConnection",
    x = "character"
  ),
  function(conn, x, ...) DBI::SQL(sprintf('"%s"', x))
)

relationship_mock_connection <- function(preflights,
                                         mappings = NULL) {
  if (is.null(mappings)) {
    mappings <- rep(
      list(data.frame(
        left_value = character(),
        right_value = character(),
        n = numeric(),
        stringsAsFactors = FALSE
      )),
      length(preflights)
    )
  }
  state <- new.env(parent = emptyenv())
  state$valid <- TRUE
  state$queries <- character()
  state$results <- list(
    preflight = preflights,
    mappings = mappings
  )
  state$indices <- list(preflight = 0L, mappings = 0L)
  methods::new("RelationshipProfileMockConnection", state = state)
}

relationship_preflight_result <- function(total_rows = NULL,
                                          both_present = 0,
                                          left_missing = 0,
                                          right_missing = 0,
                                          both_missing = 0,
                                          distinct_left = 0,
                                          distinct_right = 0,
                                          distinct_combinations = 0,
                                          max_right_per_left = 0,
                                          max_left_per_right = 0,
                                          left_values_with_multiple_right = 0, # nolint: object_length_linter.
                                          right_values_with_multiple_left = 0) { # nolint: object_length_linter.
  if (is.null(total_rows)) {
    total_rows <- both_present + left_missing + right_missing + both_missing
  }
  data.frame(
    total_rows = total_rows,
    both_present = both_present,
    left_missing = left_missing,
    right_missing = right_missing,
    both_missing = both_missing,
    distinct_left = distinct_left,
    distinct_right = distinct_right,
    distinct_combinations = distinct_combinations,
    max_right_per_left = max_right_per_left,
    max_left_per_right = max_left_per_right,
    left_values_with_multiple_right = left_values_with_multiple_right,
    right_values_with_multiple_left = right_values_with_multiple_left,
    stringsAsFactors = FALSE
  )
}

relationship_test_dictionary <- function(columns = letters[1:8],
                                         types = rep("text", length(columns))) {
  inventory_columns <- data.frame(
    source_schema = rep("study", length(columns)),
    source_table = rep("relationship source", length(columns)),
    source_column = columns,
    source_ordinal = seq_along(columns),
    source_data_type = types,
    source_udt_name = types,
    source_is_nullable = rep("YES", length(columns)),
    source_character_maximum_length = rep(NA_real_, length(columns)),
    source_numeric_precision = rep(NA_real_, length(columns)),
    source_numeric_scale = rep(NA_real_, length(columns)),
    source_column_comment = rep(NA_character_, length(columns)),
    stringsAsFactors = FALSE
  )
  inventory <- structure(
    list(
      tables = data.frame(
        source_schema = "study",
        source_table = "relationship source",
        table_type = "BASE TABLE",
        table_comment = NA_character_,
        row_count = NA_real_,
        stringsAsFactors = FALSE
      ),
      columns = inventory_columns,
      constraints = data.frame(
        source_schema = character(),
        source_table = character(),
        constraint_name = character(),
        constraint_type = character(),
        source_columns = character(),
        stringsAsFactors = FALSE
      )
    ),
    class = c("epi_db_inventory", "list")
  )
  epi_eda_dictionary_scaffold(inventory)
}

relationship_test_pairs <- function(left, right) {
  data.frame(
    left_schema = rep("study", length(left)),
    left_table = rep("relationship source", length(left)),
    left_column = left,
    right_schema = rep("study", length(right)),
    right_table = rep("relationship source", length(right)),
    right_column = right,
    stringsAsFactors = FALSE
  )
}
