context("live PostgreSQL explicit-pair Spearman associations")

association_pg_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    skip("Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL integration tests.")
  }
  skip_if_not_installed("RPostgres")
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    dbname = Sys.getenv("PGDATABASE", "synthetic_records"),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "")
  )
}

association_pg_fixture <- function(con) {
  suffix <- paste(sprintf("%02x", as.integer(openssl::rand_bytes(8L))), collapse = "")
  schema <- paste0("association_", suffix)
  relation <- "spearman fixture"
  DBI::dbExecute(
    con,
    paste("CREATE SCHEMA", DBI::dbQuoteIdentifier(con, schema))
  )
  frame <- data.frame(
    tied_x = c(1, 1, 2, 3, 4, rep(NA_real_, 4L)),
    tied_y = c(5, 4, 4, 2, 1, rep(NA_real_, 4L)),
    missing_x = c(1, 2, NA, 999, NaN, Inf, -Inf, 8, 9),
    missing_y = c(1, 2, 3, 4, 5, 6, 7, NA, 999),
    insufficient_left = c(1, rep(NA_real_, 8L)),
    insufficient_right = 1:9,
    constant_left = rep(1, 9L),
    varying_right = 1:9,
    varying_left = 1:9,
    constant_right = rep(2, 9L),
    constant_both_left = rep(3, 9L),
    constant_both_right = rep(4, 9L),
    secret = paste0("runtime_secret_", seq_len(9L), "_", suffix),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = schema, table = relation),
    frame
  )
  spec <- data.frame(
    name = setdiff(names(frame), "secret"),
    label = paste("Label", setdiff(names(frame), "secret")),
    database_type = "numeric",
    analysis_type = "numeric",
    role = "measure",
    missing_codes = ifelse(
      setdiff(names(frame), "secret") %in% c("missing_x", "missing_y"),
      "999",
      ""
    ),
    stringsAsFactors = FALSE
  )
  list(
    schema = schema,
    relation = relation,
    frame = frame[setdiff(names(frame), "secret")],
    spec = spec,
    secrets = frame$secret
  )
}

association_pg_cleanup <- function(con, fixture) {
  if (DBI::dbIsValid(con)) {
    DBI::dbExecute(con, paste0(
      "DROP SCHEMA IF EXISTS ",
      DBI::dbQuoteIdentifier(con, fixture$schema),
      " CASCADE"
    ))
    DBI::dbDisconnect(con)
  }
}

association_pg_pairs <- function() {
  data.frame(
    left = c(
      "tied_x", "missing_x", "insufficient_left", "constant_left",
      "varying_left", "constant_both_left"
    ),
    right = c(
      "tied_y", "missing_y", "insufficient_right", "varying_right",
      "constant_right", "constant_both_right"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("PostgreSQL Spearman aggregates match data-frame truth and reasons", {
  con <- association_pg_connection()
  fixture <- association_pg_fixture(con)
  on.exit(association_pg_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  pairs <- association_pg_pairs()

  database <- epi_eda_profile_spearman(source, fixture$spec, pairs)
  frame <- epi_eda_profile_spearman(fixture$frame, fixture$spec, pairs)

  expect_identical(names(database), names(frame))
  expect_identical(database[c(
    "left", "left_label", "right", "right_label", "n", "status", "reason"
  )], frame[c(
    "left", "left_label", "right", "right_label", "n", "status", "reason"
  )])
  expect_equal(database$rho, frame$rho, tolerance = 1e-14)
  expect_identical(database$n, c(5, 2, 1, 9, 9, 9))
  expect_equal(database$rho[1:2], c(-0.921052631578948, 1), tolerance = 1e-14)
  expect_true(all(is.na(database$rho[3:6])))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_true(DBI::dbIsValid(con))

  result_text <- paste(unlist(database), collapse = "\n")
  print_text <- paste(capture.output(print(database)), collapse = "\n")
  for (secret in fixture$secrets) {
    expect_false(grepl(secret, result_text, fixed = TRUE))
    expect_false(grepl(secret, print_text, fixed = TRUE))
  }
})

test_that("PostgreSQL Spearman uses one aggregate-only repeatable snapshot", {
  con <- association_pg_connection()
  fixture <- association_pg_fixture(con)
  on.exit(association_pg_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  pairs <- association_pg_pairs()[1:2, , drop = FALSE]
  queries <- list()
  setup <- character()
  begins <- commits <- 0L
  original_fetch <- getFromNamespace("eda_db_fetch", "episcout")
  original_statement <- getFromNamespace("eda_db_statement", "episcout")
  original_begin <- getFromNamespace("eda_db_begin", "episcout")
  original_commit <- getFromNamespace("eda_db_commit", "episcout")
  testthat::local_mocked_bindings(
    eda_db_fetch = function(con,
                            statement,
                            params = list(),
                            query_kind,
                            limit,
                            timing_env = NULL,
                            variable_index = NA_integer_,
                            name = NA_character_) {
      if (identical(query_kind, "association_spearman")) {
        queries[[length(queries) + 1L]] <<- list(
          statement = as.character(statement),
          limit = limit
        )
      }
      original_fetch(
        con, statement, params, query_kind, limit, timing_env,
        variable_index, name
      )
    },
    eda_db_statement = function(con, statement, query_kind, timing_env = NULL) {
      setup <<- c(setup, as.character(statement))
      original_statement(con, statement, query_kind, timing_env)
    },
    eda_db_begin = function(con) {
      begins <<- begins + 1L
      original_begin(con)
    },
    eda_db_commit = function(con) {
      commits <<- commits + 1L
      original_commit(con)
    },
    .package = "episcout"
  )

  observed <- epi_eda_profile_spearman(source, fixture$spec, pairs)

  expect_identical(nrow(observed), 2L)
  expect_identical(begins, 1L)
  expect_identical(commits, 1L)
  expect_identical(
    sum(setup == "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY"),
    1L
  )
  expect_length(queries, 2L)
  expect_true(all(vapply(queries, function(query) query$limit == 1L, logical(1))))
  expect_true(all(vapply(queries, function(query) {
    grepl("corr(left_rank, right_rank)", query$statement, fixed = TRUE) &&
      grepl("rank() OVER", query$statement, fixed = TRUE) &&
      grepl("count(DISTINCT", query$statement, fixed = TRUE)
  }, logical(1))))
  for (secret in fixture$secrets) {
    expect_false(any(vapply(
      queries,
      function(query) grepl(secret, query$statement, fixed = TRUE),
      logical(1)
    )))
  }
})

test_that("the complete Spearman API retains its snapshot during a write", {
  con <- association_pg_connection()
  writer <- association_pg_connection()
  fixture <- association_pg_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(writer)) DBI::dbDisconnect(writer)
      association_pg_cleanup(con, fixture)
    },
    add = TRUE
  )
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  pairs <- association_pg_pairs()[1:2, , drop = FALSE]
  original <- getFromNamespace("association_pg_spearman_pair", "episcout")
  target <- paste0(
    DBI::dbQuoteIdentifier(writer, fixture$schema), ".",
    DBI::dbQuoteIdentifier(writer, fixture$relation)
  )
  inserted <- FALSE
  observed <- testthat::with_mocked_bindings(
    epi_eda_profile_spearman(source, fixture$spec, pairs),
    association_pg_spearman_pair = function(source,
                                            spec,
                                            pair,
                                            contracts,
                                            index) {
      value <- original(source, spec, pair, contracts, index)
      if (!inserted) {
        inserted <<- TRUE
        DBI::dbExecute(
          writer,
          paste0(
            "INSERT INTO ", target,
            " (missing_x, missing_y) VALUES (10, 10)"
          )
        )
      }
      value
    },
    .package = "episcout"
  )

  expect_identical(observed$n, c(5, 2))
  later <- epi_eda_profile_spearman(
    source, fixture$spec, pairs[2, , drop = FALSE]
  )
  expect_identical(later$n, 3)
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
})

test_that("PostgreSQL Spearman rolls back aggregate failures", {
  con <- association_pg_connection()
  fixture <- association_pg_fixture(con)
  on.exit(association_pg_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  original <- getFromNamespace("eda_db_fetch", "episcout")
  testthat::local_mocked_bindings(
    eda_db_fetch = function(con,
                            statement,
                            params = list(),
                            query_kind,
                            limit,
                            timing_env = NULL,
                            variable_index = NA_integer_,
                            name = NA_character_) {
      if (identical(query_kind, "association_spearman")) {
        stop("forced aggregate failure", call. = FALSE)
      }
      original(
        con, statement, params, query_kind, limit, timing_env,
        variable_index, name
      )
    },
    .package = "episcout"
  )

  expect_error(
    epi_eda_profile_spearman(
      source, fixture$spec, association_pg_pairs()[1, , drop = FALSE]
    ),
    "forced aggregate failure"
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_true(DBI::dbIsValid(con))
})

test_that("PostgreSQL Spearman returns the same typed empty schema", {
  con <- association_pg_connection()
  fixture <- association_pg_fixture(con)
  on.exit(association_pg_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  pairs <- data.frame(left = character(), right = character())

  database <- epi_eda_profile_spearman(source, fixture$spec, pairs)
  frame <- epi_eda_profile_spearman(fixture$frame, fixture$spec, pairs)

  expect_identical(database, frame)
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
})
