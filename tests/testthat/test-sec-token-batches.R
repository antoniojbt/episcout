library(episcout)
library(testthat)

test_that("registry token allocation is bounded and set-based", {
  generated <- integer()
  appended <- list()
  queries <- character()
  result <- with_mocked_bindings(
    with_mocked_bindings(
      sec_fill_registry_tokens(NULL, "registry", 5, 2),
      dbGetQuery = function(con, statement, ...) {
        queries <<- c(queries, statement)
        if (grepl("token_prefix", statement, fixed = TRUE)) {
          return(data.frame(token_prefix = "E", n_bytes = 24L))
        }
        data.frame(collision = FALSE)
      },
      dbExecute = function(con, statement, ...) 0L,
      dbAppendTable = function(con, name, value, ...) {
        appended[[length(appended) + 1L]] <<- value
        nrow(value)
      },
      .package = "DBI"
    ),
    sec_generate_tokens = function(n, n_bytes, prefix) {
      generated <<- c(generated, n)
      paste0(prefix, seq_len(n), "-", length(generated))
    },
    sec_quote_table = function(con, schema, table) paste(schema, table, sep = "."),
    .package = "episcout"
  )
  expect_true(result)
  expect_identical(generated, c(2, 2, 1))
  expect_lte(max(vapply(appended, nrow, integer(1))), 2L)
  expect_equal(sum(grepl(" AS collision", queries, fixed = TRUE)), 3L)
  expect_false(any(grepl("entity_token = $1", queries, fixed = TRUE)))
})

test_that("registry token allocation stops after five batch collisions", {
  generated <- 0L
  expect_error(
    with_mocked_bindings(
      with_mocked_bindings(
        sec_fill_registry_tokens(NULL, "registry", 1, 1),
        dbGetQuery = function(con, statement, ...) {
          if (grepl("token_prefix", statement, fixed = TRUE)) {
            return(data.frame(token_prefix = "E", n_bytes = 24L))
          }
          data.frame(collision = TRUE)
        },
        dbExecute = function(con, statement, ...) 0L,
        dbAppendTable = function(con, name, value, ...) nrow(value),
        .package = "DBI"
      ),
      sec_generate_tokens = function(n, n_bytes, prefix) {
        generated <<- generated + 1L
        paste0(prefix, generated)
      },
      sec_quote_table = function(con, schema, table) paste(schema, table, sep = "."),
      .package = "episcout"
    ),
    "could not be generated"
  )
  expect_equal(generated, 5L)
})
