library(episcout)
library(testthat)

test_that("pseudonymisation preflight reports structured missing capabilities", {
  linkage <- list(
    tables = data.frame(
      source_schema = "source", source_table = "people",
      destination_table = "people_safe", stringsAsFactors = FALSE
    ),
    crosswalks = data.frame(
      crosswalk_schema = character(), crosswalk_table = character(),
      stringsAsFactors = FALSE
    )
  )
  issues <- with_mocked_bindings(
    with_mocked_bindings(
      sec_privilege_issues(NULL, linkage, "registry", "output", "replace"),
      dbGetQuery = function(con, statement, params = NULL) {
        if (grepl("has_database_privilege", statement, fixed = TRUE)) {
          return(data.frame(allowed = FALSE))
        }
        data.frame(allowed = FALSE)
      },
      .package = "DBI"
    ),
    sec_destination_state = function(con, schema, table) {
      list(exists = TRUE, replaceable = FALSE, owned = FALSE)
    },
    .package = "episcout"
  )
  expect_true(all(c(
    "source_schema_usage_missing", "source_select_missing",
    "registry_schema_usage_missing", "registry_select_missing",
    "registry_insert_missing", "database_temp_missing",
    "output_schema_usage_missing", "output_schema_create_missing",
    "destination_ownership_missing"
  ) %in% issues$issue_code))
  expect_true(all(issues$severity == "error"))
  expect_false(any(grepl("GRANT|REVOKE", issues$message, ignore.case = TRUE)))
})

test_that("destination ownership recognises inherited owning roles", {
  statements <- character()
  state <- with_mocked_bindings(
    sec_destination_state(NULL, "output", "people_safe"),
    dbGetQuery = function(con, statement, params = NULL) {
      statements <<- c(statements, statement)
      data.frame(
        relkind = "r", relispartition = FALSE,
        owned = TRUE, has_dependencies = FALSE
      )
    },
    .package = "DBI"
  )
  expect_true(state$replaceable)
  expect_true(state$owned)
  expect_true(any(grepl("pg_has_role\\(current_user, c.relowner, 'MEMBER'\\)", statements)))
  expect_false(any(grepl("pg_get_userbyid", statements, fixed = TRUE)))
})
