# PostgreSQL EDA repeats relation-wide aggregate queries, including one `count(*)` per categorical variable

Status: Completed through issue [#220](https://github.com/antoniojbt/episcout/issues/220), spec `022-postgresql-eda-row-count-reuse` and merged PR #222; retained as the original investigation record.

## Issue summary

`epi_eda_db_run()` produces correct, reconciled aggregate bundles, but its PostgreSQL backend sends many separate statements that repeatedly query the same relation. One narrow case is clearly redundant: the run obtains the relation row count once and passes it into the summary workflow, yet every categorical variable calls `eda_postgres_row_count()` again.

This is a noncritical performance issue. All tested bundles completed successfully and their outputs reconciled. Processing continued; no correctness or privacy failure was observed.

## Tested environment

- `episcout`: 0.2.0
- Tested upstream SHA: `f6f09defd211f234268ee0bb9f5a8e19fe2d88a9`
- R: 4.5.3
- DBI: 1.3.0
- RPostgres: 1.4.10
- PostgreSQL: 18.4 (`server_version_num = 180004`)
- Source kind: PostgreSQL views
- Workflow: unstratified aggregate EDA with plots enabled
- Affected exported function: `epi_eda_db_run()`
- Relevant internal functions: `eda_postgres_summaries_inside()`, `eda_postgres_missing_inside()`, `eda_pg_categorical_summary()`, `eda_postgres_basic_counts()`, `eda_postgres_row_count()`, `eda_postgres_plot_data_inside()` and `eda_pg_identifier_qa_inside()`

The measurements below come from one private integration campaign. Relation and variable names have been replaced with neutral labels, and row counts and source fingerprints are intentionally omitted. Timings are observations from this environment, not portable benchmarks.

## Observed measurements

| Source | Columns | End-to-end seconds | Timed database operations | `row_count` operations | Redundant `row_count` operations |
| --- | ---: | ---: | ---: | ---: | ---: |
| `relation_a` | 29 | 295.818 | 127 | 13 | 12 |
| `relation_b` | 30 | 249.913 | 131 | 14 | 13 |
| `relation_c` | 28 | 81.104 | 123 | 14 | 13 |
| **Total** | **87** | **626.835** | **381** | **41** | **38** |

There are 41 `row_count` operations in total: one necessary initial count for each relation and 38 additional counts, exactly one for each categorical variable. The earlier shorthand “41 redundant counts” was incorrect; the reconciled number is **38 redundant counts**.

The full timing inventory also shows repeated relation-query statements at several stages:

| Query kind | `relation_a` | `relation_b` | `relation_c` | Total |
| --- | ---: | ---: | ---: | ---: |
| `missing_scalar` | 29 | 30 | 28 | 87 |
| `variable_counts` | 29 | 30 | 28 | 87 |
| `categorical_frequency` | 12 | 13 | 13 | 38 |
| `text_aggregate` | 12 | 12 | 10 | 34 |
| `numeric_first_pass` | 3 | 3 | 3 | 9 |
| `numeric_moments` | 3 | 3 | 3 | 9 |
| `numeric_fences` | 3 | 3 | 3 | 9 |
| `identifier_qa` | 1 | 1 | 1 | 3 |
| `plot_histogram` | 15 | 15 | 13 | 43 |
| `row_count` | 13 | 14 | 14 | 41 |
| Catalogue queries | 6 | 6 | 6 | 18 |
| Transaction setup | 1 | 1 | 1 | 3 |
| **Total** | **127** | **131** | **123** | **381** |

These counts do not prove that PostgreSQL performs an identical physical full-table scan for every statement; that depends on the view definition, plan, indexes and server state. They do show that the client sends 360 separate statements that reference the analytical relation, plus 18 catalogue operations and three transaction-setup operations.

## Confirmed narrow cause

The installed implementation of `epi_eda_db_run()` does the following inside one PostgreSQL transaction:

1. Calls `eda_postgres_row_count()` and stores the result in `n_total`.
2. Passes `n_total` to missingness, summary and identifier-QA workflows.
3. Calls `eda_postgres_summaries_inside(source, spec, timing_env, n_total)`.

Within the categorical branch, however, `eda_pg_categorical_summary()` does not accept `n_total`. After obtaining the frequency table, it calls:

```r
n_total <- eda_postgres_row_count(source, timing_env = timing_env)
```

Consequently, a specification containing `k` categorical or binary variables produces `1 + k` `row_count` timing entries. The extra count is used only to calculate `p_total`; the already available transaction-consistent `n_total` would provide the same denominator.

The wider query inventory also shows opportunities for later optimization:

- `eda_postgres_missing_inside()` calculates missingness once per variable.
- `eda_postgres_basic_counts()` then recalculates missing, observed and distinct counts once per variable.
- Type-specific summaries calculate some of the same counts again.
- Identifier QA recalculates identifier missing, observed and distinct counts while adding duplication metrics.
- Numeric and text plots issue separate histogram queries.

Some separation may be intentional for failure isolation and reconciliation. The narrow categorical recount does not appear to provide either benefit because the original `n_total` was obtained in the same transaction.

## Expected behavior

At minimum, one call to `epi_eda_db_run()` should count the relation once, regardless of the number of categorical variables. Categorical `p_total` values should reuse the `n_total` already calculated within the same read-only snapshot.

As a separate follow-up, compatible missingness, basic-count and typed-summary aggregates could be consolidated where this can be done without changing:

- aggregate values or missing-value semantics;
- the read-only repeatable snapshot;
- identifier protections and bounded outputs;
- reconciliation and manifest ownership;
- source and specification fingerprints;
- failure isolation and actionable diagnostics.

No unbounded row extraction or client-side collection should be introduced as a performance optimization.

## Observed behavior

- The three-view campaign issued 41 relation counts instead of three.
- All 41 count operations took 14.880 seconds in total. The timing contract does not distinguish each relation's initial count from its categorical recounts, so the redundant subset is not assigned a separate measured duration. This is not the largest cost, but the repeated call pattern is deterministic and avoidable.
- Per-variable missingness, basic-count, typed-summary, QA and plot statements account for most of the remaining runtime.
- All outputs were valid, so severity is **noncritical / performance**.
- The integration campaign continued and completed every source.

## Minimal neutral reproduction

Run this only against an approved disposable PostgreSQL database. It creates and drops a uniquely named schema, uses generated synthetic rows, and does not require project data. Standard libpq environment variables supply the connection; do not place credentials in the script.

```r
library(DBI)
library(RPostgres)
library(episcout)

run_reproduction <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres())
  fixture_schema <- paste0(
    "episcout_perf_",
    format(Sys.time(), "%y%m%d%H%M%S", tz = "UTC"),
    Sys.getpid()
  )
  stopifnot(grepl("^[a-z0-9_]+$", fixture_schema))

  quoted_schema <- as.character(DBI::dbQuoteIdentifier(con, fixture_schema))
  quoted_relation <- as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = fixture_schema, table = "observations")
  ))
  output_dir <- tempfile("episcout-perf-bundle-")

  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(
        con,
        paste("DROP SCHEMA", quoted_schema, "CASCADE")
      )
      DBI::dbDisconnect(con)
    }
    unlink(output_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  DBI::dbExecute(con, paste("CREATE SCHEMA", quoted_schema))
  DBI::dbExecute(
    con,
    paste0(
      "CREATE VIEW ", quoted_relation, " AS ",
      "SELECT ",
      "'P' || lpad((((g - 1) % 50000) + 1)::text, 5, '0') AS pid, ",
      "CASE WHEN g % 2 = 0 THEN 'A' ELSE 'B' END AS category_a, ",
      "CASE WHEN g % 3 = 0 THEN 'X' ELSE 'Y' END AS category_b, ",
      "repeat('x', (g % 40)::integer) AS note, ",
      "(g % 1000)::double precision AS measure ",
      "FROM generate_series(1, 1000000) AS series(g)"
    )
  )

  source <- episcout::epi_eda_postgres_source(
    con,
    fixture_schema,
    "observations"
  )
  spec <- data.frame(
    name = c("pid", "category_a", "category_b", "note", "measure"),
    label = c(
      "Participant identifier",
      "Category A",
      "Category B",
      "Synthetic note",
      "Synthetic measure"
    ),
    type = c("text", "categorical", "categorical", "text", "numeric"),
    role = c("identifier", "covariate", "covariate", "covariate", "outcome"),
    required = TRUE,
    stringsAsFactors = FALSE
  )

  result <- episcout::epi_eda_db_run(
    source,
    spec,
    output_dir = output_dir,
    overwrite = FALSE,
    plots = TRUE
  )
  timings <- result$timings
  counts <- table(timings$query_kind)
  print(counts)

  # Current behavior: one initial count plus one recount for each of the
  # two categorical variables.
  stopifnot(unname(counts[["row_count"]]) == 3L)
  invisible(counts)
}

run_reproduction()
```

This reproduction was executed successfully against an isolated PostgreSQL 18.4 container with the tested package installation. Its timing inventory contained five `missing_scalar`, five `variable_counts`, two `categorical_frequency` and **three `row_count`** entries. The generated schema and container were removed after the check.

With the narrow fix, the final assertion should change to:

```r
stopifnot(unname(counts[["row_count"]]) == 1L)
```

## Suggested changes

### 1. Bug-sized change

- Add `n_total` to the internal `eda_pg_categorical_summary()` interface.
- Pass the existing transaction-local value from `eda_postgres_summaries_inside()`.
- Remove its per-variable call to `eda_postgres_row_count()`.
- Preserve the current `p_total`, `p_observed`, missing and reconciliation results.

This should remove exactly one query per categorical or binary variable without changing the exported API.

### 2. Separate performance feature

Profile the query plan at the package level and consider consolidating compatible aggregates. Potential approaches include one multi-column missingness statement per relation, reuse of typed-summary counts in `summary_variables`, and reuse of identifier-QA counts for identifier variables. This should be designed and benchmarked upstream rather than emulated by callers.

## Acceptance checks for an upstream fix

1. The neutral reproduction records exactly one `row_count` operation.
2. Adding more categorical variables does not increase the `row_count` count.
3. Categorical counts, `p_total`, `p_observed`, missingness, identifier QA, typed summaries and plot inventories match the pre-fix results on a fixed synthetic fixture.
4. The run remains inside the existing read-only transaction and does not extract rows.
5. Bundle reconciliation, checksums, source/specification fingerprints and `overwrite = FALSE` behavior remain intact.
6. Timing output continues to identify each executed database operation accurately.

## Integration impact

The behavior increases runtime and database work during live integration testing, especially when relations contain many variables or expensive views. It did not prevent valid aggregate results, so downstream processing continued. No local compatibility layer or workaround was applied, and this report does not modify or patch `episcout`.
