# Test Design

Spec ID: `028-epi-geo-postgis`
Status: Active

## Unit And Structural Tests

| Area | Scenarios | Independent evidence |
| --- | --- | --- |
| Public contract | exact formals, fixed source/description components, redacted print/str | hand-authored interface contract from issue #233/spec 026 |
| Identifier and bbox validation | odd quoted names; dotted/SQL identifiers; duplicate/geometry columns; malformed, non-finite, reversed or wrong-SRID bbox; integer bounds | exact accepted/rejected input matrix |
| Catalogue | absent extension; zero/one/multiple geometry columns; explicit selection; relation kinds; geometry/geography; fingerprint drift | mocked bounded query inventory and fixed value-free errors |
| Aggregate QA | zero/null/empty/valid/invalid; mixed type/SRID/dimension; aggregate bounds | hand-counted neutral fixture results |
| Bounded collection | below/at/above bound, empty bbox, boundary intersection, exact columns, query failure | hand-counted selected features and fail-not-truncate assertions |
| Data locality | intercepted statements, row limits and returned shapes | description statements cannot return geometry/WKT/WKB or ordinary columns; feature query exists only in collect |
| Lifecycle/privacy | existing transaction, begin/setup/query/commit failures, disconnect, messages/notices, connection reuse | canary strings absent from conditions and structures; reuse query succeeds |

## Disposable PostGIS Fixture

Create one process-unique schema in a PostGIS-enabled test database and drop exactly that schema on exit. The fixture contains:

- a typed `geometry(Point, 4326)` table with independently stated rows at bbox edges, one null and one empty geometry;
- a typed polygon table including one invalid bow-tie polygon;
- approved views exercising permissive declared type, mixed observed types, mixed SRIDs and Z dimensions;
- a `geography(Point, 4326)` table;
- a materialized view and an ordinary view over approved geometry;
- two spatial columns for the explicit ambiguity gate.

Expected counts, types, validity and bounds are written directly from the fixture literals, not generated with episcout. Tests create relations only as fixture setup; package functions never perform DDL.

## Mandatory Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'epi-geo-postgis', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 PGHOST=127.0.0.1 PGPORT=55432 PGDATABASE=synthetic_records PGUSER=postgres scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'epi-geo-postgis', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
```

The GitHub PostgreSQL integration and coverage jobs run the live geo tests against PostGIS 3.5/PostgreSQL 17. Local live evidence may use the repository mamba environment with a disposable user-owned server.
