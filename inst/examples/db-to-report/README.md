# Database-to-report walkthrough

This installed example is a step-by-step R script for a disposable PostgreSQL 17 database. It starts with a deliberately duplicated synthetic longitudinal CSV, creates related PostgreSQL source tables, reviews database metadata, pseudonymises both tables through one stable identity registry, runs aggregate-only PostgreSQL EDA, and finishes with plots, a long-form Table 1 and an HTML report.

The files are:

- `walkthrough.R`: the interactive, commented workflow;
- `synthetic-longitudinal.csv`: neutral synthetic input with one intentional exact duplicate visit row.

The data are synthetic and exist only to teach and test the workflow. They are not suitable for inference, privacy protection or disclosure-control validation.

## Prerequisites

Use a disposable PostgreSQL 17 or later database in which your approved learning role may create schemas and tables. The script never creates a PostgreSQL server, login role or credential. A database administrator should provide those infrastructure prerequisites; for a local disposable installation this may be as simple as creating an empty database named `episcout_walkthrough`.

Set standard libpq environment variables before opening R. Do not place a password in the script or commit it to source control.

```bash
export PGHOST=127.0.0.1
export PGPORT=5432
export PGDATABASE=episcout_walkthrough
export PGUSER=your_learning_role
# Use an approved password store or a temporary PGPASSWORD only when required.
```

Install the suggested packages used by the walkthrough: `RPostgres`, `data.table`, `compare`, `ggplot2`, `rmarkdown` and their dependencies. The HTML step also requires Pandoc 1.12.3 or later; `rmarkdown::pandoc_available()` should return `TRUE` before the database work begins.

## Run it

Locate the installed example, open it in an editor, and run one numbered section at a time in an interactive R session:

```r
example_dir <- system.file("examples", "db-to-report", package = "episcout")
file.edit(file.path(example_dir, "walkthrough.R"))
```

For a repository checkout, open `inst/examples/db-to-report/walkthrough.R` directly. The script creates three uniquely named schemas, so it does not overwrite a previous run. It prints the schema names and output directory near the end.

Set `EPISCOUT_WALKTHROUGH_CLEANUP=1` before running only when the disposable schemas should be removed automatically after the outputs have been inspected. Cleanup drops the three uniquely named walkthrough schemas with `CASCADE`; it never targets a fixed production name.

## Privacy boundary

The final row-level extraction and HTML report are appropriate here only because every row is explicitly synthetic. For real restricted data, use the aggregate-only `epi_eda_db_run()` bundle by default and obtain separate approval before extracting pseudonymised rows. Pseudonymised data remain restricted personal data; they are not anonymous or automatically safe to share.
