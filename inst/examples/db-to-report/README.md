# Database-to-EDA-delivery walkthrough

This installed example is a step-by-step R script for a disposable PostgreSQL 17 or later database. It starts with a deliberately duplicated synthetic longitudinal CSV, creates related PostgreSQL source tables, builds a semantic dictionary, declares separate linkage column policy, pseudonymises both tables through one stable identity registry, and finishes with a manifest-owned PostgreSQL EDA delivery whose HTML report is rendered only from aggregate bundle artifacts.

The files are:

- `walkthrough.R`: the interactive, commented workflow;
- `synthetic-longitudinal.csv`: neutral synthetic input with one intentional exact duplicate visit row.

The data are synthetic and exist only to teach and test the workflow. They are not suitable for inference.

## Prerequisites

Use a disposable PostgreSQL 17 or later database in which your learning role may create schemas and tables. The script never creates a PostgreSQL server, login role or credential. A database administrator should provide those infrastructure prerequisites; for a local disposable installation this may be as simple as creating an empty database named `episcout_walkthrough`.

Set standard libpq environment variables before opening R. Do not place a password in the script or commit it to source control.

```bash
export PGHOST=127.0.0.1
export PGPORT=5432
export PGDATABASE=episcout_walkthrough
export PGUSER=your_learning_role
# Use an appropriate password store or a temporary PGPASSWORD only when required.
```

Install the suggested packages used by the walkthrough: `RPostgres`, `data.table`, `compare`, `ggplot2`, `rmarkdown`, `knitr` and their dependencies. Pandoc must also be available for the HTML report.

## Run it

Locate the installed example, open it in an editor, and run one numbered section at a time in an interactive R session:

```r
example_dir <- system.file("examples", "db-to-report", package = "episcout")
file.edit(file.path(example_dir, "walkthrough.R"))
```

For a repository checkout, open `inst/examples/db-to-report/walkthrough.R` directly. The script creates three uniquely named schemas, so it does not overwrite a previous run. It prints the schema names and output directory near the end.

Set `EPISCOUT_WALKTHROUGH_CLEANUP=1` before running only when the disposable schemas should be removed automatically after the outputs have been inspected. Cleanup drops the three uniquely named walkthrough schemas with `CASCADE`; it never targets a fixed production name.

## Output ownership

The walkthrough does not extract pseudonymised rows into R for reporting. Its semantic output dictionary passes directly into `epi_eda_dictionary_spec()`, and `epi_eda_db_run(layout = "delivery")` publishes the requested aggregate summaries, categorical numerator/denominator companions, plots, README and portable HTML after the database snapshot closes. The HTML is the human-facing entry point; the manifest-owned CSV, SVG and checksum artifacts remain the canonical evidence. Core manifests use `artifact`, `type`, `path`, `status` and `checksum_md5`. episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared. Specialised security manifests and restricted-data safeguards remain unchanged.
