# Database-to-EDA-delivery walkthrough

This installed example is a numbered R script for an approved disposable PostgreSQL 17 or later database. It starts with a deliberately duplicated neutral synthetic longitudinal CSV, creates related source tables, builds and reviews a semantic dictionary, declares separate linkage column policy, pseudonymises both tables through one stable identity registry, hands the pseudonymised visit dictionary to PostgreSQL-backed EDA, and publishes a manifest-owned delivery whose HTML report is derived from the completed bundle.

Use `vignette("specification-first-eda", package = "episcout")` for the canonical data-frame and PostgreSQL EDA contracts. Use `vignette("longitudinal-pseudonymisation", package = "episcout")` for detailed linkage, recovery and restricted-data controls. This walkthrough connects those workflows without replacing either guide.

The installed files are:

- `walkthrough.R`: the interactive, commented workflow;
- `synthetic-longitudinal.csv`: neutral synthetic input with one intentional exact duplicate visit row.

The data exist only to teach and test the workflow. They are not suitable for inference, anonymity or disclosure control.

## Prerequisites and ownership

Use an approved disposable PostgreSQL 17 or later database in which the learning role may create and drop schemas and tables. The script never creates a PostgreSQL server, database, login role or credential. The caller or database administrator provides those infrastructure prerequisites; episcout does not manage them.

Set standard libpq environment variables before opening R. Do not place a password in the script or commit it to source control.

```bash
export PGHOST=127.0.0.1
export PGPORT=5432
export PGDATABASE=episcout_walkthrough
export PGUSER=your_learning_role
# Use an appropriate password store or a temporary PGPASSWORD only when required.
```

Install the suggested packages used by the walkthrough: `RPostgres`, `data.table`, `compare`, `ggplot2`, `rmarkdown`, `knitr` and their dependencies. Pandoc must also be available for the portable HTML report.

The walkthrough executes fixed, safely quoted schema-management statements and package-defined database operations. The EDA source itself accepts only separate schema and relation identifiers, not arbitrary SQL. PostgreSQL and driver logs, backups, credentials and access controls remain the caller's infrastructure responsibility.

## Run the ordered workflow

Locate the installed example, open it in an editor, and run one numbered section at a time in an interactive R session:

```r
example_dir <- system.file("examples", "db-to-report", package = "episcout")
file.edit(file.path(example_dir, "walkthrough.R"))
```

For a repository checkout, open `inst/examples/db-to-report/walkthrough.R` directly. The sections perform this ordered path:

1. Read and inspect the neutral synthetic CSV.
2. Confirm and remove only the intentional exact duplicate.
3. Connect to the approved disposable database and create three uniquely named restricted schemas.
4. Inventory source metadata, scaffold the extended dictionary, declare its semantics and profile only explicitly selected catalogue fields.
5. Declare linkage tables, columns and record keys, then initialise the identity registry.
6. Audit and explicitly apply pseudonymisation into the output schema.
7. Convert the pseudonymisation output dictionary into the 15-field EDA specification, profile the PostgreSQL relation and publish `layout = "delivery"`.
8. Reconcile the five-column manifest, disconnect and optionally remove the disposable schemas.

The script prints the unique schema names and output root near the end. Set `EPISCOUT_WALKTHROUGH_RUN_ID` only to a unique 6-to-20-character lower-case alphanumeric value when a reproducible name is needed. Set `EPISCOUT_WALKTHROUGH_OUTPUT` to an explicit local output parent when the default current-directory parent is unsuitable.

Set `EPISCOUT_WALKTHROUGH_CLEANUP=1` before running only when the three disposable schemas should be removed automatically after output inspection. Cleanup drops those uniquely named walkthrough schemas with `CASCADE`; it never targets a fixed production name. If execution stops early, disconnect explicitly and have the database owner review the printed unique names before cleanup.

## EDA handoff and delivery contract

Pseudonymisation is a separate restricted-data stage. Its semantic `output_dictionary` and `output_catalogues` pass to `epi_eda_dictionary_spec()`, while retained-column policy remains in the linkage specification. The walkthrough does not extract pseudonymised observations into R for reporting. PostgreSQL-backed EDA has no preparation mode; the reviewed pseudonymised output relation already provides its storage and semantic handoff.

`epi_eda_db_run(layout = "delivery", maps = FALSE)` reads one relation through a caller-owned open idle RPostgres connection. It performs its profiling in one read-only repeatable-read snapshot, closes the snapshot before rendering and atomically publishes the completed root. A successful call returns `status == "complete"`; validation or publication failures are errors rather than intake-style blocked statuses.

The delivery root owns these entry points and evidence:

- `README.md` and `reports/eda-report.html`: human-facing entry points;
- `QA_QC/`: aggregate schema, missingness, summary, identifier and inventory components;
- `plot_data/`: compact aggregate plot inputs, including categorical numerator, denominator, proportion and basis fields;
- `plots/`: deterministic SVG plots requested by this example;
- `maps/`: requested map SVGs when any are created; this walkthrough sets `maps = FALSE`, so the directory is absent;
- `run_manifests/`: source and run metadata, query timings, delivery metadata and `manifest.csv`.

The core manifest fields are exactly `artifact`, `type`, `path`, `status` and `checksum_md5`; paths are relative to the output root. Retain and transfer the complete owned root. The HTML is a view of the validated manifest-owned CSV and SVG evidence, not a replacement for it.

With maps disabled, this delivery contains no source rows, raw text examples, observed identifiers, pseudonymisation bridge tables, map coordinates or thematic row values. Its summaries, dictionary metadata, category labels, small cells and plots can still be sensitive, so aggregate-only does not mean anonymous or automatically disclosure-controlled. episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared.

Pseudonymisation replaces or separates direct identifiers under a controlled registry and linkage process. Pseudonymised observations remain restricted personal data; pseudonymisation is not anonymity, publication approval or disclosure control.
