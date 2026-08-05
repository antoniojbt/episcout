# Project Map

This map identifies the current `episcout` implementation, its user-facing entry points and the records that govern future work. Historical material under `archive/` and `legacy/` is not part of the current package contract.

## User Workflows

| Workflow | Main public entry points | Implementation | Tests | User documentation |
| --- | --- | --- | --- | --- |
| Review-gated data intake and EDA | `epi_eda_intake_run()`, `epi_eda_spec_scaffold()`, `epi_eda_prepare()` | `R/eda_intake.R`, `R/eda_spec_scaffold.R`, `R/eda_prepare.R` | `tests/testthat/test-eda-intake.R`, `tests/testthat/test-eda-spec-scaffold.R`, `tests/testthat/test-eda-prepare.R` | `README.md`, `vignettes/specification-first-eda.Rmd` |
| Canonical descriptive summaries and Table 1 | `epi_eda_profile_summaries()`, `epi_eda_profile_stratified()`, `epi_eda_table1()`, `epi_stats_summary()` | `R/eda_summaries.R`, `R/eda_stratified.R`, `R/eda_table1.R`, `R/summary_cores.R` | Corresponding `tests/testthat/test-eda-*` and `test-stats-*` files | `README.md`, `vignettes/specification-first-eda.Rmd` |
| Specification-first plots and reports | `epi_eda_profile_plots()`, `epi_eda_render_report()`, `epi_eda_run()` | `R/eda_plots.R`, `R/eda_report.R`, `R/run_eda.R`, `inst/report-template/` | Plot, report and workflow tests under `tests/testthat/` | `vignettes/specification-first-eda.Rmd` |
| PostgreSQL-backed specification-first EDA | `epi_eda_postgres_source()`, the four direct EDA profilers, `epi_eda_db_run()` | `R/eda_postgres_source.R`, `R/eda_postgres_queries.R`, `R/eda_db_run.R`, shared EDA modules | `tests/testthat/test-eda-postgres-*.R` plus data-frame EDA regressions | `README.md`, `vignettes/specification-first-eda.Rmd`, function help |
| PostgreSQL inventory and dictionaries | `epi_db_inventory()`, `epi_eda_dictionary_scaffold()`, `epi_eda_dictionary_validate()` | `R/db_inventory.R`, `R/eda_dictionary.R` | Database inventory and dictionary tests under `tests/testthat/` | Function help and both workflow vignettes |
| Longitudinal PostgreSQL pseudonymisation | `epi_sec_linkage_scaffold()`, `epi_sec_linkage_spec()`, `epi_sec_identity_registry_init()`, `epi_sec_pseudonymise_db()` | `R/epi_sec_linkage.R`, `R/epi_sec_registry.R`, `R/epi_sec_pseudonymise_db.R` | `tests/testthat/test-sec-linkage.R`, `tests/testthat/test-sec-pseudonymise-postgres.R` | `vignettes/longitudinal-pseudonymisation.Rmd` |
| Starter EDA project | `epi_eda_create_project()` | `R/use_episcout_project.R`, `inst/project-template/` | `tests/testthat/test-project-template.R` | `inst/project-template/README.md` |
| Lower-level cleaning, statistics, plotting and utilities | `epi_clean_*`, `epi_stats_*`, `epi_plot_*`, `epi_utils_*` | Prefix-matched files under `R/` | Prefix-matched tests under `tests/testthat/` | `README.md`, generated help under `man/` |

Pseudonymisation and descriptive EDA are separate controlled stages. Pseudonymised data remain restricted personal data and are not anonymous or automatically disclosure-controlled.

## Repository Layout

| Path | Role |
| --- | --- |
| `R/` | Package source and roxygen documentation; this is the source of truth for generated help. |
| `tests/testthat/` | Unit, integration, fixture and regression tests. Live PostgreSQL tests are gated by `EPISCOUT_TEST_POSTGRES=1`. |
| `vignettes/` | Canonical worked guides for specification-first EDA and longitudinal pseudonymisation. |
| `inst/` | Installed project and report templates. |
| `man/` and `NAMESPACE` | Roxygen-generated package interfaces; do not edit them directly. |
| `data-raw/` | Development scripts and provenance for package or test data. |
| `scripts/` | Canonical local and CRAN-oriented verification entry points. |
| `.github/workflows/` | R package, PostgreSQL integration and coverage automation. |
| `checklists/` | Required self-review criteria routed by `AGENTS.MD`. |
| `future/` | Draft/active specifications, TODOs, decisions, reviews and completed design records; it does not define package behaviour by itself. |
| `archive/` and `legacy/` | Historical records only. |

## Planning Status

- Spec `003-large-data-backend-strategy` remains active while its implemented PostgreSQL EDA contract awaits the external representative benchmark, independent reviews and owner acceptance.
- Completed spec records `001`, `002`, `004`–`010` and `012`–`016` are under `future/specs/done/`; spec `007` was a design-only review and the others record implemented package work.
- `future/TODOs.md` is the only task backlog. `future/README.md` defines the specification lifecycle, and `future/changelog.md` records material planning and implementation outcomes.

## Verification Entry Points

Use the repository R wrapper and existing scripts:

```bash
scripts/rscript_env_caller.R -e "R.home(); .libPaths()"
scripts/check-local.sh
scripts/check-cran.sh
```

Run focused tests first when practical. PostgreSQL integration uses the same documented wrapper with `EPISCOUT_TEST_POSTGRES=1` and an approved disposable database.
