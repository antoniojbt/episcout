# Project Map

This map identifies the current `episcout` implementation, its user-facing entry points and the records that govern future work. Historical material under `archive/` and `legacy/` is not part of the current package contract.

## User Workflows

| Workflow | Main public entry points | Implementation | Tests | User documentation |
| --- | --- | --- | --- | --- |
| Review-gated data intake and EDA | `epi_eda_intake_run()`, `epi_eda_spec_scaffold()`, `epi_eda_prepare()`, `epi_eda_profile_geo()` | `R/eda_intake.R`, `R/eda_spec_scaffold.R`, `R/eda_prepare.R`, `R/eda_geo.R` | `tests/testthat/test-eda-intake.R`, `tests/testthat/test-eda-spec-scaffold.R`, `tests/testthat/test-eda-prepare.R`, `tests/testthat/test-eda-geo*.R` | `README.md`, `vignettes/specification-first-eda.Rmd` |
| Canonical descriptive summaries and Table 1 | `epi_eda_profile_summaries()`, `epi_eda_profile_stratified()`, `epi_eda_table1()`, `epi_stats_summary()` | `R/eda_summaries.R`, `R/eda_stratified.R`, `R/eda_table1.R`, `R/summary_cores.R` | Corresponding `tests/testthat/test-eda-*` and `test-stats-*` files | `README.md`, `vignettes/specification-first-eda.Rmd` |
| Specification-first plots and reports | `epi_eda_profile_plots()`, `epi_eda_render_report()`, `epi_eda_run()` | `R/eda_plots.R`, `R/eda_report.R`, `R/run_eda.R`, `inst/report-template/` | Plot, report and workflow tests under `tests/testthat/` | `vignettes/specification-first-eda.Rmd` |
| PostgreSQL-backed specification-first EDA | `epi_eda_postgres_source()`, the four direct EDA profilers, `epi_eda_db_run()`, `epi_eda_render_db_report()` | `R/eda_postgres_source.R`, `R/eda_postgres_queries.R`, `R/eda_db_run.R`, `R/eda_db_report.R`, shared EDA modules | `tests/testthat/test-eda-postgres-*.R`, `test-eda-db-report.R` plus data-frame EDA regressions | `README.md`, `vignettes/specification-first-eda.Rmd`, function help |
| PostgreSQL inventory and dictionaries | `epi_db_inventory()`, `epi_eda_dictionary_scaffold()`, `epi_eda_dictionary_validate()` | `R/db_inventory.R`, `R/eda_dictionary.R` | Database inventory and dictionary tests under `tests/testthat/` | Function help and both workflow vignettes |
| Longitudinal PostgreSQL pseudonymisation | `epi_sec_linkage_scaffold()`, `epi_sec_linkage_spec()`, `epi_sec_identity_registry_init()`, `epi_sec_pseudonymise_db()` | `R/epi_sec_linkage.R`, `R/epi_sec_registry.R`, `R/epi_sec_pseudonymise_db.R` | `tests/testthat/test-sec-linkage.R`, `tests/testthat/test-sec-pseudonymise-postgres.R` | `vignettes/longitudinal-pseudonymisation.Rmd` |
| Multi-table PostgreSQL identifier universe | `epi_sec_identity_universe_spec()`, `epi_sec_identity_universe_db()` | `R/epi_sec_identity_universe.R` | `tests/testthat/test-sec-identity-universe.R`, `tests/testthat/test-sec-identity-universe-postgres.R` | `vignettes/longitudinal-pseudonymisation.Rmd` and function help |
| Synthetic database-to-report walkthrough | Inventory, dictionary, duplicate, pseudonymisation, PostgreSQL EDA, Table 1 and report entry points | Installed script and fixtures under `inst/examples/db-to-report/` | Component PostgreSQL and report tests listed above; complete script verified manually | `inst/examples/db-to-report/README.md`, `inst/examples/db-to-report/walkthrough.R` |
| Restricted CURP structural audit | `epi_clean_curp_audit()`; compatibility extraction through `epi_clean_curp()` | `R/epi_clean_curp.R`, pinned code facts under `inst/extdata/` | `tests/testthat/test-curp-validation.R`, `tests/testthat/test-curp_misc.R` | Function help and `README.md` |
| Reviewed vector and bounded PostGIS mapping | `epi_geo_read()`, `epi_geo_from_coords()`, `epi_geo_describe()`, `epi_geo_transform()`, `epi_geo_map()`, `epi_geo_write()`, `epi_geo_postgis_source()`, `epi_geo_postgis_describe()`, `epi_geo_postgis_collect()` | `R/epi_geo.R`, `R/epi_geo_postgis.R`; optional `sf`/RPostgres and existing `ggplot2` path | `tests/testthat/test-epi-geo.R`, `tests/testthat/test-epi-geo-postgis.R` | `README.md`, `vignettes/geospatial-mapping-primer.Rmd` and function help |
| Starter EDA project | `epi_eda_create_project()` | `R/use_episcout_project.R`, `inst/project-template/` | `tests/testthat/test-project-template.R` | `inst/project-template/README.md` |
| Lower-level cleaning, statistics, plotting and utilities | `epi_clean_*`, `epi_stats_*`, `epi_plot_*`, `epi_utils_*` | Prefix-matched files under `R/` | Prefix-matched tests under `tests/testthat/` | `README.md`, generated help under `man/` |

Pseudonymisation and descriptive EDA are separate controlled stages. Pseudonymised data remain restricted personal data and are not anonymous or automatically disclosure-controlled.

## Repository Layout

| Path | Role |
| --- | --- |
| `R/` | Package source and roxygen documentation; this is the source of truth for generated help. |
| `tests/testthat/` | Unit, integration, fixture and regression tests. Live PostgreSQL tests are gated by `EPISCOUT_TEST_POSTGRES=1`. |
| `vignettes/` | Canonical worked guides for specification-first EDA and longitudinal pseudonymisation. |
| `inst/` | Installed project and report templates plus runnable worked examples. |
| `man/` and `NAMESPACE` | Roxygen-generated package interfaces; do not edit them directly. |
| `data-raw/` | Development scripts and provenance for package or test data. |
| `scripts/` | Canonical local and CRAN-oriented verification entry points. |
| `.github/workflows/` | R package, PostgreSQL integration and coverage automation. |
| `checklists/` | Required self-review criteria routed by `AGENTS.md`. |
| `future/` | Draft/active specifications, TODOs, decisions, reviews and completed design records; it does not define package behaviour by itself. |
| `archive/` and `legacy/` | Historical records only. |

## Planning Status

- GitHub release `0.3.0` is published from commit `40ef702`; CRAN work remains deferred under issue #81.
- GitHub roadmap issue [#249](https://github.com/antoniojbt/episcout/issues/249) is the authoritative live sequence. `future/TODOs.md` is its synchronised repository index, `future/README.md` defines the lifecycle and `future/changelog.md` records completed outcomes.
- Completed spec `025-curp-validation-and-reconciliation` records issue #225 and PR #231, merged to canonical `master` as `7e42f22`. Checksum verification remains `not_verified`; owner-resolved issue #230 closed without adopting an unofficial algorithm.
- Completed design spec `026-epi-geo-series-plan` and implementation specs `027-epi-geo-phase-a`, `028-epi-geo-postgis` and `029-eda-reviewed-coordinate-roles` are preserved under `future/specs/done/`; issue #226 closed through PR #234, merged as `b37b391`, issue #233 through PR #238, merged as `460acd0`, and terminal Phase-C issue #237 through planning PR #240 and implementation PR #241, merged as `308d544`. Later spatial inference requires a new concrete scientific tracker.
- Completed spec `030-simplify-core-eda-controls-geo-outputs` records issue #243, planning PR #244 and implementation PR #246, merged as `825215e`. Completed implementation spec `031-canonical-eda-delivery` records issue #245, planning PR #251 and implementation PR #252, merged as `74aeb0a`; opt-in PostgreSQL delivery publishes aggregate-only HTML/README output without database or row-level access. Completed design spec `032-eda-denominator-gap-assessment` and implementation spec `033-categorical-denominator-presentation` record issues #248/#253 through PRs #254/#257/#258, ending at `074f13a` with no successor.
- Owner roadmaps `issue-274`/`issue-275` retain all seven `epi_sec_*` exports while removing package governance authority. Completed design `issue-276`/`spec-034-retained-epi-sec-technical-contract` records the accepted technical contract through `PR-277`, merged as `commit-8641abe` and closed out canonically at `commit-cc05cb0`; it changed no package behaviour. Completed implementations `issue-278`/`spec-035-identity-universe-technical-contract`, `issue-284`/`spec-036-epi-sec-registry-neutral` and `issue-285`/`spec-037-epi-sec-linkage-results-neutral` record `PR-281`, final `PR-289` and `PR-291`, merged as `commit-ebd8d35`, `commit-40b284f` and `commit-0d9b302`. Documentation `issue-269` is the terminal reconciliation and remains gated by `issue-268`.
- Completed spec `022-postgresql-eda-row-count-reuse` is under `future/specs/done/` after issue #220 and PR #222 merged. Historical completed manifests remain authoritative records of their own accepted work.
- Security issue #213, CRAN issue #81 and clarification issues #61/#62/#65/#212 are deferred under roadmap #249. Issue #196 is superseded by #245 and #235 is closed as not planned.

## Verification Entry Points

Use the repository R wrapper and existing scripts:

```bash
scripts/rscript_env_caller.R -e "R.home(); .libPaths()"
scripts/check-workflow-state.sh
scripts/check-local.sh
scripts/check-cran.sh
```

Run focused tests first when practical. PostgreSQL integration uses the same documented wrapper with `EPISCOUT_TEST_POSTGRES=1` and an approved disposable database.
