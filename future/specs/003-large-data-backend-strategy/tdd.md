# Test Design

Spec ID: `003-large-data-backend-strategy`
Status: Draft; revision required before activation

## Design-only First Step

Complete and review the executable test contract before package-facing behaviour changes. The current checklist is a draft derived from `future/scratch/episcout_postgresql_backend_plan.md`, not authority to implement.

## Future Test Categories

- [ ] Independently calculated fixtures cover every supported type, ordinary and sentinel missingness, empty/all-missing/constant inputs, infinities, outliers, categorical level states, temporal values, non-syntactic names and identifier duplication.
- [ ] Data-frame and PostgreSQL paths match every canonical component under the same reviewed specification, with type-7 quantiles and other bounded collection explicitly tested.
- [ ] Identifier roles produce aggregate QA only and never return values or plots.
- [ ] SQL identifiers are quoted, values are bound where applicable, invalid sources fail safely and connections are left usable.
- [ ] Compact plot data preserve bins, counts, labels and exclusions; shared renderers produce deterministic SVG outputs without row-level values.
- [ ] Owned artifact writes, collision handling, manifests, checksums and failure cleanup preserve the existing filesystem policy.
- [ ] Returned objects, logs, SQL diagnostics and written artifacts contain no row-level values, identifiers, credentials or connection details.
- [ ] The full existing data-frame suite remains compatible.
- [ ] The external representative workload is benchmarked against the reviewed threshold without adding its data, schema, dictionary or terminology to this repository.

## Future Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```
