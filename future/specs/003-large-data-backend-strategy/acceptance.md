# Acceptance

Spec ID: `003-large-data-backend-strategy`
Status: Active

## Planning And Activation

- [x] `future/TODOs.md` and active specs were reconciled before revision; spec 003 was the repository's ready-next Priority 1 task.
- [x] The PostgreSQL design input was incorporated without importing project-specific data, schemas, dictionaries, credentials, terminology or output conventions.
- [x] Brief, SDD, TDD, acceptance, manifest and planning review define a coherent PostgreSQL-first design before package-code changes.
- [x] PostgreSQL 17+ and RPostgres are the only database backend/driver in scope; Arrow, DuckDB, data.table, SQLite, dbplyr and generic DBI dispatch remain excluded.
- [x] Existing profiler arguments, data-frame dispatch, six canonical components and output ordering remain the compatibility baseline.
- [x] Source, type, missingness, statistical, identifier-QA, plot-data, transaction, privacy, bundle, performance and failure contracts are explicit and independently testable.
- [x] Every success measure maps to planned validation and evidence; no blocking design question remains.
- [x] Applicable repository checklists and owner-review stop conditions are recorded.
- [x] Baseline package lint, tests, local check and CRAN-style check are recorded before package-code changes, with inherited/environment failures preserved rather than waived.

## Implementation Contract

- [x] `epi_eda_postgres_source()` accepts exactly one safely identified supported PostgreSQL relation through a live caller-owned connection and exposes no connection details.
- [x] The four existing profilers dispatch on data frame or explicit PostgreSQL source without accepting arbitrary DBI/lazy objects or changing data-frame result schemas.
- [x] Every supported technical type and incompatibility follows the reviewed mapping; no implicit source preparation or timestamp-without-time-zone interpretation occurs.
- [x] Standard/sentinel missingness, NaN, infinities, denominators and all-missing/zero-row states preserve canonical semantics.
- [x] Numeric/integer calculations reproduce type-7, e1071 type-3, Shapiro and outlier contracts with exact discrete results and predeclared floating tolerances.
- [x] Categorical/binary results contain complete declared/unexpected frequencies and both denominators; display-only leading-level collapse never changes canonical output.
- [x] Text and temporal results remain aggregate-only and reproduce R character/UTC/epoch semantics without raw text or session-timezone inference.
- [x] Explicit identifier roles are policy-skipped from ordinary summaries/plots and receive only the fixed aggregate QA fields.
- [x] Direct calls and the orchestrator own stable repeatable-read, read-only snapshots, reject caller transactions, clean up results and leave connections usable.
- [x] Client execution is sequential and every non-categorical fetch is bounded by the fixed query-kind contract; no full-row collection occurs.
- [x] Shared compact plot data reconcile to summaries and shared renderers produce deterministic, inspected SVGs without raw identifier/text values.
- [x] The database run returns the fixed object and publishes the exact aggregate-only owned bundle through tested staging/manifest/restore rules.
- [x] No database mutation, temporary relation, server setting, schema/index/grant management, pseudonymisation, suppression, approximation, sampling or alternate statistics are introduced.

## Evidence And Verification

- [x] Neutral fixtures cover every supported type and material edge case; expected values have independent provenance and anti-circularity guards.
- [x] Unit and mandatory disposable PostgreSQL integration tests cover parity, quoting/binding, snapshot consistency, read-only behaviour, lifecycle cleanup, privacy and filesystem recovery.
- [x] Existing data-frame, intake, dictionary, plotting and PostgreSQL security suites pass without weakened expectations.
- [x] Returned/file/log/condition/plot canaries and a repository-content audit establish the stated client-artifact privacy boundary and its caller-authored specification exception.
- [x] Plot compact data are reconciled before the exact delivered SVGs are rendered and visually inspected.
- [ ] The dedicated PostgreSQL CI job records three protocol-compliant fixed synthetic runs after one warm-up; median end-to-end time is less than 120 seconds and data-locality limits hold.
- [x] The data-frame limitation is established structurally without exporting restricted rows or making a comparative speed claim: full materialisation scales with source rows, while the PostgreSQL client boundary contains only reviewed aggregates and bounded test vectors subject to the categorical-frequency exception.
- [x] README, NEWS, roxygen and the specification-first vignette agree with observed behaviour and retain disclosure, server-logging and unsupported-type limitations.
- [x] Package-loaded lint, focused/full tests, live PostgreSQL tests, `scripts/check-local.sh`, `scripts/check-cran.sh` and `git diff --check` pass or every inherited/external limitation is recorded.
- [x] Software-verification, truth/semantics, analysis/statistics, figures and copy-edit checklists are completed with evidence in `review.md`.
- [x] Independent statistical-parity, PostgreSQL/read-only/privacy and bundle/plot/documentation reviews find no unresolved blocker.
- [x] `future/TODOs.md`, `future/README.md`, `future/changelog.md`, manifest, acceptance and review are reconciled at checkpoints and closeout.

## Publication Boundary

- [ ] The focused implementation is committed and pushed on the reviewed feature branch and its PR records behaviour, tests, compatibility, synthetic benchmark evidence, privacy limitations and unresolved issues.
- [ ] Owner acceptance is recorded before the completed spec moves to `future/specs/done/`.
- [ ] No release, tag, restricted workload publication, credential operation or unrelated repository change is performed under this spec.
