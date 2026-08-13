# Software Design

Spec ID: `043`
Status: Review

## Scope

Extend `epi_eda_profile_stratified()` by dispatching internally when `data` inherits from `epi_eda_postgres_source`. The data-frame implementation and function arguments remain unchanged. PostgreSQL work executes inside the existing read-only repeatable-read transaction after source catalogue and idle-connection validation.

## Public API

`epi_eda_profile_stratified(data, spec, strata, include_overall = TRUE, include_missing_stratum = TRUE)` accepts either a data frame or an unmodified PostgreSQL source. It returns the same `epi_eda_stratified` component names and column schemas for both inputs. Metadata adds an explicit source contract and normality limitation without changing presentation consumers.

`epi_eda_db_run()` gains opt-in stratification arguments. Defaults produce exactly the existing bundle. When requested, the owned manifest includes stratified aggregate components and Table 1.

## Inputs And Outputs

The stratifier must be exactly one specified categorical or binary field with compatible deterministic PostgreSQL storage and a valid missing-sentinel contract. Group values are reviewed categorical aggregates. Analysis outputs contain counts, typed summary aggregates, categorical levels, text diagnostics and temporal extrema/quantiles; they contain no identifiers, source text examples or source rows.

## Data Flow

1. Validate flags, specification, stratifier, unmodified source, relation kind, catalogue fingerprint and idle connection.
2. Begin the existing read-only repeatable-read transaction and revalidate the source.
3. Query only stratifier level/count aggregates, convert them into the released group ordering contract and derive driver-quoted internal group predicates.
4. Run canonical PostgreSQL aggregate summaries against each filtered relation expression with value-vector collection disabled.
5. Normalise categorical level universes across groups, bind released stratified components and reconcile group/population/Overall invariants.
6. Return aggregates for direct Table 1/categorical display consumption; optionally publish them through the existing staged database bundle and manifest.

## Edge Cases

- Zero rows retain Overall and declared zero-count groups when requested.
- All-missing strata create the explicit Missing group only when included and record omitted rows otherwise.
- Declared groups and analysis levels remain present at zero count; unexpected levels sort deterministically after declared levels.
- Non-finite numerics retain canonical finite denominators and aggregates. Shapiro-Wilk remains `NA` because value-vector queries are forbidden in this path.
- UTC datetimes retain canonical formatting; local timestamp storage remains audited as skipped.
- Missing, incompatible and unsupported analysis variables remain in `variables` and `skipped` rather than disappearing.

## Errors And Warnings

Invalid or modified sources, unsafe transaction state, unsupported stratifiers, invalid sentinel contracts and source drift fail before analytical queries. Database errors remain redacted through the established query wrapper.

## Dependencies

No new dependency is required. DBI and optional RPostgres remain the established database boundary.
