# Software Design Review

Spec ID: `007-eda-stats-alignment-review`
Status: Completed

## Objective

Define a reviewed boundary between the active `epi_stats_*` statistics layer and the specification-first EDA workflow so a later builder can consolidate behaviour without guessing about public contracts, missing-value semantics, type dispatch or downstream outputs.

## Scope

The review covers all 25 exported functions whose names begin with `epi_stats_`, the related exported `rename_contingency_2x2_cols()` helper, internal call relationships among them, and direct consumers in plotting, EDA orchestration, reports, vignettes and tests.

All seven EDA specification types are in scope: `numeric`, `integer`, `categorical`, `binary`, `text`, `date` and `datetime`.

## Public API Position

`epi_stats_*` is the active primary lower-level statistics API. It is not deprecated, superseded or legacy. The specification-first EDA layer is an orchestrator that should use the same reviewed statistical primitives while adding specification metadata, sentinel-code handling, type dispatch and machine-readable workflow outputs.

Existing public contracts are evidence rather than permanent design constraints. A later implementation may introduce breaking changes only when spec 008 documents the replacement, migration path, staged deprecation where applicable, release boundary and updated tests.

## Current Inputs and Outputs

### Statistics layer

- Inputs range from numeric/date vectors to mixed data frames, correlation objects and named variables.
- Outputs vary among base data frames, tibbles, lists, numeric scalars and printed side effects.
- Missingness rules vary between standard `NA` removal, implicit table dropping and function-specific behaviour; specification sentinel codes are not understood by the statistics layer.
- Validation and optional dependency guards are inconsistent.

### EDA layer

- `epi_eda_profile_missing()` returns `name`, `n`, `n_missing` and `p_missing`, using standard `NA` plus per-variable `missing_codes`.
- `epi_eda_profile_summaries()` returns only `numeric` and `categorical` tables.
- Numeric EDA output contains `name`, `n`, `n_missing`, `mean`, `sd`, `median`, `min` and `max`.
- Categorical EDA output contains `name`, `level`, `n`, `p` and `p_observed`, preserving declared levels with zero counts.
- Text, date and datetime variables receive neither a summary nor a documented skip result.
- `epi_eda_run()` writes only `summary_numeric.csv` and `summary_categorical.csv`; the report renders those two tables.

## Recommended Target Architecture

Use shared unexported, type-specific computation cores with thin public adapters. Do not make EDA reshape the full output of broad public functions, and do not let public wrappers reimplement statistical calculations.

```text
validated data + validated specification
  -> common missing mask (NA + specification sentinel codes)
  -> common per-variable counts
  -> type dispatcher
       -> numeric/integer core
       -> categorical/binary core
       -> text core
       -> date/datetime core
  -> public epi_stats_* adapters
  -> EDA adapters with specification metadata
  -> epi_eda_run() outputs
  -> CSV/report consumers
```

### Target EDA summary contract

`epi_eda_profile_summaries()` should eventually return these machine-readable components:

| Component | Row unit | Required content |
| --- | --- | --- |
| `variables` | One row per specified variable | `name`, `label`, `type`, `role`, `n`, `n_missing`, `n_observed`, `n_unique`, `n_infinite`, and status/reason fields |
| `numeric` | One row per numeric/integer variable | Finite observed count, sum, min, quartiles, mean, median, max, IQR, SD, variance, SEM, CV, Tukey fences and outlier counts; normality/shape fields must be explicitly optional |
| `categorical` | One row per declared or observed level | Level count, total-row proportion and observed-row proportion, retaining declared zero-count levels |
| `text` | One row per text variable | Missing, empty and whitespace counts, unique count, minimum and maximum observed string length |
| `temporal` | One row per date/datetime variable | Class/time zone, missing and unique counts, minimum, quartiles, median, maximum and observed range in documented units |
| `skipped` | One row per variable not summarised | Variable name, specification type, observed class and explicit reason |

The implementation spec must decide the transition from the current two-component contract. The recommended migration is additive for one release, with old CSV names retained while new components are introduced, followed by any approved rename or removal at a documented release boundary.

### Shared semantic policies

- Standard `NA`, `NaN` and specification sentinel codes are missing; EDA applies the specification before calling common cores.
- Counts use total, missing, observed and unique terms consistently.
- Numeric moments and quantiles operate on finite observed values; `Inf` and `-Inf` are counted as `n_infinite` data-quality conditions rather than silently producing contradictory summaries.
- Declared categorical levels remain visible at zero count; undeclared observed values must be surfaced rather than silently dropped.
- Date, `IDate`, POSIXct and POSIXlt inputs require explicit supported-class, time-zone and output-unit policies.
- Computation functions return values without printing; presentation formatting is a separate layer.
- Empty and all-missing inputs return stable typed rows or documented empty tables, never `NaN`, `Inf`, `-Inf` or opaque base errors unless non-finite values are intentionally represented as data-quality fields.

## Review Stages and Contracts

### S-001 — Inventory public statistics interfaces

**Purpose:** Establish the complete review surface.
**Inputs:** Current R sources, NAMESPACE, documentation and test references.
**Output:** The function contract matrix in `review.md`, covering every exported statistics function and compatibility helper.
**Validation:** Namespace enumeration contains 25 `epi_stats_*` exports and the matrix contains each exactly once.
**Failure modes:** Missing aliases, undocumented direct consumers or assuming a filename defines the public API.

### S-002 — Map behaviour and evidence

**Purpose:** Distinguish documented contracts, tested behaviour and confirmed defects.
**Inputs:** Function sources, tests, fixtures, archived EDA requirements and read-only edge-case probes.
**Output:** Evidence-backed findings and a future behaviour-test matrix.
**Validation:** Every finding cites current source/test behaviour or a recorded probe; assumptions are labelled.
**Failure modes:** Mirroring implementation as expected behaviour, treating passing tests as proof of correctness, or changing tracked snapshots during baseline runs.

### S-003 — Map EDA types and downstream contracts

**Purpose:** Show how all specification types flow through summaries, orchestration, CSV output and reports.
**Inputs:** EDA specification, summary, run and report contracts plus blood-storage and penguins fixtures.
**Output:** The type map and downstream compatibility matrix in `review.md`.
**Validation:** All seven specification types appear; every current or proposed output consumer is listed.
**Failure modes:** Ignoring text/date/datetime omissions or expanding outputs without identifying report and fixture consequences.

### S-004 — Select the target architecture

**Purpose:** Recommend one coherent reuse and migration direction.
**Inputs:** Function matrix, EDA map, confirmed gaps and compatibility risks.
**Output:** Shared-core architecture, per-group dispositions and ordered implementation programme.
**Validation:** Every function group has one disposition and every breaking recommendation has a migration expectation.
**Failure modes:** One large refactor spanning unrelated APIs, direct EDA dependence on presentation-oriented outputs, or unannounced breaking changes.

### S-005 — Builder handoff gate

**Purpose:** Define what must be accepted before implementation planning.
**Inputs:** Completed review documents.
**Output:** Priority 1 human-review task and provisional scope for spec 008.
**Validation:** Spec 008 does not exist; no package files changed; blocking implementation decisions are clearly identified for human acceptance.
**Failure modes:** Treating review completion as approval to refactor or creating executable tests before target contracts are accepted.

## Dependencies and Reproducibility

This review adds no dependencies. Evidence commands use `scripts/rscript_env_caller.R` and the project mamba environment. The baseline records R 4.5.3 on aarch64 macOS, the current branch and tracked fixture versions. Read-only probes contain no randomness except existing function internals; the review flags unseeded Monte Carlo testing as a reproducibility concern.

## Protocols and Standards

- Repository `AGENTS.md`: R wrapper, SDD/TDD workflow, no speculative over-engineering, behaviour-focused tests and preservation of public behaviour unless reviewed.
- Archived EDA architecture: build on existing `epi_stats_*` helpers and use them where suitable.
- R package compatibility: exported interface changes require documentation, tests, migration handling and package checks.

## Implementation Boundary

Spec 007 is complete as a review artifact but is not `ready_for_build`. Human acceptance of the target contracts is required before creating spec 008. No R source, NAMESPACE, generated documentation, report template, fixture or executable test is changed here.
