# Software Design

Spec ID: `039-reviewable-qc-cleaning-proposals`

Status: Active

## Design Authority And Boundary

The reviewed EDA specification is authoritative for semantic type, role, units, declared levels, missing codes and descriptive `min`/`max` metadata. The source is authoritative only for observations in the profiled snapshot. Generated aggregates are descriptive evidence, and deterministic candidates are pending review prompts. Neither evidence nor a pending proposal is an approved rule.

The implementation adds a separate result and never adds columns to, edits or returns a modified semantic specification. The future approved-rule contract belongs to issue-272. No string or state change inside this result can authorise rule execution.

## Public API

```r
epi_eda_qc_proposals(
  data,
  spec,
  variable_keys
)
```

`data` is an ordinary data frame or an `epi_eda_postgres_source`. `spec` is a data frame or CSV path accepted by `epi_eda_spec()`. `variable_keys` is an ordinary data frame containing exactly `name` and `variable_key` in that order.

The function returns class `c("epi_eda_qc_proposals", "list")` with exactly `evidence` and `proposals`. It has no mode, approval, threshold, output-path or overwrite argument. The fixed Tukey coefficient is the existing canonical value 1.5. No randomness, timestamps or runtime metadata enter the result.

## Stable Variable Keys

`variable_keys$name` is an input-only exact lookup value and must cover every normalised specification name exactly once with no extra row. `variable_keys$variable_key` is a caller-created, persisted opaque identifier matching `^var_[a-z0-9]{16,64}$`. Keys must be unique, non-missing and non-empty. The package does not derive, hash, regenerate, trim or reinterpret them.

The same map must be reused when equivalent in-memory and PostgreSQL sources are compared. A key remains stable across observation changes, backend changes and semantic dictionary edits because its lifecycle is caller-managed. Renaming or replacing a source variable requires an explicit caller decision to retain or replace its key; the package does not infer identity across names.

The returned tables contain `variable_key` but never `name`, `source_schema`, `source_table`, `source_column`, relation identity or a key/name crosswalk. Validation errors use fixed field-level wording and never print names, keys or unmatched values. Documentation and tests use only neutral names and opaque example keys.

## State And Authority Separation

| Artifact | State field and generated value | Contents | Authority |
| --- | --- | --- | --- |
| `evidence` | `evidence_state = "descriptive"` | Aggregate observations and fixed status/reason codes. | Describes the profiled source snapshot only. |
| `proposals` | `proposal_state = "pending"` | Candidate fields and review prompts derived under the rules below. | Pending analyst review; never executable or approved. |
| Future approved rule set | Separate `rule_state = "approved"` contract under issue-272 | Explicit executable fields such as `valid_min`, `valid_max`, `allowed_values` and approved missing codes plus required provenance. | Analyst-authored and separately validated; never emitted or accepted by this function. |

The proposal table deliberately has no `approved`, `apply`, `valid_min`, `valid_max` or destination field. Changing `proposal_state` after return does not create a valid approved rule set. `issue-272` must reject this result if it is presented directly as executable input.

## Evidence Table

Return one row per specification row in specification order with this exact schema:

| Column | Type | Meaning |
| --- | --- | --- |
| `variable_key` | character | Caller-managed opaque key. |
| `evidence_state` | character | Always `descriptive`. |
| `declared_type` | character | Reviewed semantic type from the specification. |
| `profile_status` | character | `profiled` or `not_profiled`. |
| `evidence_code` | character | One fixed code from the status rules below. |
| `n` | integer | Source row count when profiling is supported, including zero. |
| `n_missing` | integer | Standard and reviewed-code missing count when supported. |
| `n_observed` | integer | `n - n_missing` when supported. |
| `n_unique` | integer | Distinct non-missing count without returned values when supported. |
| `n_infinite` | integer | Observed positive/negative infinity count for numeric/integer variables; otherwise typed `NA`. |
| `n_finite` | integer | Finite observed count for numeric/integer variables; otherwise typed `NA`. |
| `observed_min` | numeric | Minimum finite observed numeric value; typed `NA` when unavailable or inapplicable. |
| `observed_max` | numeric | Maximum finite observed numeric value; typed `NA` when unavailable or inapplicable. |
| `tukey_lower_fence` | numeric | `Q1 - 1.5 * IQR` over finite values; typed `NA` when unavailable or inapplicable. |
| `tukey_upper_fence` | numeric | `Q3 + 1.5 * IQR` over finite values; typed `NA` when unavailable or inapplicable. |
| `n_below_tukey` | integer | Finite values below the lower fence; typed `NA` for non-numeric variables. |
| `n_above_tukey` | integer | Finite values above the upper fence; typed `NA` for non-numeric variables. |

`evidence_code` is `profiled`, `zero_rows`, `all_missing`, `declared_identifier`, `missing_variable`, `incompatible_storage` or `unsupported_storage`. Explicit reviewed roles equal to `id` or `identifier` after trimming and case folding receive `profile_status = "not_profiled"`, `evidence_code = "declared_identifier"` and typed `NA` in every count/statistic field. No role is inferred from a name, uniqueness or database constraint.

Missing or incompatible variables also receive `not_profiled` and typed `NA` aggregates. Supported zero-row variables are `profiled` with zero counts, numeric `n_infinite = 0`, `n_finite = 0`, unavailable numeric statistics and `evidence_code = "zero_rows"`. Supported all-missing variables are `profiled`, reconcile `n_missing = n`, have `n_observed = n_unique = 0`, and use `evidence_code = "all_missing"`. A supported non-empty/non-all-missing variable uses `evidence_code = "profiled"`.

Counts must fit the existing canonical R integer contract or the complete call fails with a fixed value-free error. Numeric fields use finite values only and preserve canonical missing-code, type-7 quartile and 1.5-IQR definitions. `observed_min` and `observed_max` are explicitly descriptive extrema; they do not read from or write to semantic `min` and `max`.

## Pending Proposal Table

Return zero or one row per variable in specification order with this exact schema:

| Column | Type | Generated meaning |
| --- | --- | --- |
| `variable_key` | character | Opaque link to the evidence row and caller's private key map. |
| `proposal_state` | character | Always `pending`. |
| `candidate_type` | character | `binary` only under the exact binary rule; otherwise blank. |
| `units_review_required` | logical | `TRUE` only when the reviewed type is numeric, integer, date or datetime and semantic `units` is absent/blank. |
| `candidate_units` | character | Always blank in generated output; units cannot be inferred from names or observations. |
| `candidate_screening_min` | numeric | Lower Tukey fence only under the screening rule; otherwise typed `NA`. |
| `candidate_screening_max` | numeric | Upper Tukey fence only under the screening rule; otherwise typed `NA`. |
| `screening_basis` | character | `tukey_1_5_iqr` under the screening rule; otherwise blank. |
| `candidate_allowed_levels` | character | `0;1` only with the exact binary candidate; otherwise blank. |
| `candidate_missing_codes` | character | Always blank in generated output; sentinel meaning is never inferred. |
| `rationale_codes` | character | Semicolon-delimited fixed codes in the priority order below. |

A row is emitted only when at least one review prompt applies, and no proposal row is emitted for an explicitly declared identifier. Rationale codes use this fixed order: `units_not_declared`, `observed_integral_zero_one`, `finite_values_beyond_tukey`, `non_finite_values_present`. No rationale contains names, values, frequencies, source types, prose assembled from source metadata or project terminology.

### Unit Prompt

Set `units_review_required = TRUE` when the reviewed type is `numeric`, `integer`, `date` or `datetime`, the semantic units field is absent, missing or blank after the normal `epi_eda_spec()` character-field rules, and the reviewed role is not an identifier role. Keep `candidate_units` blank. This is a metadata-completeness prompt, not a proposed unit. It may appear for zero-row, all-missing, missing-variable or storage-incompatible rows because it does not depend on observations.

### Exact Binary Candidate

Set `candidate_type = "binary"` and `candidate_allowed_levels = "0;1"` only when all of these facts hold: the reviewed type is `numeric` or `integer`; the evidence row is profiled; `n_observed > 0`; `n_unique = 2`; `n_infinite = 0`; and the finite observed minimum and maximum are exactly 0 and 1. This aggregate predicate proves that both 0 and 1 were observed and no other non-missing value was observed without returning rows or a general level list.

The candidate never changes semantic `type`, produces a factor, rewrites observations or becomes an approved rule. A variable satisfying the binary predicate does not receive a Tukey screening proposal because an imbalanced binary distribution can make a legitimate level appear beyond a fence.

### Tukey Screening Prompt

For a profiled numeric/integer variable that is not an exact binary candidate, populate both candidate screening bounds and `screening_basis = "tukey_1_5_iqr"` only when both fences are finite and `n_below_tukey + n_above_tukey > 0`. The bounds are copied from descriptive evidence and never labelled `valid_min` or `valid_max`.

The prompt does not claim an error, outlier in the scientific sense, implausible measurement or invalid value. It identifies finite tail observations for analyst review only. Empty, all-missing, infinite-only and no-tail variables receive no screening fields. A zero-IQR fence may be proposed when finite tail counts are positive because the fixed rule is descriptive; the rationale and evidence make that degeneracy inspectable.

### Non-finite Prompt

Add `non_finite_values_present` when a profiled numeric/integer variable has `n_infinite > 0`. Do not convert infinity to missing, propose a missing code or infer a screening bound solely from this fact. The aggregate evidence remains separate for analyst interpretation.

### Levels And Missing Codes

The generator never enumerates observed character, factor, enum, date, datetime or general numeric/integer values. The only populated allowed-level string is the issue-authorised generic `0;1` binary candidate. It never proposes missing codes from frequency, extrema, labels, names or common sentinel-looking values. Analysts may edit a copy of the pending table for review, but that edited table remains pending and non-executable.

## Data-frame Profiling

Validate the complete source shape, specification and key map before profiling. Work column by column in specification order without copying or mutating the source. Standard `NA`/`NaN` and reviewed `missing_codes` use the canonical missing mask. Count distinct values internally but never retain or return the distinct values. Numeric/integer evidence uses the existing canonical numeric core only after storage compatibility is established. Unsupported list, matrix, raw, complex or semantic subclasses become fixed `not_profiled` rows rather than value-bearing errors.

Save and compare the source, specification and key map in tests, including a data.table source, so successful and failed calls cannot mutate by reference. The function writes no file and emits no message or warning on a valid call.

## PostgreSQL Profiling

Use the caller-owned `epi_eda_postgres_source` and the existing read-only repeatable-read transaction wrapper. Revalidate relation identity and catalogue before and inside the snapshot. The QC path returns one scalar aggregate row per supported variable, or a small fixed numeric follow-up row when canonical fence counts require it. It must not call canonical categorical frequency collection, catalogue profiling, Shapiro vector collection, plot preparation or any row-valued query.

For every supported variable, SQL computes total, missing, observed and distinct counts from allow-listed expressions and bound missing-code parameters. Numeric/integer SQL also computes finite extrema, type-7 quartiles and infinity counts, then a scalar fence-count pass when finite values exist. Every query has a fixed one-row client limit and query-kind allow-list entry. Timing, SQL, parameters, native database messages, connection attributes and relation identifiers are not returned.

High cardinality changes only the scalar `n_unique`; it never changes client row count. Nondeterministic text equality, incompatible storage, local timestamps without instant meaning and unsupported PostgreSQL types receive fixed `not_profiled` evidence without values. The operation commits no data change and leaves the caller's connection open and idle after success or failure.

## Cross-backend Equivalence And Determinism

Equivalent inputs use the same normalised specification, variable-key map, missing-code semantics and logical observations. Integer counts, states, codes, candidate decisions, strings and ordering must be identical. Numeric evidence must match the canonical combined tolerance of `1e-10 * max(1, abs(expected))`; tests use asymmetric values that discriminate type-7 quartiles and fence decisions. A candidate decision may not depend on a floating comparison except exact 0/1 extrema, which are exactly representable in both backends.

Repeated calls against an unchanged data frame or PostgreSQL snapshot return identical R objects for the same backend. No timestamp, query timing, source fingerprint or random identifier weakens that determinism.

## Semantic Dictionary Compatibility

The function normalises but never modifies `spec`. Semantic `type`, `units`, `levels`, `min`, `max` and `missing_codes` remain exactly as supplied and retain their existing meanings. Existing `epi_eda_spec_scaffold()`, `epi_eda_dictionary_scaffold()`, `epi_eda_dictionary_refresh()`, `epi_eda_dictionary_validate()`, `epi_eda_dictionary_spec()` and `epi_eda_prepare()` signatures, schemas and behaviour do not change.

Proposal fields use `candidate_` names. Screening fields cannot be passed to existing preparation as `min`/`max`; the first implementation adds no adapter that copies proposals into a semantic dictionary. Existing dictionary source keys remain private source identity and are not reused as result keys.

## Privacy, Errors And Printing

The result contains aggregate evidence that can still be sensitive in context. It is not automatically safe to save or share. The implementation creates no artifact or automatic report and documentation states that the caller must review aggregate disclosure risk.

The complete return, default print, `str()`, warnings and errors contain no source variable names, relation names, schemas, observed categorical/text values, general numeric value lists, row identifiers, examples, SQL, parameters or credentials. Default printing reports only the class, evidence-row count and pending-proposal count; it does not print keys or table contents. Direct access to the returned tables is explicit caller action.

Structural failures use fixed actionable messages naming only contract fields. Per-variable profiling incompatibility becomes a fixed evidence code instead of an error containing the variable or source type. Native PostgreSQL conditions continue through the existing sanitised condition path.

Package documentation, fixtures and tests use neutral synthetic inputs only. Runtime aggregate evidence necessarily describes the caller's source, but package-owned examples and committed expected outputs contain no real or source-specific result.

## Side Effects, Failure And Recovery

The operation has no filesystem, data mutation, database write, approval or publication side effect. Validate the public arguments and key coverage before opening a PostgreSQL transaction. Any structural, count-overflow, query-contract, connection or catalogue-drift failure returns no partial object. The PostgreSQL wrapper rolls back the read-only transaction when needed and preserves caller ownership.

Recovery requires correcting the input contract or constructing a new reviewed PostgreSQL source after catalogue drift, then repeating the call. There is no partial file, table, rule or cleaned data set to remove.

## Documentation And Dependencies

Roxygen, README, NEWS and the specification-first EDA vignette document the opaque key map, exact result schemas, review-only candidates, dictionary non-mutation, high-cardinality behaviour and approved-rule boundary. The worked example uses neutral synthetic variables and shows that the input remains identical after proposal generation.

No new dependency is required. Base R and current Imports are sufficient; RPostgres remains a Suggest required only for PostgreSQL-backed calls.
