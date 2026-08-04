# Software Design

Spec ID: `013-specification-guided-data-preparation`
Status: Completed

## Scope

Add one side-effect-free public preparation function that validates a reviewed EDA specification against an in-memory data frame, returns a comprehensive audit in both modes, and applies an approved deterministic transformation plan only when no blocking finding remains.

## Authority And Semantic Basis

The reviewed specification is authoritative for variable names, declared types, declared categorical levels, sentinel missing codes, requiredness and the optional datetime timezone policy. Observed data determine counts and whether the requested transformations are possible. Existing source values never establish new scientific semantics.

When the specification contains the scaffold evidence column `review_status`, apply mode requires every row to be exactly `reviewed`. Any other or missing status is a dataset-level blocker. Audit mode remains available so an untouched `review_required` scaffold can be diagnosed safely. Specifications without scaffold evidence retain the historical caller-asserted review contract.

The preparation path must reuse canonical standard/sentinel missing matching and strict ISO temporal grammar. Existing code and tests demonstrate prior behaviour but do not independently justify a conversion; expected results must follow this approved contract and hand-authored examples.

## Public API

```r
epi_eda_prepare(
  data,
  spec,
  mode = c("audit", "apply"),
  unexpected_levels = c("error", "append"),
  extra_variables = c("keep", "error", "drop")
)
```

`data` must be a data frame. `spec` is accepted through `epi_eda_spec()`. `mode`, `unexpected_levels` and `extra_variables` use exact `match.arg()` semantics, making audit, error and keep the respective safe defaults.

Return a named list with class `c("epi_eda_preparation", "list")` and fixed components in this order: `data`, `audit`, `schema_before`, `schema_after`, and `metadata`. No print method is required in v1.

## Mode And Blocking Contract

- Audit mode always returns the original input data unchanged and sets `schema_after = NULL`; it does not construct a candidate prepared dataset.
- Audit mode reports preparation blockers in `audit` and `metadata` rather than throwing for data-dependent findings.
- Apply mode resolves the complete plan before copying or transforming any column. If a blocker exists, it returns the original data unchanged, `schema_after = NULL`, and blocked metadata.
- Successful apply mode returns a fully prepared copy and computes `schema_after` from that complete copy.
- Malformed top-level arguments, an invalid specification shape, duplicate data column names that prevent unambiguous lookup, or an internally inconsistent audit contract are function errors. Observed incompatibilities, absent required variables, unexpected levels and conversion failures are reported blockers, not early errors.
- No mode mutates the caller's object by reference.

## Overall Status

`metadata$overall_status` is one of `audited`, `blocked`, or `prepared`. Audit mode returns `audited` even when blocking actions exist and separately reports `n_blocking`; blocked apply returns `blocked`; successful apply returns `prepared`.

## Output Data Contract

Source row count and row order are unchanged in every returned dataset. Audit and blocked apply return the original input object exactly; successful apply returns an ordinary data frame with source row names preserved. Present specification variables are ordered in specification order. With `extra_variables = "keep"`, extra variables follow in their original relative order; with `"drop"`, they are absent after successful apply; with `"error"`, their presence blocks apply. Missing optional or unasserted variables are not created.

`required = TRUE` makes absence blocking. `required = FALSE`, missing `required`, and typed `NA` requiredness all mean optional/unasserted for preparation: absence is audited and skipped without blocking.

## Audit Contract

Return one ordinary data frame with fixed columns:

| Column | Type | Meaning |
| --- | --- | --- |
| `name` | character | Specification or extra variable name; dataset-level rows use a reserved `.dataset.` prefix. |
| `stage` | character | `dataset`, `presence`, `missingness`, `type`, or `levels`. |
| `declared_type` | character | Specification type or typed `NA` for dataset/extra checks. |
| `observed_class_before` | character | Source class or typed `NA` when absent/not applicable. |
| `observed_class_after` | character | Planned/applied class or typed `NA` when unavailable. |
| `action` | character | Stable machine-readable action identifier. |
| `status` | character | `unchanged`, `planned`, `applied`, `skipped`, `warning`, or `blocking`. |
| `n_total` | integer | Source row count for the audited variable or dataset. |
| `n_standard_missing` | integer | Standard missing count before sentinel replacement, or typed `NA` when not applicable. |
| `n_sentinel_missing` | integer | Non-standard values matching declared sentinel codes, or typed `NA` when not applicable. |
| `n_invalid` | integer | Remaining observations invalid for the declared conversion, or typed `NA` when not applicable. |
| `n_unexpected` | integer | Remaining categorical observations outside declared levels, or typed `NA` when not applicable. |
| `n_affected` | integer | Rows affected by the reported stage, including duplicate rows, sentinel matches, invalid rows or extra-column handling. |
| `n_changed` | integer | Rows that would change in audit mode or did change in successful apply mode; zero for report-only and blocked actions. |
| `reason` | character | Deterministic, value-free explanation. |

Counts on different stage rows describe that stage and must not be summed as if rows were disjoint. For each present specification variable, `n_standard_missing + n_sentinel_missing + n_remaining = n_total`, where `n_remaining` is derived during implementation review; `n_invalid` and `n_unexpected` are subsets of remaining observations. All counts are non-negative integers or typed `NA` only where the field is not applicable.

Audit rows are ordered as dataset rows, specification variables in specification order and stage order, then extra variables in source order. Dataset-level names are reserved and begin with `.dataset.`, including `.dataset.dimensions`, `.dataset.duplicate_rows`, `.dataset.zero_shape` and `.dataset.column_names`. A real source/specification name beginning `.dataset.` is rejected because it would collide with the audit namespace.

`.dataset.spec_review` reports the scaffold review gate when `review_status` is present. It is unchanged when every row is `reviewed` and blocking otherwise; its reason and counts never reproduce status values from the specification.

Audit and errors may include column names, declared types, observed classes, counts and policy names. They must never contain observed values, sentinel values, unexpected levels, invalid strings, timestamps, minima, maxima or example records.

## Metadata Contract

Return a one-row data frame containing `mode`, `overall_status`, `n_rows_before`, `n_columns_before`, `n_rows_after`, `n_columns_after`, `n_unchanged`, `n_planned`, `n_applied`, `n_skipped`, `n_warning`, and `n_blocking`. After-dimension fields equal the returned prepared dimensions only on successful apply and are typed `NA` in audit or blocked apply. Status counts count audit action rows, not source observations.

## Missingness And Sentinel Replacement

Standard `NA` and `NaN` remain missing. Sentinel matching uses the canonical `summary_missing_mask()`/`eda_missing_codes()` character-comparison semantics, but `n_sentinel_missing` excludes values already standard missing. Empty and whitespace-only text, literal `"NA"`, infinities and other values remain observed unless their exact character representation is declared in `missing_codes`.

The v1 semicolon-delimited `missing_codes` representation trims entries and uses semicolon as its delimiter. It therefore cannot declare an empty string, a whitespace-only string or a sentinel containing a semicolon. Preparation does not invent a private escape syntax; those values remain observed until a separately approved specification encoding supports them.

Sentinel replacement occurs conceptually before type validation. A successful apply uses the correctly typed missing value for the destination: `NA_real_`, `NA_integer_`, `NA_character_`, factor `NA`, `as.Date(NA)`, or `as.POSIXct(NA, tz = ...)`. A sentinel declared level remains in factor metadata with zero observed count after replacement.

## Numeric Semantics

- Base double storage remains numeric after typed sentinel replacement.
- Base integer storage may be converted to double without loss for declared numeric output.
- Non-finite numeric values remain observed unless explicitly declared sentinels.
- Character or factor storage declared numeric is blocking in v1, even if some strings appear parseable, because locale, grouping and decimal rules are absent.
- When every non-standard character value is a declared sentinel, v1 still blocks character-to-numeric conversion rather than introducing a special implicit parsing exception.

## Integer Semantics

- Base integer storage remains integer after typed sentinel replacement.
- Base double storage converts only when every remaining observed value is finite, whole-valued and within `[-.Machine$integer.max, .Machine$integer.max]`.
- A non-whole, infinite or out-of-range remaining value increments `n_invalid` and blocks apply; values are never rounded, clipped or converted to missing.
- Character/factor-to-integer parsing is blocking in v1 for the same locale and encoding reasons as numeric parsing.

## Declared Range Semantics

Optional `min` and `max` fields remain reviewed descriptive or future validation metadata in v1. Preparation does not enforce them, clamp values, convert out-of-range observations to missing or report them as conversion failures. A later validation feature may add range findings without changing the type-preparation contract.

## Categorical And Binary Semantics

Specification `levels` are authoritative and retain declared order and zero-count levels. Missing codes are applied before level comparison. Character, factor, base integer and base numeric values may be converted only by exact comparison of their character representations with declared levels.

Categorical rows require at least one declared level. Binary rows require exactly two distinct declared levels, except logical binary input may omit `levels` and then uses the explicit fixed declaration `FALSE;TRUE`. Logical input with any other declaration is blocking rather than being relabelled by position.

Unexpected values are counted without being reported. With `unexpected_levels = "error"`, any unexpected value blocks apply. For categorical variables under `"append"`, unique unexpected character representations are appended after declared levels in deterministic radix-sorted order, the factor is created without changing observations to missing, and the levels audit row receives a warning explaining that prepared data diverge from the reviewed specification. Binary variables never append an unexpected third value because that would violate the declared binary type; any binary unexpected value remains blocking under both policies. The specification object is never mutated or returned as silently revised.

Factor conversion uses displayed labels, never underlying integer codes. Literal `"NA"` is an observed level unless declared missing. Declared and appended levels must remain compatible with the package's semicolon-delimited level contract; unsafe metadata blocks rather than being escaped privately.

## Text Semantics

Character storage remains character after typed sentinel replacement. Factor storage converts with `as.character()` so labels, not underlying codes, are preserved. No other storage converts implicitly to text in v1. Empty and whitespace-only strings remain observed unless exactly declared missing.

## Date Semantics

`Date` and `IDate` storage remain compatible and retain their supported class where replacement permits. Character dates convert only when every remaining observation matches and round-trips through strict `YYYY-MM-DD` parsing. Invalid values are counted and block; they are never converted to missing. Other storage is blocking.

## Datetime And Timezone Semantics

Compatible `POSIXct` and `POSIXlt` storage is preserved, including its source timezone metadata, after typed sentinel replacement. Character datetime conversion uses the existing strict ISO grammar.

Character timestamps containing `Z` or an explicit numeric offset are normalised to `POSIXct` UTC. Local ISO timestamps without an offset require a non-blank optional specification `timezone` value on that datetime row. The timezone must be `UTC` or an exact member of `OlsonNames()` and is used only to interpret local wall time; converted character datetime output is normalised to UTC.

A mixed vector of local and offset/Z timestamps is allowed only when the reviewed timezone is present for its local members, and all converted values are returned in UTC. Invalid, nonexistent daylight-saving wall times and ambiguous repeated wall times without an explicit offset are blocking and are counted without reporting the timestamp. A `timezone` value on a non-datetime row is ignored with a warning audit row rather than altering that variable.

The optional `timezone` field remains an extra EDA specification column and does not change the four currently required specification columns. It must round-trip as character through `epi_eda_spec()`.

## Presence, Extras And Unsupported Columns

- Missing required variables produce a blocking presence row.
- Missing optional/unasserted variables produce a skipped presence row.
- Extra variables with `keep` are retained and audited unchanged; with `error` each is blocking; with `drop` each is planned/applied as an explicit column removal.
- Unsupported or nested specified columns are blocking.
- Unsupported extra columns are warning/unchanged under `keep`, blocking under `error`, and safely removable under explicit `drop`.
- Duplicate data column names are a top-level ambiguity error. Empty names and names using the reserved `.dataset.` prefix are also rejected.

## Dataset Checks

`.dataset.dimensions` records source dimensions and successful resulting dimensions through metadata. `.dataset.duplicate_rows` reports `sum(duplicated(data))`, meaning repeated rows after their first occurrence, as a warning with no deletion. `.dataset.zero_shape` reports zero rows or zero columns without inventing failure when all requested transformations remain well-defined. No key-based duplicate check is performed.

## Planning And Apply Algorithm

1. Validate top-level arguments, normalise the specification and validate audit namespace/name constraints.
2. Compute `schema_before` exactly once using `epi_eda_check_schema(data, spec)`.
3. Build dataset, presence, missingness, type, level and extra-variable audit plans for every relevant column without constructing candidate output.
4. Return the audit result immediately in audit mode with original data and no `schema_after`.
5. In apply mode, return a blocked result with original data if any plan row is blocking.
6. Otherwise copy and transform every specification column from the resolved plan, then apply explicit extra-variable retention/removal and final column ordering.
7. Validate row/order invariants, construct `schema_after`, convert planned statuses to applied, populate successful after classes/dimensions and return the complete object.
8. Treat any internal plan/application discrepancy as an error and never return the internal partial copy.

## Integration And Reconciliation

On successful apply, sentinel counts in the preparation audit must reconcile with standard missingness in prepared data. `schema_after` must report every present prepared specification variable as compatible; append-mode divergence remains explicit in the audit. Passing prepared data and the unchanged reviewed specification to `epi_eda_run()` must preserve row counts and canonical missingness totals.

## Side Effects And Privacy

The core is in-memory only and accepts no output path. It does not write data, schemas or audit artifacts, print raw values, create directories, use randomness, inspect values for PII or invoke pseudonymisation. File output belongs to the later intake orchestrator and must preserve this privacy boundary.

## Dependencies

No new dependency is planned. Base R and existing internal EDA helpers are sufficient.

## Recovery

Keep implementation confined to the feature branch. If datetime ambiguity, canonical sentinel matching or an existing public compatibility rule conflicts with this contract, stop before changing production semantics, record the question in `review.md` and obtain a decision rather than accepting partial mutation or silent loss.
