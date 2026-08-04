# Software Design

Spec ID: `012-data-frame-eda-spec-scaffold`
Status: Completed

## Scope

Add a conservative public scaffold builder for an existing in-memory data frame. It creates the common EDA specification columns plus review evidence, validates the completed object with the existing EDA specification validator, and performs no source mutation or filesystem operation.

## Required Behaviour And Basis

The source data frame is authoritative only for column names, storage classes and aggregate structural evidence observed in that delivery. Human review is authoritative for analytical types that are merely candidates and for all epidemiological, privacy and validation semantics. Existing package code supplies compatible representation and missingness conventions but is not independent evidence that a semantic inference is correct.

Standard `NA` and `NaN` are missing because no reviewed sentinel declaration exists yet. Values such as `"Unknown"`, `999`, empty strings and infinities remain observed. `n_unique` follows the canonical summary contract by counting distinct non-missing observed values after character representation.

## Public API

```r
epi_eda_spec_scaffold(
  data,
  max_candidate_levels = 20L
)
```

`data` must be a data frame or subclass such as a tibble or data table. The function does not accept a path and does not read data.

`max_candidate_levels` must be one finite, non-missing, positive whole number. It controls only whether observed character or integer cardinality is recorded as a review candidate and never changes the initial type or causes observed values to be emitted.

## Return Contract

Return an ordinary data frame with one row per source column in source order and these columns in fixed order:

| Column | Type | Meaning |
| --- | --- | --- |
| `name` | character | Exact source column name. |
| `label` | character | Initially equal to `name`. |
| `type` | character | Conservative initial EDA type derived from supported storage. |
| `role` | character | Blank pending review. |
| `units` | character | Blank pending review. |
| `levels` | character | Declared factor metadata or fixed logical metadata only; otherwise blank. |
| `min` | character | Blank because observed extrema are not validation limits. |
| `max` | character | Blank because observed extrema are not validation limits. |
| `missing_codes` | character | Blank because sentinel values are not inferred. |
| `required` | logical | Typed `NA` because one delivery does not establish a future contract. |
| `group` | character | Blank pending review. |
| `description` | character | Blank pending review. |
| `observed_class` | character | Full source class vector joined with `/`. |
| `n` | integer | Source vector length. |
| `n_missing` | integer | Count of standard `NA` and `NaN`. |
| `n_observed` | integer | `n - n_missing`. |
| `n_unique` | integer | Distinct non-missing observed values under the canonical character-representation convention. |
| `candidate_type` | character | A deterministic review hint or blank. |
| `candidate_levels` | character | Reserved evidence field, blank in v1 so no observed values or duplicate declared metadata are emitted. |
| `review_status` | character | `review_required` for every non-empty scaffold row. |
| `review_reason` | character | Deterministic explanation based only on names, classes, counts and structural policy, never observed values. |

The zero-column result has the same typed schema with zero rows. The result must be accepted by `epi_eda_validate_spec()` and preserve its row order and extra evidence columns through `epi_eda_spec()`.

## Initial Type Policy

| Observed storage | Initial `type` | Core level metadata |
| --- | --- | --- |
| base integer | `integer` | Blank. |
| base double/numeric | `numeric` | Blank. |
| logical | `binary` | `FALSE;TRUE`. |
| factor or ordered factor | `categorical` | Factor levels in their declared order. |
| `Date` or `IDate` | `date` | Blank. |
| `POSIXct` or `POSIXlt` | `datetime` | Blank. |
| character | `text` | Blank. |

Class-specific temporal and factor checks occur before generic atomic or list checks. An object whose underlying storage is numeric but whose semantic class is not explicitly supported, including `integer64`, `difftime` or a labelled numeric class, is unsupported rather than silently treated as numeric. Complex, raw, matrix, nested-data-frame, list and other unrecognised columns are unsupported.

## Candidate Policy

- A non-empty base integer column within `max_candidate_levels` receives `candidate_type = "binary"` when it has exactly two distinct observed values and `candidate_type = "categorical"` otherwise.
- A base numeric column receives `candidate_type = "integer"` only when it has at least one observed value and every observed value is finite, whole-valued and within R's representable non-missing integer range. Infinities and out-of-range values prevent this candidate.
- A character column receives `candidate_type = "date"` or `"datetime"` only when it has at least one observed value and every observed value satisfies the existing strict ISO parser. Datetime takes precedence only when the datetime shape matches; date and datetime candidates are mutually exclusive.
- A non-temporal, non-empty character column within `max_candidate_levels` receives `candidate_type = "binary"` when it has exactly two distinct observed values and `candidate_type = "categorical"` otherwise.
- Logical, factor, Date/IDate and POSIX storage already supplies the initial type, so `candidate_type` is blank.
- Empty, zero-row and all-missing columns receive no value-based candidate.
- Candidate classification does not change `type`, populate semantic fields, coerce data or enumerate observed values.

`candidate_levels` is blank for every v1 row. Safe factor and logical declarations belong in the core `levels` field; observed integer and character values are never enumerated. Retaining the evidence column keeps a stable review schema without duplicating declared metadata or implying that value candidates were approved.

## Level Encoding Safety

Factor and ordered-factor metadata are accepted only when every declared level is non-missing, non-empty, contains no semicolon and equals its own `trimws()` result. Internal whitespace and the literal string `"NA"` remain allowed. When `"NA"` is the sole factor level, encode the level field as `NA;` so base `write.csv()` and `read.csv()` do not turn the quoted field into a missing cell; existing `eda_spec_levels()` ignores the trailing empty token and recovers the literal level. Logical metadata are always the fixed safe string `FALSE;TRUE`.

If any factor column has unsafe level metadata, preflight validation fails and lists the blocking column and class without printing the level value. The implementation must not escape into a private encoding that existing `eda_spec_levels()` cannot read and must not return a partially built scaffold.

## Name And Column Preflight

- Reject non-data-frame input before inspecting columns.
- Preserve non-syntactic names exactly by constructing with `check.names = FALSE`.
- Reject missing, empty or whitespace-only names and duplicate names before producing rows.
- Detect every unsupported column and every unsafe factor metadata column, then report all blocking column names and classes in one actionable error where practical.
- Errors may expose source column names and classes because those are required to repair the structure, but they must not contain observed values or declared level values.
- POSIXlt must not be rejected merely because its internal representation is list-like.

## Counts And Structural Evidence

For each supported vector, calculate `missing <- summary_missing_mask(values)` with no sentinel codes, `n <- length(values)`, `n_missing <- sum(missing)`, `n_observed <- n - n_missing`, and `n_unique <- length(unique(as.character(values[!missing])))`. Infinities are observed and count toward `n_observed` and `n_unique`; no finite-only analytical statistics are computed.

`observed_class` is `paste(class(values), collapse = "/")`. Datetime timezone may be mentioned in `review_reason` using the existing safe timezone helper, but no additional public evidence column is added under this spec.

## Review Fields

Every returned row has `review_status = "review_required"`. `review_reason` states that storage supplied the draft type and, where applicable, identifies a candidate based on strict shape, whole-valued storage or cardinality counts. It also reminds the user that semantic fields and missing codes require review. Reasons must be deterministic and may include aggregate counts but not observed values.

## Data Flow

1. Validate `data`, `max_candidate_levels` and source names.
2. Classify every column and collect unsupported-class and unsafe-level blockers without modifying the source.
3. Stop once with actionable structural blockers before constructing a partial result.
4. For each supported column in source order, calculate standard missingness counts, cardinality, initial type, safe metadata levels and candidate type.
5. Bind rows against an explicit typed empty constructor.
6. Validate the complete result with `epi_eda_validate_spec()` and return it as an ordinary data frame.

## Reuse And Compatibility

Reuse `summary_infer_type()`, `summary_missing_mask()`, `eda_all_iso_dates()`, `eda_all_iso_datetimes()` and `summary_temporal_timezone()` only where their exact semantics match this design. Guard the ISO helpers against zero observed values because their schema-compatibility contract treats empty input as vacuously parseable.

The implementation may extract a small internal storage-class helper shared with typed `epi_stats_summary()`, but it must not route typed `epi_stats_summary()` through the new public scaffold because that interface currently records unsupported columns as skipped rather than failing the whole call.

Do not use `epi_clean_class_to_factor()` or another automatic conversion helper. Do not change `epi_eda_dictionary_scaffold()`, database dictionary validation, sentinel parsing or the canonical summary contract.

## Side Effects And Privacy

The function does not mutate `data`, change classes, create directories, write files, log values, seed randomness or depend on global options. It emits no raw examples or observed candidate levels. The returned object can still contain source column names and factor-level metadata, so documentation must tell users to review privacy before saving or sharing it.

## Documentation

Roxygen, README, NEWS and the specification-first EDA vignette must describe the scaffold as a human-review starting point rather than a scientific or privacy approval. The worked example starts with a small neutral in-memory received dataset, saves the draft to CSV, performs explicit human-equivalent edits, reloads it with `epi_eda_spec()` and passes it to `epi_eda_run()`.

Documentation must distinguish this API from the database inventory scaffold and state that roles, labels, units, missing sentinels, categorical declarations, privacy classification and validation ranges require review.

## Dependencies

No new dependency is required. Base R plus existing package internals are sufficient.

## Recovery

Keep all implementation in the feature branch and avoid unrelated refactors. If privacy-safe candidate evidence, declared-level encoding or a storage subclass cannot meet this approved contract, stop before changing the public behaviour, record the issue in `review.md` and obtain an explicit decision rather than weakening the no-raw-values or no-silent-coercion requirements.
