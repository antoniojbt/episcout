# Software Design

Spec ID: `041-reviewed-civil-date-derivation`

Status: Active

## Authority And Temporal Boundary

Civil-date meaning is a caller-reviewed declaration, not an inference from observations, source names or storage. The package validates the declaration and its opaque external approval reference but does not authenticate the analyst or decide whether the declaration is scientifically justified.

The operation is calendar-local only. It never assigns or infers a timezone, never converts between timezones and never treats a local timestamp as an instant. Existing rejection of PostgreSQL `timestamp without time zone` by instant-oriented preparation remains unchanged. In memory, only a strict character representation is supported; `POSIXct` and `POSIXlt` are rejected because their numeric/timezone representation is instant-oriented.

## Public Interfaces

```r
epi_eda_approved_civil_dates(operations)

epi_eda_derive_civil_dates(
  data,
  operations,
  variable_keys,
  output_path = NULL,
  output_format = NULL,
  destination_schema = NULL,
  destination_table = NULL
)
```

`epi_eda_approved_civil_dates()` validates, canonicalises and sorts a non-empty operation data frame and returns class `c("epi_eda_approved_civil_dates", "data.frame")`. Application requires the unmodified class and rebuilds it to detect later mutation. `variable_keys` is the caller-owned exact `name`/`variable_key` crosswalk used by the approved cleaning workflow; every source key must resolve exactly once.

Data-frame application returns class `c("epi_eda_civil_date_result", "list")` with the complete derived data frame in `data` and aggregate-only reconciliation in `audit`. PostgreSQL application returns the same class with `data = NULL` because rows remain server-side.

## Exact Approved-Operation Schema

The input contains exactly these columns in this order:

| Column | Type | Contract |
| --- | --- | --- |
| `source_variable_key` | character | Unique caller-managed opaque key matching `^var_[a-z0-9]{16,64}$`. |
| `derived_name` | character | Unique, non-empty, valid UTF-8 name for the new column. |
| `operation_state` | character | Exactly `approved`. |
| `declared_semantics` | character | Exactly `civil_date`. |
| `preserve_source` | logical | Non-missing and exactly `TRUE`. |
| `require_midnight` | logical | Non-missing and exactly `TRUE`. |
| `approval_id` | character | Opaque caller-managed reference matching `^approval_[a-z0-9]{16,64}$`. |

Source keys and derived names are unique. Extra, missing or reordered fields, zero operations, pending states, any other semantic declaration, false/missing safeguards, malformed keys/names/approvals and mutated operation objects fail before source processing. Canonical order is the source opaque-key order.

## In-Memory Validation And Derivation

Every resolved source column must exist and have exact base character storage. Each non-missing value must use `YYYY-MM-DD HH:MM:SS` with an optional decimal fraction, contain a valid proleptic Gregorian date with a four-digit year from 0001 through 9999, and contain a valid 00–23 hour and 00–59 minute/second. `T`, offset and zone suffixes are rejected rather than interpreted.

All source classes, lexical forms, destination collisions and midnight conditions are validated across all operations before the result is copied. Exact midnight means hour, minute and second are zero and every supplied fractional digit is zero. Fractional-zero midnight is valid; any non-zero time or fractional digit contributes one affected source value. If the aggregate count is non-zero, the call reports only that count and creates nothing.

After validation, each derived column is `as.Date()` of the already validated first ten characters. Missing input produces typed `Date` missingness. New columns are appended in canonical operation order. Source vectors, source column order and values, rows, row order and row names remain unchanged.

## File Publication

File arguments follow the existing approved-cleaning contract: both path and explicit format are absent for memory-only results or both are present; formats are exactly `csv` and `rds`; existing destinations are refused. The complete derivation and reconciliation finish before the existing same-directory staged, no-replace publication path runs. RDS is reconciled exactly; CSV is reconciled by complete dimensions because CSV cannot preserve the `Date` class. Failure removes only staging or a destination created by the current call.

## PostgreSQL Validation And Derivation

PostgreSQL requires both new destination identifiers and rejects file arguments. The destination schema must exist; the destination relation must differ from the source and not already exist. Every resolved source column must have the PostgreSQL local-datetime storage family backed by `timestamp without time zone`; `timestamp with time zone` and other storage are rejected.

One repeatable-read write transaction revalidates source catalogue identity and destination availability. For each operation, one scalar server-side aggregate uses a guarded `CASE` to count non-finite timestamps or finite `source::time <> TIME '00:00:00'` while excluding missing values. Counts are summed in R and any non-zero total blocks before `CREATE TABLE AS`. No timestamp or row is collected.

Creation selects every source column unchanged and appends each `source::date AS derived` expression. Both casts are calendar-local PostgreSQL casts and do not introduce a timezone. Destination row count, exact expected names, derived `date` storage and source-to-derived missing counts reconcile before commit. Any failure rolls back the new table. PostgreSQL relations have no physical row-order contract; the projection does not filter or explicitly reorder rows.

## Aggregate Audit

`audit` contains `summary` and `operations`. `summary` records a deterministic operation-set SHA-256 hash, publication kind, source/destination dimensions, operation count, total source/derived missing counts, a zero successful non-midnight count and reconciliation flags. `operations` contains only opaque source keys, row counts, source/derived missing counts and reconciliation flags.

The hash covers the canonical exact approved-operation data frame, including the derived name and approval reference. Returned audits and custom display methods omit source names, derived names, paths, relation identities, timestamps and approval references. Aggregate counts and hashes can still be sensitive in context and require caller review before sharing.

## Determinism And Compatibility

Equivalent canonical operations and unchanged source values yield the same hash, derived dates and audit counts. Existing approved-cleaning, dictionary, preparation and PostgreSQL compatibility APIs do not change. PostgreSQL `CREATE TABLE AS` does not preserve source constraints, indexes or privileges. No new dependency is required.
