# Software Design

Spec ID: `017-deterministic-local-time-ambiguity`  
Status: Completed  

## Scope And Correctness Basis

IANA tzdb 2026c defines `Pacific/Kwajalein` as UTC+11 until October 1969 and UTC-12 afterwards. The resulting 23-hour rollback repeats the issue fixture. `clock::naive_time_info()` independently classifies naive wall times as `unique`, `ambiguous` or `nonexistent` using the tzdb package data rather than host timezone files.

## Public API

`epi_eda_prepare()` retains its formals, return components, audit schema and statuses. Character local datetimes now use the timezone names and transition data exposed by `clock`; names unavailable to that engine block. `DESCRIPTION` declares `R (>= 4.0.0)` and imports `clock (>= 0.7.4)`.

## Data Flow

1. Keep the strict ISO shape and component checks, including leap-second rejection.
2. Parse `Z` and numeric-offset values with the existing UTC arithmetic.
3. For local values, require an exact member of `clock::tzdb_names()` and normalise the separator to `T`.
4. Parse the first 19 characters as second-precision naive times. Treat parse or timezone-engine errors as unavailable classification.
5. Accept only `clock::naive_time_info(... )$type == "unique"`; all other types count as invalid without recording values.
6. Convert unique naive times with `clock::as_date_time(..., nonexistent = "NA", ambiguous = "NA")`, normalise the returned POSIXct to UTC, and add the numeric fractional-second suffix separately.
7. Apply mode remains blocked when any invalid value exists, so no partial conversion is returned.

## Errors, Privacy And State

Unsupported timezone data, ambiguity, nonexistence and engine failure produce an actionable blocking audit reason recommending an explicit offset or corrected reviewed timezone. Audit rows and conditions include variable names, stages and counts only. The implementation does not modify `TZ`, `TZDIR`, locale or options.

## Dependencies And Compatibility

The current `clock` release supplies the required high-level ambiguity classification and conversion over bundled tzdb data. This avoids maintaining a timezone parser or compiled tzdata inside episcout. The owner explicitly accepted its R 4.0 minimum for full historical support. Verification records `clock` and tzdb versions because later IANA corrections may legitimately change historical results.
