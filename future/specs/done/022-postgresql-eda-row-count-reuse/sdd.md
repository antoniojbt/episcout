# Software Design

Spec ID: `022-postgresql-eda-row-count-reuse`
Status: Active

## Scope

Change only the internal categorical/binary summary call path. `eda_postgres_summaries_inside()` already receives a checked `n_total` from `epi_eda_db_run()` and obtains one itself when called through a direct profiler. It will pass that value to `eda_pg_categorical_summary()`, which will no longer query the relation count.

## Public API

No public API changes. The exported formals, S3 result classes, canonical summary schemas, bundle artifacts and timing schema remain unchanged.

The internal signature becomes:

```r
eda_pg_categorical_summary(
  source,
  column,
  contract,
  spec_row,
  index,
  n_total,
  timing_env
)
```

`n_total` is required on this internal path. It is the checked integer returned by the workflow's transaction-local `eda_postgres_row_count()` call.

## Inputs And Outputs

- Input `n_total`: one non-negative checked integer representing the relation row count in the current snapshot.
- Categorical output: unchanged complete frequency rows with `p_total = n / n_total`, `p_observed = n / n_observed`, declaration flags and the existing count reconciliation fields.
- Timing output: exactly one truthful `row_count` entry per `epi_eda_db_run()`; categorical-frequency timing entries remain per variable.

## Data Flow

1. `epi_eda_db_run()` begins the existing read-only repeatable-read transaction.
2. `eda_postgres_row_count()` executes once and returns checked `n_total`.
3. The workflow passes `n_total` to missingness, summaries and identifier QA as it does now.
4. `eda_postgres_summaries_inside()` passes `n_total` to every categorical/binary helper invocation.
5. The helper computes proportions and missing counts from the supplied value without issuing a relation-count query.
6. Existing reconciliation, plot preparation, staging, checksums and atomic publication continue unchanged.

Direct `epi_eda_profile_summaries()` dispatch remains compatible because `eda_postgres_summaries_inside()` retains its existing fallback: when `n_total` is absent, it queries it once before iterating variables.

## Edge Cases

- Zero-row relation: declared categorical levels remain with zero counts and unavailable proportions, using `n_total = 0L`.
- All-missing categorical/binary variable: `n_observed = 0L`, `n_missing = n_total`, and proportions retain current semantics.
- Multiple categorical/binary variables: the timing inventory still contains one row-count entry.
- Missing, incompatible or identifier-role variables: existing skip and reconciliation behavior is unchanged.
- Concurrent writes: the supplied count and frequencies remain from the same repeatable-read snapshot.

## Errors And Warnings

No new user-facing conditions. Existing checked-count, database-query, transaction and reconciliation failures remain authoritative and value-free.

## Dependencies

No dependency changes.
