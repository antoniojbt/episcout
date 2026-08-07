# Software Design

Spec ID: `019-postgresql-catalogue-missingness`
Status: Implemented

## Scope

Change only the output and SQL contract of approved catalogue profiling, plus
its focused tests and user documentation. Existing dictionary/privacy gates are
unchanged.

## Public API

`epi_db_catalogue_profile()` keeps its arguments and returns an
`epi_db_catalogue_profile` list with exactly:

- `values`: source keys, non-missing `source_value`, and frequency `n`;
- `missing`: one row per profiled source column with the same keys and
  aggregate `n_missing` for PostgreSQL `NULL`.

Both components are data frames with stable columns and numeric counts. When no
dictionary row is selected, both are typed zero-row frames. Each selected column
always has one `missing` row, including empty and all-NULL relations.

The returned `values` rows contain no missing `source_value` and can be mapped
directly into draft normalised catalogue rows. The caller must separately review
whether any source-specific sentinel is missing; PostgreSQL NULL itself is not a
catalogue source value.

This deliberate return-type change is allowed before the database dictionary API
is released in 0.3.0 and is recorded in NEWS.

## SQL And Data Flow

For each approved column:

1. Quote the reviewed schema, table and column identifiers through DBI.
2. Query `COUNT(DISTINCT column)` and a filtered PostgreSQL NULL count together.
3. Refuse the column when the non-missing distinct count exceeds `max_levels`.
4. Query grouped frequencies with `WHERE column IS NOT NULL`; order by the
   source value.
5. Append source keys to both value and missing components.
6. Bind all per-column components and return them in fixed order and class.

## Inputs And Outputs

- `max_levels` remains a positive whole number and bounds `nrow(values)` per
  profiled column.
- `n` and `n_missing` are numeric to match the existing DBI count convention.
- Zero rows: `values` has no rows; `missing$n_missing` is zero.
- All NULL: `values` has no rows; `missing$n_missing` is the table row count.
- Exactly at limit plus NULL: `values` has exactly `max_levels` rows; the NULL
  count remains one separate aggregate field.
- Limit plus one: profiling refuses before grouped value rows are returned.

## Errors And Privacy

Existing eligibility and connection errors remain. The over-limit error reports
only reviewed source metadata and aggregate cardinality. No observed value,
credential, SQL parameter or identifier is included in conditions or planning
records.

## Dependencies

No dependency changes. Unit tests use the existing DBI mock; mandatory live
tests use the existing optional RPostgres/PostgreSQL 17 CI service.
