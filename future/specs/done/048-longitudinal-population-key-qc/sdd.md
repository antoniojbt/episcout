# Software Design

Spec ID: `048`
Status: Completed

## Scope

The operation compares entity membership and optional within-period record-key quality across at least two completed PostgreSQL relations. It consumes reviewed `epi_eda_postgres_source` objects, treats caller list order as time order and retains only aggregate evidence. It neither constructs a longitudinal row-level dataset nor changes any source.

## Public API

```r
epi_eda_longitudinal_qc(sources, entity_id, record_key = NULL)
```

`sources` is a named list of at least two unmodified `epi_eda_postgres_source` objects. Names are non-empty, unique period labels and list order is semantic. Every source uses the same caller-owned, open, idle RPostgres connection. `entity_id` is one plain column identifier present in every source. `record_key` is `NULL` or a non-empty unique character vector of plain column identifiers present in every source.

Entity columns must share one supported family: `text`/`varchar`, `int2`/`int4`/`int8`, or `uuid`. Text equality requires deterministic collations. Entity membership is exact after a database-local cast to text with `C` collation; it excludes nulls and, for the text family, representations whose trimmed text is empty. It does not otherwise trim or normalise values.

Every declared record-key column must have the same base PostgreSQL type in every source. Supported base types are `bool`, `int2`, `int4`, `int8`, `float4`, `float8`, `numeric`, `text`, `varchar`, `bpchar`, `date`, `timestamp`, `timestamptz`, `time`, `timetz` and `uuid`. Collatable keys require deterministic collations. A record key is complete only when every component is non-null and every text, varchar or bpchar component has a non-empty trimmed textual representation. Duplicate evidence groups complete native PostgreSQL key tuples; incomplete keys do not contribute to distinct or duplicate-key calculations.

## Stable Result

The result has exact class `c("epi_eda_longitudinal_qc", "list")` and exact components in this order: `metadata`, `period_summary`, `adjacent_membership`, `pairwise_overlap`, `history_summary`, `issues`.

`metadata` is a one-row data frame with these columns and scalar types:

1. `contract_version` (character, `"longitudinal-qc-1"`)
2. `n_periods` (integer)
3. `entity_id` (character)
4. `record_key_declared` (logical)
5. `n_record_key_columns` (integer)
6. `period_labels` (a one-element list column containing the ordered character labels)
7. `source_fingerprints` (a one-element list column of non-sensitive source fingerprints)
8. `source_set_fingerprint_sha256` (character)

`period_summary` has one row per period in caller order and these columns:

1. `period_index` (integer)
2. `period_label` (character)
3. `n_rows` (double)
4. `n_entity_null` (double)
5. `n_entity_blank` (double; zero for non-text identifiers)
6. `n_entity_nonblank` (double)
7. `n_valid_entity_rows` (double)
8. `n_distinct_entities` (double)
9. `n_repeated_entity_rows` (double)
10. `n_repeated_entity_excess` (double)
11. `max_entity_frequency` (double)
12. `n_missing_record_key` (double)
13. `n_complete_record_key_rows` (double)
14. `n_distinct_record_keys` (double)
15. `n_duplicate_record_key_groups` (double)
16. `n_duplicate_record_key_rows` (double)
17. `n_duplicate_record_key_excess` (double)
18. `max_record_key_frequency` (double)

The seven record-key fields are `NA_real_` when `record_key` is `NULL`. Repeated entities are descriptive and never create issues.

`adjacent_membership` has one row for each consecutive pair and these columns:

1. `from_period_index` (integer)
2. `from_period_label` (character)
3. `to_period_index` (integer)
4. `to_period_label` (character)
5. `n_from_entities` (double)
6. `n_to_entities` (double)
7. `n_union` (double)
8. `n_retained` (double)
9. `n_exited` (double)
10. `n_entered` (double)
11. `retention_numerator` (double)
12. `retention_denominator` (double)
13. `retention_proportion` (double)
14. `exit_denominator` (double)
15. `exit_proportion` (double)
16. `entry_numerator` (double)
17. `entry_denominator` (double)
18. `entry_proportion` (double)

`pairwise_overlap` has one row for every increasing pair of period indices, ordered first by left then right index, and these columns:

1. `left_period_index` (integer)
2. `left_period_label` (character)
3. `right_period_index` (integer)
4. `right_period_label` (character)
5. `n_left_entities` (double)
6. `n_right_entities` (double)
7. `n_overlap` (double)
8. `n_union` (double)
9. `n_left_only` (double)
10. `n_right_only` (double)
11. `left_overlap_denominator` (double)
12. `left_overlap_proportion` (double)
13. `right_overlap_denominator` (double)
14. `right_overlap_proportion` (double)

`history_summary` groups distinct valid entities by first observed period, last observed period, number of observed periods and derived number of unobserved periods inside that span. It is ordered by first index, last index and observed-period count, with these columns:

1. `first_period_index` (integer)
2. `first_period_label` (character)
3. `last_period_index` (integer)
4. `last_period_label` (character)
5. `periods_observed` (integer)
6. `gap_periods` (integer)
7. `has_gap` (logical)
8. `n_entities` (double)
9. `proportion_denominator` (double)
10. `proportion` (double)

`issues` has this exact seven-column schema, including when it has zero rows:

1. `issue_code` (character)
2. `severity` (character)
3. `period_index` (integer)
4. `period` (character)
5. `variable` (character; the caller's entity identifier or comma-joined record-key components; `NA` for an empty period)
6. `n_affected` (double)
7. `message` (character)

Only `empty_period`, `invalid_entity_id`, `missing_record_key` and `duplicate_record_key` are emitted, always with severity `warning`, in period order and the listed issue-code order. `empty_period` has `n_affected = 0`; other findings report the relevant invalid, incomplete or complete-key duplicate-excess count. Population changes, loss, entry, reappearance and repeated entity rows do not create issues.

## Count And Denominator Rules

Every PostgreSQL count is selected as exact decimal text. The operation accepts values from zero through `2^53 - 1` and hard-fails before numeric conversion for malformed, negative, non-scalar or larger values. Existing package count converters are unchanged. All PostgreSQL counts and copied denominators are base-R doubles. Period and pair positions, `periods_observed` and `gap_periods` are base-R integers. Proportions are base-R doubles and are `NA_real_` exactly when their named denominator is zero.

`n_repeated_entity_rows` counts valid rows belonging to an entity frequency greater than one; `n_repeated_entity_excess` is the number of valid rows beyond one per distinct entity. Record-key duplicate rows, groups and excess use analogous definitions among complete keys.

## Data Flow

1. Validate list shape, ordered labels, source object classes, one shared connection and declared identifiers without querying observations.
2. Validate every source and its catalogue fingerprint, then validate supported column types and compatible entity/key declarations.
3. Start one transaction and set `REPEATABLE READ READ ONLY`; revalidate every source and fingerprint inside the snapshot.
4. Query only bounded aggregate rows for per-period summaries, distinct-membership pairs and grouped observation histories. Entity and key values remain inside PostgreSQL CTEs and are never fetched.
5. Convert guarded counts, reconcile required aggregate invariants, calculate explicitly denominated proportions and assemble the complete deterministic result.
6. Commit only after complete assembly. Any failure rolls back and returns no longitudinal object. The caller-owned connection remains open and idle.

## Edge Cases

- Empty periods return zero counts, an `empty_period` warning issue and `NA_real_` proportions when they are denominators.
- Null entity values and applicable blanks are excluded from every membership calculation and produce one aggregate warning issue per affected period.
- One-period entities, repeated entity rows, entry, exit, retention loss, non-adjacent overlap, gaps and reappearance remain descriptive.
- Quoted relation and column identifiers are handled only through DBI/package quoting helpers.
- Views, materialised views, partitioned tables, foreign tables and ordinary tables remain valid when their source objects and catalogues satisfy the existing source contract.

## Errors And Privacy

Connection, source-shape, shared-connection, fingerprint, column, type-family, snapshot, SQL, count-range, conversion and incomplete-calculation failures are hard errors. No partial result is returned. Existing value-free query and transaction lifecycle messages are reused; source relation identity and observation values are not interpolated into errors, warnings or print output. The result retains period labels, declared entity column metadata and hashes but no entity identifiers, tokens or record-key values.

## Dependencies

No dependency is added. The implementation reuses DBI, RPostgres validation and existing PostgreSQL source/transaction helpers.
