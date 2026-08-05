# Software Design

Spec ID: `003-large-data-backend-strategy`
Status: Active

## Design Authority And Boundary

The reviewed episcout specification is authoritative for variable names, semantic types, roles, labels, levels, missing sentinels and optional timezone metadata. PostgreSQL catalogues are authoritative only for relation existence, column order and technical storage. Observed aggregates establish counts and compatibility evidence; they never infer scientific roles, missing codes, categorical declarations, privacy classification or approval.

This specification extends one descriptive EDA pipeline. It does not create a second database statistics family, change an estimator, prepare source values, mutate the database or establish disclosure safety. Data frames remain the compatibility baseline.

## Public API

### PostgreSQL Source

```r
epi_eda_postgres_source(con, schema, relation)
```

`con` is one open caller-owned PostgreSQL `DBIConnection` created with RPostgres. `schema` and `relation` are separate, single, non-empty, non-missing character names. SQL fragments, `DBI::SQL`, unqualified or dotted relation strings, credentials and connection parameters are not accepted. The relation must resolve to exactly one visible ordinary table, partitioned table, view, materialized view or foreign table. Temporary relations, sequences, indexes, composite types and set-returning/query expressions are rejected.

The constructor returns class `c("epi_eda_postgres_source", "list")` with fixed components `con`, `schema`, `relation`, `relation_kind`, `columns` and `source_version`. `con` remains live and caller-owned. `columns` contains catalogue metadata but no observations. `source_version` identifies the source contract, not PostgreSQL data contents. The print and structure methods show the source class, relation kind and column count but not the connection, host, port, database, user, password, environment or connection attributes.

Construction validates but neither starts a long-lived transaction nor executes analytical queries. Every public consumer revalidates the open PostgreSQL connection, exact relation identity and catalogue shape before use. Dropped, replaced or technically changed relations fail before analytical output.

### Existing Profilers

The first argument of these existing interfaces becomes `data`, an ordinary data frame, or an `epi_eda_postgres_source`:

- `epi_eda_check_schema(data, spec)`
- `epi_eda_profile_missing(data, spec)`
- `epi_eda_profile_summaries(data, spec)`
- `epi_eda_profile_plots(data, spec)`

Arguments, data-frame dispatch, returned column names/types, component order and specification order remain unchanged. PostgreSQL dispatch is explicit on the source class, not on generic DBI objects or lazy tables. Unsupported objects retain actionable errors.

`epi_eda_profile_summaries()` continues to return exactly `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped`. Variables whose reviewed role is exactly `id` or `identifier` after trim/lowercase are marked `skipped` with the existing explicit identifier-policy reason and do not enter a type-specific component. This policy is applied consistently to both backends.

`epi_eda_profile_plots()` continues to return a named list in specification order. A missing specified column retains the current error contract. An explicit identifier-role element is named but `NULL`, so list membership remains traceable without a value-bearing plot. The default primary plot remains one element per non-identifier variable: histogram for numeric/integer, frequency bar for categorical/binary, character-length distribution for text, and temporal distribution for date/datetime. The text change is a deliberate privacy and scalability correction applied to both backends; raw text labels are no longer plotted.

### Database Run

```r
epi_eda_db_run(
  source,
  spec,
  output_dir,
  overwrite = FALSE,
  plots = TRUE,
  max_plot_levels = 20L
)
```

`source` must be an `epi_eda_postgres_source`. `spec` is accepted and normalised once through `epi_eda_spec()`. `output_dir` is one non-empty local path. `overwrite`, `plots` are scalar non-missing logicals. `max_plot_levels` is a whole number from 2 through 100 and affects displayed categorical levels only, never canonical frequency summaries.

The return class is `c("epi_eda_db_run", "list")` with fixed components in this order: `status`, `output_dir`, `manifest`, `source`, `spec`, `schema`, `missing`, `summaries`, `identifier_qa`, `plots`, `plot_inventory`, `timings`, `messages`, `metadata`. A successful run has status `complete`. `source` is a value-free data frame containing schema name, relation name, relation kind, column count and supported PostgreSQL/server contract versions; it contains no connection object or connection attributes. `plots` is a named list of rendered ggplot objects when requested and an empty named list otherwise. Serialized paths are relative even though `output_dir` is normalized for immediate use.

Malformed arguments, unsafe target directories, changed source identity, invalid connections, caller-owned transactions, catalogue failures, query failures, invariant/reconciliation failures and filesystem failures are R errors. The function publishes no partial bundle and removes its owned staging directory. Per-variable semantic/type incompatibility follows the canonical `skipped` contract and does not itself make an otherwise reconciled run fail. Warnings and skip reasons are returned and written in stable order.

## Transaction And Connection Contract

Each direct PostgreSQL profiler requires an idle connection and owns one `REPEATABLE READ READ ONLY` transaction for its complete call. `epi_eda_db_run()` requires an idle connection and owns one such transaction across catalogue revalidation, schema, missingness, summaries, identifier QA and plot-data preparation. Plot rendering and staged file serialization occur after the aggregate data have been collected and the read-only transaction has ended.

The implementation does not commit, roll back, close or reconnect a caller-owned connection outside its own transaction. It rejects a pre-existing transaction rather than nesting, committing caller work or relying on unknown isolation. On every success or failure path, outstanding query results are cleared, the owned transaction ends, the connection remains open and usable, and no database write has occurred. PostgreSQL repeatable read provides one stable snapshot across successive `SELECT` statements; the package claims internal snapshot consistency, not a broader business-transaction invariant.

Client execution is sequential and follows specification order. No R worker, second connection, temporary database object, advisory lock, query cancellation worker or client-side cache is introduced. PostgreSQL remains free to choose server-side parallel plans.

## Source And Type Contract

The source relation is introspected from `pg_catalog`/`information_schema` using bound schema/relation names. Observed type names in public schema output are mapped to the existing episcout vocabulary; value-free `type_reason` explains native compatibility or the need for a reviewed view cast.

| Specification type | Supported PostgreSQL storage | Contract |
| --- | --- | --- |
| `numeric` | `smallint`, `integer`, `bigint`, `numeric`, `real`, `double precision` | Values are analysed in R-compatible double precision; finite, infinity and NaN handling below applies. Decimal precision beyond double representation is not claimed. |
| `integer` | `smallint`, `integer`, `bigint` | Native integral values are accepted. Values outside exact R double integer range or output counts above `.Machine$integer.max` block rather than silently lose precision or overflow the canonical integer columns. |
| `categorical` | `text`, `character varying`, `character`, enum, boolean and integral storage when reviewed levels cover all observed non-missing values | Native equality defines counts. Text/enum columns must use deterministic equality semantics; nondeterministic collations block with a reviewed-view recommendation. |
| `binary` | `boolean`, or the categorical-compatible types with exactly two declared levels covering every observed non-missing value | Unexpected values produce the existing explicit unexpected-level rows where storage is otherwise compatible. A missing two-level declaration for non-boolean storage is incompatible. |
| `text` | `text`, `character varying`, `character`, enum | Only counts and character-length aggregates are returned. No observed string is returned by text profiling. |
| `date` | `date` | Character or timestamp columns are not parsed implicitly; use a reviewed view cast if needed. |
| `datetime` | `timestamp with time zone` | Values are normalised to UTC. `timestamp without time zone` is incompatible in v1 because local-time/DST meaning cannot be established from storage alone; use a reviewed view cast. |

Arrays, ranges, domains whose base type is not explicitly mapped, JSON/JSONB, XML, bytea, UUID, network, geometric, interval and user-defined non-enum types are unsupported for ordinary summaries. An unsupported identifier-role column may still receive identifier QA when PostgreSQL provides equality, distinct counting and grouping, but raw values remain uncollected. Otherwise it is skipped with an explicit reason.

Empty relations, zero-column mismatches, absent optional/required variables, all-missing columns, constant columns and declared empty categorical levels retain canonical zero/`NA` semantics. Counts that cannot be represented by the current canonical R integer columns fail; this spec does not silently introduce doubles or `integer64` into released schemas.

## Missingness And Value Normalisation

Standard PostgreSQL `NULL` is missing for every type. PostgreSQL numeric `NaN` is also standard missing because the equivalent R value satisfies `is.na()`. Positive and negative infinity are observed but non-finite unless a reviewed sentinel explicitly masks them.

Specification `missing_codes` are split and trimmed by the existing specification helper. Each code is bound as a query parameter and cast through a fixed allow-listed expression for the catalogued type. Unparseable, out-of-range or ambiguous sentinels make the variable incompatible; they are never ignored or interpolated. Text sentinels use exact native equality. Date sentinels must be ISO dates. Datetime sentinels must include `Z` or an explicit offset and are compared as instants. Empty/whitespace-only sentinels remain unrepresentable under the current semicolon format and are not inferred.

The package constructs SQL only from fixed fragments plus identifiers quoted with `DBI::dbQuoteIdentifier()`. Specification values use placeholders and parameter binding. Catalogue-derived type branches are allow-listed; raw catalogue strings are not pasted as executable casts. SQL text, bound parameters and native database messages are not placed in returned objects, ordinary conditions, timing artifacts or files. Friendly conditions may name schema/relation/variable identifiers and stable stage/reason codes but do not reproduce observations, sentinels, credentials or connection attributes.

## Canonical Statistical Contract

All summaries preserve the current component schemas and definitions in `summary_cores.R`. Query helpers return small typed intermediate records; shared canonical builders perform final formatting, empty-schema construction, ordering, status and reason assembly. Backend-specific code does not define an alternate public schema.

### Variables And Missingness

One relation-wide `COUNT(*)` supplies `n`; all component counts must reconcile to it. For each specification variable, `variables$n_missing + variables$n_observed = variables$n`, `n_unique <= n_observed`, and numeric `n_finite + n_infinite = n_observed`. `epi_eda_profile_missing()` returns the current `name`, `n`, `n_missing`, `p_missing` schema in specification order. Absent columns retain `NA` missing counts and current missing-variable reasons.

### Numeric And Integer

All ordinary aggregates exclude standard/sentinel missing values. `n_observed` includes infinities; finite calculations exclude both infinities. `sum`, `min`, `max`, `mean`, `var_samp`/`stddev_samp`, standard error and coefficient of variation follow the current finite-value definitions. Quartiles use the R type-7 rule; PostgreSQL `percentile_cont` is permitted only after fixture tests establish equivalent interpolation for every defined edge size, otherwise the exact order-statistic/type-7 calculation must be expressed explicitly.

Skewness and kurtosis reproduce the current e1071 default type-3 formula from `n`, the mean and centred second/third/fourth sums. Centred moments use a second aggregate pass against the first-pass mean to reduce avoidable cancellation. They are `NA` for the same small/constant cases as the current core. This is an exact formula contract with floating-point tolerance, not an approximate PostgreSQL extension.

IQR, fences and below/above counts use finite values and the current coefficient 1.5. Fence counts are a second bounded-result aggregate query. Shapiro-Wilk remains the current R calculation only when `3 < n_finite < 5000` and variation is positive. In that case only, the complete finite vector is fetched ordered by numeric value; the hard maximum is 4,999 values. Otherwise no vector is fetched and `shapiro_p` is `NA`. No other numeric statistic may collect the input vector.

Discrete counts and categorical rows must match exactly. Floating-point statistics are compared with a combined tolerance of `1e-10 * max(1, abs(expected))` unless an independently justified tighter field-specific tolerance is recorded before implementation. `NA`, `NaN`, positive infinity and negative infinity are compared by class/sign, never converted to zero.

### Categorical And Binary

The backend fetches the complete non-missing grouped frequency table for each compatible categorical/binary variable because the canonical result exposes every declared and unexpected level. Declared levels appear first in reviewed order, including zero-count levels; unexpected values follow the same client-side deterministic ordering used by the data-frame path. `p_total` uses all relation rows and `p_observed` uses non-missing rows. Neither denominator is inferred from the number of returned groups.

This aggregate can be large or sensitive. The bundle marks categorical summaries `disclosure_review`; documentation states that aggregation is not de-identification. No automatic suppression, truncation or leading-level collapse is applied to canonical CSVs. Identifier roles never take this path.

### Text

Text output remains aggregate-only: total, missing, observed and distinct counts; empty and whitespace-only counts; and minimum/maximum Unicode character length. Whitespace follows R `trimws()` default semantics through an explicit ASCII space/tab/carriage-return/newline class, not the database locale. PostgreSQL `char_length()` must agree with R `nchar(type = "chars")` on the neutral Unicode fixture. No text values, examples, most-common strings or frequency table are fetched.

### Temporal

Date values are converted to days from the Unix epoch for type-7 quantiles and range calculations, then formatted with the current ISO date formatter. Datetime values are converted to UTC seconds from the Unix epoch, summarised and formatted as `YYYY-MM-DDTHH:MM:SSZ`; sub-second results follow the existing formatter precision and must be tested. `source_class` is the R-equivalent canonical class (`Date` or `POSIXct/POSIXt`), timezone is `NA` for date and `UTC` for datetime, and range units remain `days`/`seconds`.

The most-common temporal value is not part of the public temporal component and need not be fetched. Tied values used only internally must follow an explicit deterministic earliest-value rule. Invalid or unsupported temporal storage is skipped rather than parsed under server/session timezone settings.

## Identifier QA

Identifier roles are defined only by reviewed role exactly `id` or `identifier` after trim/lowercase. Names, uniqueness and PostgreSQL constraints do not infer the role. Each present identifier produces exactly one row with `name`, `observed_type`, `n`, `n_missing`, `n_observed`, `n_distinct`, `n_repeated_values`, `duplicate_excess`, `max_frequency`, `status`, `reason`.

`n_repeated_values` is the number of distinct non-missing identifiers with frequency greater than one. `duplicate_excess` is the sum of `frequency - 1` across those identifiers, equivalently `n_observed - n_distinct`. `max_frequency` is the largest non-missing frequency and is zero when none are observed. Counts reconcile and no identifier value or hash is returned, printed, logged, plotted or persisted. Missing identifiers receive ordinary aggregate missingness because it does not reveal values.

The direct six-component summary call records identifier policy skips but does not add a seventh component. `epi_eda_db_run()` invokes the internal QA calculation and exposes/writes the separate stable artifact. This preserves the canonical summary contract.

## Plot-Data And Rendering Contract

Plot preparation and rendering are separated internally. Data-frame and PostgreSQL preparers must return the same compact plot-data schemas; the renderer contains no backend branch and never receives a source connection.

| Plot type | Compact data | Rule |
| --- | --- | --- |
| Numeric/integer histogram | 30 rows: bin index, lower/upper boundary, count | Equal-width finite-value bins derived from canonical min/max; constants use one centred occupied bin and 29 empty bins; boundary inclusion and total reconciliation are fixed in tests. Infinities and missing values are excluded and disclosed in metadata. |
| Numeric/integer quantile box | one row: min, q1, median, q3, max, lower/upper fence, below/above counts | No raw outlier points. This is an additional bundle SVG; the existing primary plot-list element remains the histogram. |
| Categorical/binary bar | level, count, display order, remainder flag | The canonical frequency table is complete. Display keeps at most `max_plot_levels` leading levels by descending count then canonical order and combines the rest as an explicit `Other (k levels)` count. Declared zero levels remain eligible and exclusions reconcile. |
| Text length | 30 rows: length bin boundaries and count | Uses observed character lengths only; raw strings are never plot data. Empty strings have length zero and whitespace-only strings retain their actual character length. |
| Date/datetime distribution | 30 rows: temporal boundaries and count | Uses the same numeric epoch representation and missing exclusions as temporal summaries, with labels converted by the shared formatter. |
| Identifier | none | Named `NULL` in the direct plot list and a `not_created`/skipped plot-inventory row in the bundle. |

Each plot inventory row has `variable_index`, `name`, `type`, `plot_type`, `n_total`, `n_missing`, `n_plotted`, `n_excluded_non_finite`, `n_displayed_levels`, `n_collapsed_levels`, `status`, `reason`, `path`. Paths use deterministic indices such as `plots/001-histogram.svg`, never raw variable names. SVG dimensions, theme, labels, locale and deterministic order are fixed in tests. Rendered plots are visually inspected at their delivered size and their underlying compact data are reconciled before snapshot/aesthetic review.

## Run Stages And Evidence

| Stage | Input | Output | Required validations | Failure evidence |
| --- | --- | --- | --- | --- |
| S-01 Preflight | source, spec, options, target | normalised spec, revalidated catalogue, staging directory | arguments, idle PostgreSQL connection, relation identity/kind, exact prior manifest ownership | value-free R error; no published mutation |
| S-02 Snapshot/schema | S-01 | source metadata, schema table, row count | read-only repeatable-read transaction; specification/catalogue coverage; type mapping | stage/reason condition; staging removed |
| S-03 Aggregate profiling | S-02 | missing, six canonical components, identifier QA, timings | every count/schema/order invariant; bounded fetch registry; no identifiers in type tables | stage/reason condition; transaction rolled back; staging removed |
| S-04 Plot preparation | S-03 | compact plot data and inventory | plot totals reconcile to canonical counts; leading-level remainder exact | value-free stage/reason condition |
| S-05 Render/serialize | committed aggregate snapshot | SVGs and CSVs in staging | stable schemas; escaped/valid paths; regular files only; no source rows | staging removed; prior target unchanged |
| S-06 Publish | complete staging bundle | output directory and return object | exact manifest entries, sensitivity, checksums and atomic replacement/restoration | prior target restored where possible; actionable error |

Every stage records elapsed monotonic client time. Query timings contain `stage`, `variable_index`, `name`, `query_kind`, `elapsed_seconds`, `rows_returned`, `bounded_limit`, `status`; no SQL or parameter text is stored. The implementation maintains an allow-list of query kinds and maximum client rows. A runtime assertion rejects any query result that exceeds its category: scalar aggregate one row, histogram 30 rows, Shapiro 4,999 rows, categorical frequency `n_distinct` rows, and catalogue/schema metadata the exact relation column count. The categorical exception is visible rather than described as bounded.

## Owned Bundle And Filesystem Safety

The bundle contains only these registered artifacts, in deterministic order:

```text
manifest.csv
run_metadata.csv
messages.csv
spec_reviewed.csv
source_metadata.csv
schema.csv
missing.csv
summary_variables.csv
summary_numeric.csv
summary_categorical.csv
summary_text.csv
summary_temporal.csv
summary_skipped.csv
identifier_qa.csv
plot_inventory.csv
query_timings.csv
plots/<index>-<plot-type>.svg
```

All CSVs have stable zero-row schemas. The manifest columns remain `artifact`, `type`, `path`, `status`, `sensitivity`, `checksum_md5`; dynamic plot rows are ordered by variable index and plot type. Paths are portable relative paths. Created regular files except the self-referential manifest have MD5 checksums. Sensitivity is `internal_review`, `specification_review` or `disclosure_review`; canonical categorical summaries, identifier QA, plots and plot inventory require disclosure review. Checksums establish bundle integrity, not confidentiality.

The run reuses the intake workflow's sibling staging, exact-manifest ownership, checksum validation, target-to-backup/staging-to-target rename and restoration rules through tested shared internals. An existing non-empty directory is replaceable only with `overwrite = TRUE` and a valid prior database-EDA manifest whose exact registered created files, paths and checksums match disk and whose source/spec/options identity permits the new registry. Intake bundles, unowned files, changed files, missing files, symlinks, directories in file positions and special entries are refused before target mutation. No broad recursive deletion target, unresolved path, root, home or workspace directory is accepted.

No source/prepared rows, row previews, raw text, observed identifier values, pseudonymisation bridges, credentials, connection object/attributes, SQL, query parameters, query plans or environment secrets enter the bundle. The normalized caller-authored specification, variable names, source schema/relation, declared levels/sentinels and small aggregates may still be sensitive and are explicitly classified. The workflow is not anonymisation or disclosure control.

## Determinism, Provenance And Reproducibility

No randomness is used. One run records the package version, R version, DBI/RPostgres versions, PostgreSQL major/minor version, source contract version, canonical summary contract, plot-data contract, relation kind, schema/relation names, row/column/spec counts, option values, UTC start/end timestamps, final status and normalized-spec SHA-256 fingerprint. It omits host, port, database, user, password and connection attributes.

For an unchanged snapshot, spec and options, component rows, discrete counts, status/reason codes, plot inventory and filenames are deterministic. Floating-point aggregates may vary within the stated tolerance across planner/hardware reductions; timestamps, elapsed timings, server/package versions and file checksums are operational metadata, so byte-identical bundles are not promised.

## Performance Acceptance Protocol

The representative workload remains outside the public repository. Before implementation acceptance, the owner records a redacted evidence row in `review.md` and retains the detailed private run record separately. The repository record contains only an opaque workload ID, relation kind, order-of-magnitude row/column counts, number/type mix of specification variables, PostgreSQL/R/package versions, coarse CPU/RAM class, whether the snapshot was warm, three end-to-end seconds, median seconds, maximum client rows by query kind, peak R memory if measurable, status and evidence custodian/date. It contains no project, institution, schema, relation, variable, category, identifier or filesystem names.

The benchmark uses the largest approved immutable or operationally quiescent representative view, the same reviewed specification and default sequential options, with SVG rendering and bundle publication included. After one unmeasured warm-up, run three times against an unchanged source/spec on the same host; clear only the output bundle between runs through its owned overwrite path and do not restart PostgreSQL or flush system caches. M-005 passes when the median is at most 300 seconds and all runs complete/reconcile without privacy or bounded-fetch violations. Individual stage/query timings diagnose bottlenecks but cannot substitute for end-to-end time.

The data-frame limitation is evidenced separately by source size/cardinality metadata and a policy/memory assessment showing why complete client materialisation is prohibited or impractical. No restricted rows are exported merely to time the old path. If a fully synthetic workload of equivalent shape is feasible, it may additionally compare in-memory and PostgreSQL runtime, but it does not replace the representative acceptance run.

Failure of the 300-second threshold does not authorise concurrency or approximate results. The implementation must first profile fixed sequential queries and indexes/views owned by the caller. If the package itself still needs bounded multi-connection execution, stop and amend/review this spec with a common exported snapshot, connection cap, cancellation, cleanup and deterministic merge contract.

## Dependencies And Compatibility

`DBI` remains an Import. `RPostgres` remains a Suggest and is required only when constructing or using a PostgreSQL source; ordinary data-frame use and package installation do not require a server. No new package dependency, PostgreSQL extension, workflow framework or server object is required.

Implementation may extract backend-neutral canonical builders, plot-data renderers and owned-bundle helpers only behind regression tests. It must not change `epi_eda_run()` into database orchestration, add synthetic database generation, or route database sources through `epi_eda_prepare()`. Data-frame inputs and reference-capable subclasses remain unchanged after every call; database source relations and metadata remain unchanged after audit, success and failure.

## Authoritative Technical References

- R's `stats::quantile()` type-7 definition is the canonical quantile authority: <https://www.stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html>.
- PostgreSQL ordered-set/statistical aggregate behaviour is documented at <https://www.postgresql.org/docs/current/functions-aggregate.html>; fixture evidence must still prove parity rather than assuming similarly named functions are identical.
- PostgreSQL repeatable-read snapshot semantics are documented at <https://www.postgresql.org/docs/17/transaction-iso.html>.
- DBI identifier quoting, parameter binding and result lifecycle follow the DBI specification at <https://dbi.r-dbi.org/articles/spec.html>.

## Stop Conditions

Stop for owner review before implementation continues if repository or fixture evidence requires changing a canonical output column/type, collecting more than 4,999 non-categorical values, returning raw text/identifier values, accepting arbitrary SQL, supporting timestamp-without-time-zone implicitly, writing to PostgreSQL, adding approximate/sampled results, suppressing cells automatically, adding a generic backend, using multiple connections, adding a dependency, importing representative workload material or weakening a valid data-frame regression.
