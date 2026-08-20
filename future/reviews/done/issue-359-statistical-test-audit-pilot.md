# Issue 359 Statistical Test-Audit Pilot

## Audit boundary

Baseline: canonical `master` at `90f2525` on 2026-08-20.

Selected functions:

1. `summary_numeric_core()` as the shared numeric summary primitive.
2. `epi_eda_profile_summaries()` as the public specification-first data-frame summary interface.
3. `epi_eda_longitudinal_qc()` as the public PostgreSQL longitudinal population and key-QC interface.

The audit read implementation before tests, then inspected direct tests and the fixture/helper code needed to reconstruct expected results. It did not execute R, PostgreSQL, tests or coverage; modify code, tests, fixtures or snapshots; or assess unselected functions as standalone targets. The PostgreSQL branch of `epi_eda_profile_summaries()` was excluded except where its tests supplied evidence about the shared numeric primitive.

## `summary_numeric_core()`

Role: internal primitive reused by canonical EDA summaries, numeric compatibility output, QC proposals and outlier counting.

Implementation contract reviewed:

- accepts numeric non-date vectors and a non-negative scalar Tukey coefficient;
- treats `NA`, `NaN` and configured missing codes as missing;
- counts non-missing infinities as observed but excludes them from finite statistics;
- uses type-7 quartiles, sample variance and SD, `SD / sqrt(n_finite)` SEM, and `SD / mean` CV unless the mean is zero;
- requests `e1071` skewness and kurtosis when at least three varying finite values are available, and Shapiro-Wilk only for 4–4,999 varying finite values;
- uses strict Tukey fences and the finite count as the outlier-percentage denominator; coefficient zero disables outlier detection;
- returns typed unavailable values for empty, all-missing, non-varying and unsupported calculation states.

Independent checks:

For `c(1, 2, 10, Inf, 999, NA)` with missing code `999`, total `n = 6`, missing `n = 2`, observed `n = 4`, infinite `n = 1`, and finite values are `1, 2, 10`. Their sum is `13`, mean is `13/3`, median is `2`, type-7 Q1 is `1.5`, type-7 Q3 is `6`, IQR is `4.5`, and sample variance is `(1 + 4 + 100 - 13^2/3) / 2 = 73/3`. The 1.5-IQR fences are `-5.25` and `12.75`, so both directional counts and the outlier percentage are zero. These values agree with the literal assertions in `test-eda-summaries.R`.

Evidence assessment:

- Independent correctness evidence: the small literal finite/missing fixture and its count, location, dispersion and fence expectations; the explicit outlier fixture; empty and degenerate states.
- Cross-implementation or parity evidence: PostgreSQL numeric expectations and wrapper results exercise related paths, but backend agreement is not itself the oracle.
- Contract or regression evidence: output names, scalar types, invalid inputs, coefficient-zero behaviour and unavailable states.
- Coverage-only evidence: availability checks for Shapiro-Wilk do not establish its numeric result.

Strong tests cover the load-bearing counts, finite-value restriction, type-7 quartiles, sample dispersion, Tukey rules, zero mean, empty/all-missing inputs and typed unavailable states. SEM and CV tests restate their formulas using the same base summary operations, and skewness/kurtosis comparisons intentionally use `e1071`, which is the declared implementation convention.

Conclusion: **minor test improvement**. Add one small literal independent fixture for selected secondary statistics, especially SEM/CV and one shape or normality value if those fields become load-bearing. No correctness defect was identified.

## `epi_eda_profile_summaries()` data-frame path

Role: public dispatcher that validates the EDA specification, accounts for each specified variable and builds canonical typed summary components.

Implementation contract reviewed:

- accepts a data frame or PostgreSQL source and a validated EDA specification; this pilot covers the data-frame path;
- preserves specification order, reports absent required/optional variables explicitly and places each successful variable in one typed component;
- applies configured missing codes before counts and summaries;
- includes declared categorical levels with zero counts, appends sorted observed unexpected levels, and reports proportions against both total rows and observed non-missing values;
- reports incompatible or invalid typed inputs as explicit skipped rows rather than partial typed results.

Independent checks:

The numeric fixture above reconciles the audit and numeric component. For categorical values `A, B, D, NA, UNK, "NA", A`, with `UNK` configured missing and `A;B;C` declared, the seven total rows contain five observed values: `A, A, B, D, "NA"`. Counts are therefore `2, 1, 0, 1, 1`; total-row proportions use denominator `7`, observed-value proportions use denominator `5`, `C` is a declared zero-count level, and `D` and `"NA"` are unexpected. These values agree with the assertions.

For observed dates 2020-01-01, 2020-01-03, 2020-01-05 and 2020-01-07, the type-7 positions give Q1 halfway between the first two dates, median halfway between the middle dates and Q3 halfway between the last two; formatted civil dates are 2020-01-02, 2020-01-04 and 2020-01-05, matching the fixture assertions.

Evidence assessment:

- Independent correctness evidence: literal numeric, categorical, missingness and temporal expectations from small transparent fixtures.
- Cross-implementation or parity evidence: `epi_stats_summary()` equivalence and PostgreSQL/data-frame comparisons establish composition or parity, not independent truth by themselves.
- Contract or regression evidence: component names, specification order, explicit skips, output types and deterministic files.
- Coverage-only evidence: the larger blood-storage fixture's shape and non-empty checks do not establish its substantive values.

The tests credibly establish the principal data-frame summary contract, including the denominator distinction most likely to be silently wrong. They also exercise empty/all-missing numeric states, incompatible classes, factor metadata, text states and required/optional absence.

Conclusion: **minor test improvement**. A transparent case where an incompatible storage class contains only configured missing values would directly establish the deliberate all-missing coercion path. No correctness defect or important test weakness was identified for the audited data-frame path.

## `epi_eda_longitudinal_qc()`

Role: public aggregate-only PostgreSQL interface for ordered period populations, adjacent and pairwise membership, history patterns, record-key quality, typed warnings and value-free failure behaviour.

Implementation contract reviewed:

- requires at least two uniquely labelled unmodified sources sharing one caller-owned connection;
- excludes null and blank text entity identifiers from membership and uses distinct entity-period membership;
- executes all validation and aggregate queries in one repeatable-read, read-only transaction, rolls back failures and leaves the connection reusable;
- reports exact counts only after guarding base-R double precision, uses source order as time order, and returns `NA_real_` for zero-denominator proportions;
- defines retained as intersection, exited as left-only and entered as right-only; history gaps are the unobserved periods between first and last membership;
- includes only complete record keys in distinct/duplicate calculations and returns no entity or key values.

Independent checks:

The fixture's entity sets are `P1 = {1,2,3}`, `P2 = {1,2,4,5}`, `P3 = {2,4,5,6}` and `P4 = {1,2,5,6,7}`. Adjacent intersections are `2, 3, 3`, unions are `5, 5, 6`, exited counts are `1, 1, 1`, and entered counts are `2, 1, 2`. Retention proportions are therefore `2/3, 3/4, 3/4`; exit proportions are `1/3, 1/4, 1/4`; and entry proportions are `2/4, 1/4, 2/5`. All agree with the assertions.

The six pairwise overlaps are `2, 1, 2, 3, 3, 3` for pairs `(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)`, with unions `5, 6, 6, 5, 6, 6`. The fixture's seven entities have seven distinct history patterns: period 1 only; periods 1,2,4 with one gap; all four periods; periods 2,3; periods 2,3,4; periods 3,4; and period 4 only. Each asserted history group therefore has count `1`, denominator `7` and proportion `1/7`.

The period-one key pairs contain one duplicated `(key1,1)` pair among six complete rows, giving five distinct keys, one duplicate group, two duplicate rows and one excess row. Period two has two complete distinct keys and two missing keys. Period three has four complete rows, three distinct keys and one duplicate pair. Period four has five complete distinct keys. These counts agree with the assertions.

Evidence assessment:

- Independent correctness evidence: literal period, membership, overlap, direction, key and history expectations reconstructed from small index-based fixtures; explicit zero-denominator checks.
- Cross-implementation or parity evidence: none is needed for the main population oracle; reversed ordering provides a useful transformation check.
- Contract or regression evidence: exact schemas and types, source-kind support, input errors, count bounds, snapshot stability, rollback/reuse, repeatability and value-leak canaries.
- Coverage-only evidence: simple-key checks assert only broad conditions and do not independently establish every resulting count.

The end-to-end truth fixture is strong for membership directions, pairwise counts, duplicate-key semantics and privacy boundaries. However, every entity has a unique history pattern. An implementation that mishandled grouped history multiplicity, including returning one row per pattern with `n_entities = 1`, could satisfy all current end-to-end history assertions. The reconciliation guard would not expose that error on this fixture because seven singleton groups still sum to the seven-entity denominator.

Conclusion: **important test weakness**. Add a future, separately authorised fixture where at least two entities share one history pattern and independently assert the grouped `n_entities`, common denominator and proportion. No implementation defect was identified by static inspection.

## Pilot conclusion

The workflow distinguished independent mathematical/count evidence from parity, compatibility and coverage evidence and identified one consequential gap that ordinary green execution would not reveal. It should remain an explicitly triggered audit for 2–4 selected functions rather than an automatic package-wide process.

The audit stops at the three selected functions. No test run, coverage run, package modification or follow-up implementation was performed. Any strengthening of the longitudinal history fixture or secondary numeric expectations requires separately scoped and authorised work.
