# Software Design

Spec ID: `010-canonical-eda-summary-contract`  
Status: Draft  

## Scope

Replace the unreleased EDA v1/v2 branching with one canonical typed summary path. Remove the legacy two-table adapters and version-suffixed internal names, route EDA workflows, CSV output, HTML reporting and typed `epi_stats_summary()` through the canonical builder, and update active documentation and tests to describe only observed behaviour.

## Required Behaviour And Basis

The EDA data specification is the authoritative declaration of variable names, analytical types, missing sentinels and categorical levels. Observed data determine counts and values. Existing code, fixtures and outputs establish prior behaviour only; expected analytical results must be justified independently through explicit definitions and hand calculations. The repository truth, analysis and software-verification checklists govern implementation and review.

Before implementation acceptance, confirm calculation conventions against the official R documentation for `quantile()`, `var()`, `sd()` and `shapiro.test()` and the installed `e1071` documentation for skewness and kurtosis. Record the selected definitions and parameters in `review.md`; agreement with existing outputs alone is not validation.

## Public API

- `epi_eda_profile_summaries(data, spec)` retains its exported name but removes `summary_version` and always returns the canonical typed result.
- `epi_eda_run(data, spec, output_dir = NULL, synthetic = FALSE, n = 100, seed = NULL)` removes `summary_version` and always includes the canonical result in `summaries`.
- `epi_eda_render_report(data, spec, output_dir, synthetic = FALSE, n = 100, seed = NULL, quiet = TRUE)` removes `summary_version` and always renders the canonical result.
- `epi_stats_summary(..., output = "typed")` retains its current public argument and output name but calls the same internal canonical builder after constructing an inferred specification and applying global sentinel codes.
- `epi_stats_numeric()` retains its exported name and output schema, but its all-missing, sentinel-only, infinite-only and zero-row `sum` becomes `NA` under the canonical no-observed-total policy. This is an intentional correctness change rather than compatibility behaviour.
- No public legacy option, deprecation adapter or alternate compact calculation path is introduced.

## Canonical Output

`epi_eda_profile_summaries()` returns a named list in this fixed order:

1. `variables`: one row for every specification variable, including `name`, `label`, `type`, `role`, `required`, total and missingness counts where observable, `status` and `reason`.
2. `numeric`: one row for each successfully summarised numeric or integer variable, with finite-value descriptive statistics, distribution fields and outlier fields.
3. `categorical`: one row per declared or unexpected observed level for successfully summarised categorical and binary variables, including counts, both denominators and declaration status.
4. `text`: one row for each successfully summarised text variable, including missingness, cardinality, length, empty-string and whitespace-only counts.
5. `temporal`: one row for each successfully summarised date or datetime variable, including counts, source class, timezone where applicable, ISO-formatted quantiles and range units.
6. `skipped`: one row for each specification variable that cannot be summarised, including its declared type, observed class where available and an actionable reason.

Every component is a data frame with a stable typed zero-row schema. Each specification variable appears exactly once in `variables` and is either represented in its type component or in `skipped`, never both and never neither.

## Statistical And Semantic Policies

### Missingness And Counts

- `NA`, `NaN` and per-variable specification `missing_codes` are missing for EDA summaries; typed `epi_stats_summary()` applies its `codes` argument globally.
- Blank and whitespace-only text remain observed unless the specification explicitly lists them as missing sentinels.
- `n` is the source vector length, `n_missing` counts standard and sentinel missing values, and `n_observed` is `n - n_missing`.
- A variable absent from the data has unavailable counts represented by typed `NA`, not the dataset row count. Its `variables` row records `status = "skipped"` and states whether the absent variable was marked required.
- When the optional specification `required` field is absent, the canonical `variables$required` value is typed `NA`; typed `epi_stats_summary()` therefore does not invent a required/optional designation.
- `n_unique` counts distinct observed values after missing exclusions. Numeric infinity counts remain observable and separate from finite analytical values.

### Numeric And Integer Variables

- Positive and negative infinity are counted in `n_infinite` and excluded from finite-value statistics.
- `n_finite` is the denominator for finite analytical summaries and outlier percentages.
- When no finite values exist, `sum`, location, spread, shape, normality and fence values are `NA`; an all-missing or infinite-only variable must not imply an observed total of zero.
- When finite values exist, `sum`, quantiles, mean, median, minimum and maximum use only those values. Sample variance and standard deviation require at least two finite values; shape and normality fields retain their documented sample-size and variation requirements.
- Tukey fences use the documented quantile method and coefficient. Outlier counts report the numerator and `outlier_percentage` uses `n_finite` as its denominator.

### Categorical And Binary Variables

- Specification `levels` are the sole source of declared levels. Unused factor levels not declared by the specification are neither observed nor declared and do not appear solely because they remain in factor metadata.
- Declared levels remain in output with zero counts. Unexpected observed values are appended deterministically and marked `is_unexpected = TRUE`.
- Literal strings such as `"NA"` remain distinct from missing values unless explicitly configured as sentinels.
- `p_total` divides a level count by the complete source vector length and `p_observed` divides by the number of observed non-missing values. A zero denominator produces `NA`, not zero.

### Text Variables

- Text summaries accept character and factor data when compatible with the declared type.
- Empty strings and non-empty whitespace-only strings are counted separately and remain part of length and cardinality calculations unless configured as missing.
- All-missing and zero-row inputs preserve typed unavailable minima and maxima rather than inventing values.

### Date And Datetime Variables

- Dates accept `Date`, `IDate` and strict ISO `YYYY-MM-DD` character input; datetimes accept `POSIXct`, `POSIXlt` and supported strict ISO-8601 character input.
- Invalid non-missing temporal values make that variable skipped with the parse failure stated; they are not silently converted to missing.
- Date ranges use days. Datetime values are presented in ISO UTC form with source timezone metadata and ranges use seconds.

### Incompatible And Missing Variables

- An incompatible observed class or invalid temporal value is a per-variable skip so the EDA audit can report all variables in one run.
- Invalid top-level inputs, malformed specifications and impossible output locations remain function-level errors.
- Missing variables are excluded from plot dispatch while remaining visible in schema and summary status output.

## Internal Design

- Replace `profile_summaries_v2()` with an unversioned internal builder such as `build_typed_summaries()`.
- Remove `profile_summaries_numeric()`, `profile_summaries_categorical()`, `empty_eda_*_v1()` and other code used only by the legacy two-table path.
- Rename version-suffixed row and empty-table helpers to semantic unversioned names.
- Retain the shared numeric, categorical, text and temporal cores where their behaviour satisfies this specification; correct the cores only where the canonical semantic policies require it.
- Keep `epi_stats_summary(output = "current")` outside the EDA return-shape migration and protect its class/action-specific schemas and unrelated values with regression tests; the authorised no-observed-total correction may flow through its numeric mode.

## Data Flow

1. Validate `data` and normalise `spec` through `epi_eda_spec()`.
2. Iterate over every specification row in specification order.
3. Record a variable audit row and dispatch compatible observed data to the appropriate shared core.
4. Append the type result or an explicit skipped result, then bind each component to a stable typed schema.
5. Return the canonical six-component list.
6. Have `epi_eda_run()` use that list directly, filter plot dispatch to present variables, and write one CSV per component without recalculation.
7. Have the report template present the returned components with explicit coverage, denominator and empty-component context, without choosing a summary version or deriving alternate statistics.

## Database Dictionary Integration

`epi_eda_dictionary_spec()` continues to convert the extended multi-table dictionary into the common EDA specification. The canonical summary does not version the dictionary and does not alter inventory, catalogue or privacy semantics. An integration test must prove that a reviewed dictionary specification flows directly into `epi_eda_profile_summaries()` and that declared levels, missing codes, ordering and variable coverage survive the conversion.

## Presentation And Export

- CSV files expose the unmodified canonical component values and stable machine-readable column names.
- The HTML report derives every displayed value from the returned canonical object and may format labels or numeric display precision but must not recalculate statistics or manually transcribe values.
- The report presents the variable coverage table and skipped reasons, labels finite-value exclusions and categorical denominators, states temporal range units and distinguishes a genuinely empty component from an omitted section.
- A zero-row component is represented by a clear statement that no variables of that type were summarised rather than disappearing without explanation.

## Implementation Order

1. Record focused and full baseline results before package-code changes.
2. Add failing behaviour tests with independently calculated expectations for the canonical contract and the identified semantic corrections.
3. Refactor the summary builder and shared-core behaviour required by those tests.
4. Remove version arguments from workflow and report functions and update CSV and plot dispatch.
5. Update report, README, vignette, NEWS and roxygen source, then regenerate documentation.
6. Run focused tests, `scripts/check-local.sh` and `scripts/check-cran.sh` and inspect the returned tables, CSV files, rendered report and source tarball.

## Recovery

Keep implementation in one focused refactor branch and commit only after the canonical contract passes targeted verification. If a semantic policy proves unresolved, stop before changing production behaviour, record the unresolved question in `review.md` and revert only the uncommitted spec-010 implementation files while preserving unrelated work.

## Dependencies

No new dependency is planned. Existing base R and declared package dependencies are sufficient.
