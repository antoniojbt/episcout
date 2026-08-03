# Test Design

Spec ID: `013-specification-guided-data-preparation`
Status: Completed

## Test Files

- `tests/testthat/test-eda-prepare.R`
- relevant existing schema, specification, summary and workflow regression tests

## Independent Basis

Expected prepared values, classes, counts, audit rows and statuses must be hand-authored from this approved SDD. Production preparation, schema or summary output must not generate the expected fixtures used to validate itself. Existing canonical helpers may be tested for reconciliation only after independent expectations are established.

## Baseline

Spec 012's final `scripts/check-local.sh` passed on the same starting commit with zero errors, warnings or notes and the two known environment skips; its generated side effects were restored. The main implementation agent must run and record a focused current baseline before changing package code on this branch.

## Public And Mode Tests

- [ ] Public formals and safe defaults are exact.
- [ ] The return has fixed component order and `epi_eda_preparation`/list classes.
- [ ] Audit mode returns the input object unchanged, never creates candidate data, and always has `schema_after = NULL`.
- [ ] Successful apply returns a distinct complete prepared object and compatible `schema_after`.
- [ ] Blocked apply returns original data, no `schema_after`, all blockers, and no partial conversion.
- [ ] Invalid top-level arguments error without observed values.
- [ ] Apply blocks an untouched scaffold whose evidence rows are not all `reviewed`; audit remains available, and specifications without scaffold evidence remain supported.

## Audit And Metadata Tests

- [ ] Audit columns, types, stage/action/status values and row ordering are stable for populated and zero-row results.
- [ ] Every specification variable has presence and applicable transformation stages.
- [ ] Dataset rows use reserved `.dataset.` names and real colliding names are rejected.
- [ ] Standard, sentinel, invalid, unexpected, affected and changed counts match hand-derived row sets and respect subset relationships.
- [ ] Metadata dimensions and action-status counts reconcile exactly with the audit.
- [ ] Audit reasons contain names, classes, counts and policy only, never observed, sentinel, unexpected or invalid values.

## Missingness Tests

- [ ] Numeric, integer, categorical, binary, text, Date and datetime sentinels become correctly typed missing values on successful apply.
- [ ] Standard `NA`/`NaN` remain missing and are not double-counted as sentinels.
- [ ] Literal `"NA"`, empty strings, whitespace, infinities and sentinel-looking values remain observed unless exactly declared missing.
- [ ] Declared sentinel levels remain as zero-count factor metadata after their observations become missing.
- [ ] Preparation missing counts reconcile with canonical missingness on prepared data.

## Numeric And Integer Tests

- [ ] Base integer-to-numeric conversion preserves finite values exactly and uses double `NA`.
- [ ] Base numeric storage retains infinities when they are not sentinels.
- [ ] Character/factor-to-numeric and character/factor-to-integer are blocking without printing strings, including all-sentinel character vectors.
- [ ] Double-to-integer accepts finite whole values at both representable boundaries.
- [ ] Fractional, infinite and out-of-range double values are counted as invalid and block without rounding, clipping or missing conversion.
- [ ] A blocker in a later column proves an earlier valid conversion is not returned partially.

## Categorical And Binary Tests

- [ ] Declared factor order and zero-count levels are retained.
- [ ] Character, factor, integer and numeric values match declared levels by exact character representation.
- [ ] Factor-to-factor conversion uses labels rather than internal codes.
- [ ] Missing/empty categorical levels and binary declarations other than exactly two levels block.
- [ ] Logical binary input without levels uses `FALSE;TRUE`; incompatible alternative labels block.
- [ ] Unexpected-level error mode counts and blocks without exposing values.
- [ ] Append mode appends unique unexpected levels in deterministic order, retains observations, emits a divergence warning and leaves the specification unchanged.
- [ ] Binary unexpected values remain blocking under append mode rather than creating a factor with more than two levels.
- [ ] Literal `"NA"` remains an observed level unless declared missing.
- [ ] Semicolon or boundary-whitespace-unsafe declared/appended metadata is blocked without disclosure.

## Text Tests

- [ ] Character storage remains character and applies text sentinels exactly.
- [ ] Factor-to-text preserves displayed labels.
- [ ] Empty and whitespace-only values remain observed unless explicitly declared missing.
- [ ] The semicolon-delimited v1 limitation for empty, whitespace-only and semicolon-containing sentinels is documented and no private escaping rule is inferred.
- [ ] Numeric, date and nested storage declared text block rather than silently stringify.

## Temporal Tests

- [ ] Date/IDate and POSIXct/POSIXlt storage retain supported class and timezone semantics.
- [ ] Strict valid character dates convert to Date; invalid dates count and block without becoming missing.
- [ ] Z and explicit-offset datetime strings normalise to the independently calculated UTC instants.
- [ ] Local datetime strings block without `timezone` and convert using a valid reviewed Olson timezone.
- [ ] Invalid timezone names block without falling back to the machine timezone.
- [ ] Mixed local and offset/Z strings require timezone for local members and return UTC.
- [ ] Invalid, nonexistent and ambiguous daylight-saving local times block without revealing timestamps.
- [ ] A timezone on a non-datetime row produces a warning and has no conversion effect.
- [ ] Optional `min`/`max` fields do not clamp, recode or block otherwise valid observations.

## Presence, Extra And Dataset Tests

- [ ] Missing `required = TRUE` variables block.
- [ ] Missing `FALSE`, absent and `NA` requiredness variables are skipped as optional/unasserted.
- [ ] Extra keep retains columns in original relative order; error blocks; drop removes only explicitly extra columns.
- [ ] Unsupported extra columns warn/retain under keep, block under error and can be explicitly dropped.
- [ ] Present specification columns follow specification order in successful output.
- [ ] Duplicate rows are counted as rows after the first occurrence and never removed.
- [ ] Zero rows, zero columns and all-missing supported columns have stable audit and prepared classes.
- [ ] Duplicate, empty and reserved-prefix column names fail before ambiguous planning.
- [ ] Nested/unsupported specified columns block apply and are fully audited.

## Non-mutation And Privacy Tests

- [ ] Data, specification, factors and data-frame subclasses remain identical after audit and blocked apply.
- [ ] Successful apply does not mutate a data.table or other data-frame subclass by reference.
- [ ] Successful apply returns an ordinary data frame with source row names preserved.
- [ ] No mode writes files, creates directories, emits values in messages or depends on global timezone/locale options.
- [ ] Sensitive-looking observed, sentinel, unexpected and invalid fixture values are absent from the complete result except the deliberately returned original/prepared `data` component.

## Integration Tests

- [ ] `schema_before` is identical to direct `epi_eda_check_schema()` output.
- [ ] Successful `schema_after` reports compatible prepared specification variables.
- [ ] Prepared output passed to `epi_eda_run()` retains dimensions and reconciles standard/sentinel missing totals with the audit.
- [ ] Data from a reviewed `epi_eda_spec_scaffold()` CSV flows through audit, apply and canonical EDA.
- [ ] Existing scaffold, schema, dictionary and canonical-summary tests remain unchanged and pass.

## Documentation And Inspection

- [ ] Roxygen, README, vignette, NEWS, generated Rd and NAMESPACE agree with observed formals, statuses and policies.
- [ ] The worked example demonstrates received data, reviewed specification, audit diagnosis, correction, apply and canonical EDA.
- [ ] Documentation distinguishes missing sentinels from unexpected values, observed evidence from validation declarations, audit from mutation, and pseudonymisation from anonymisation.
- [ ] A realistic returned audit, prepared data, before/after schemas and canonical result are inspected directly.

## Focused Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-prepare|eda_schema|eda_spec|eda-summaries|run_eda', reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); lintr::lint_package()"
```

## Acceptance Commands

```bash
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
```

Record exact commands, versions, warnings, skips, generated-file effects and direct artifact inspection in `review.md`.
