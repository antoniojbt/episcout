# Codex PR instruction: add `penguins_raw` as an external EDA truth fixture

## Intended use

Pass this file directly to Codex as the PR instruction.

This PR should add a second external fixture for standard EDA validation, using `palmerpenguins::penguins_raw`. The aim is not to replace the existing `medicaldata::blood_storage` fixture. The aim is to complement it with a widely known public EDA dataset that has stable external documentation, common examples, mixed variable types, raw column names, and missingness.

## Repository context

Repository:

```text
antoniojbt/episcout
```

Target branch:

```text
episcout2
```

Existing design principle:

```text
external fixture and expected output first
failing fixture-backed tests second
minimal implementation third
```

Keep that sequence intact.

The existing `blood_storage` fixture is appropriate for early biomedical EDA tests. It should remain the primary biomedical fixture for schema, missingness, and clinical-data realism. The new `penguins_raw` fixture should be added as a second external truth fixture for broader standard EDA behaviour, especially summaries, missingness, categorical handling, dates, labels, plots, and report examples.

## Why this PR is needed

The current `blood_storage` fixture is useful because it is real biomedical data, small, public, and already has committed expected schema and missingness outputs. It is less ideal as the only standard EDA truth fixture because it is not as widely used for canonical EDA examples as `palmerpenguins`.

`palmerpenguins::penguins_raw` is useful because it provides:

- a well-known public dataset;
- external package documentation and examples;
- 344 rows and 17 variables;
- raw-style column names, including spaces and punctuation;
- mixed variable types;
- dates;
- character/categorical variables;
- numeric morphometric variables;
- real missingness;
- common scatterplot, histogram, boxplot, grouped-summary and faceting examples.

Use source documentation from:

```text
https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
https://allisonhorst.github.io/palmerpenguins/articles/examples.html
```

## PR goal

Create a small, reviewable PR that adds `penguins_raw` as a pinned external fixture and adds anti-circularity guardrails so future agent-generated implementation code cannot simply validate itself.

Do not implement new EDA functions in this PR.

## Scope

### Must create

```text
data-raw/test-fixtures/make_penguins_raw_fixture.R
tests/testthat/fixtures/penguins_raw/SOURCE.md
tests/testthat/fixtures/penguins_raw/penguins_raw.csv
tests/testthat/fixtures/penguins_raw/penguins_raw_spec.csv
tests/testthat/fixtures/penguins_raw/expected_missing.csv
tests/testthat/fixtures/penguins_raw/expected_summary_numeric.csv
tests/testthat/fixtures/penguins_raw/expected_summary_categorical.csv
tests/testthat/fixtures/penguins_raw/expected_plot_inventory.csv
```

### Should update

```text
spec_driven_EDA_plan/docs/sdd/tdd-external-fixtures.md
spec_driven_EDA_plan/docs/codex/review-checklist.md
tests/testthat/fixtures/README.md
```

### May create

```text
tests/testthat/test-fixture-generation-guardrails.R
```

Only create this test if it is lightweight and does not run fixture-regeneration scripts.

### Must not edit

Do not edit implementation files for this PR:

```text
R/epi_eda_spec.R
R/eda_schema.R
R/eda_missing.R
R/eda_summaries.R
R/eda_plots.R
R/epi_eda_run.R
```

Do not add or modify package behaviour.

Do not add `palmerpenguins` to `Imports`.

Do not make routine tests require internet access.

Do not make routine tests require `palmerpenguins` to be installed.

## Dependency policy

`palmerpenguins` may be used by the manual regeneration script only.

The committed fixture CSV files must be used by routine tests.

The regeneration script must fail clearly if `palmerpenguins` is not installed:

```r
if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
  stop(
    "Package 'palmerpenguins' is required to regenerate penguins_raw fixtures. ",
    "Install it with install.packages('palmerpenguins').",
    call. = FALSE
  )
}
```

## Fixture-generation requirements

The regeneration script must:

1. Load `palmerpenguins::penguins_raw`.
2. Write `penguins_raw.csv` to `tests/testthat/fixtures/penguins_raw/`.
3. Write `SOURCE.md` with provenance.
4. Write a manually reviewed `penguins_raw_spec.csv`.
5. Write expected outputs using only base R or simple transparent calculations.
6. Avoid all `episcout` calls.
7. Avoid all new EDA function calls.
8. Avoid internet access.
9. Be manually run only; do not call it from routine tests.

The script must not call any of these:

```text
library(episcout)
episcout::
epi_eda_spec(
epi_eda_validate_spec(
epi_eda_check_schema(
epi_eda_profile_missing(
epi_eda_profile_summaries(
epi_eda_profile_plots(
epi_eda_run(
epi_eda_generate_synthetic_data(
```

## Expected outputs

### `expected_missing.csv`

Compute independently with base R:

```r
expected_missing <- data.frame(
  name = names(penguins_raw),
  n = nrow(penguins_raw),
  n_missing = vapply(penguins_raw, function(x) sum(is.na(x)), integer(1)),
  p_missing = vapply(penguins_raw, function(x) mean(is.na(x)), numeric(1)),
  stringsAsFactors = FALSE
)
```

### `expected_summary_numeric.csv`

For numeric and integer variables, include at least:

```text
name
class
n
n_missing
mean
sd
median
q25
q75
min
max
```

Use base R functions only:

```r
numeric_cols <- names(penguins_raw)[vapply(penguins_raw, is.numeric, logical(1))]

summarise_numeric <- function(x, name) {
  non_missing <- x[!is.na(x)]
  data.frame(
    name = name,
    class = class(x)[1],
    n = length(x),
    n_missing = sum(is.na(x)),
    mean = mean(non_missing),
    sd = stats::sd(non_missing),
    median = stats::median(non_missing),
    q25 = unname(stats::quantile(non_missing, 0.25, names = FALSE)),
    q75 = unname(stats::quantile(non_missing, 0.75, names = FALSE)),
    min = min(non_missing),
    max = max(non_missing),
    stringsAsFactors = FALSE
  )
}

expected_summary_numeric <- do.call(
  rbind,
  Map(function(nm) summarise_numeric(penguins_raw[[nm]], nm), numeric_cols)
)
```

### `expected_summary_categorical.csv`

For character, factor, logical and non-numeric categorical variables, include at least:

```text
name
level
n
p
n_missing
```

Use base R only. Missing values should be counted separately or documented clearly.

### `expected_plot_inventory.csv`

This is not a visual snapshot. It is a plain expected contract for later plot-dispatch tests.

Include at least:

```text
name
expected_plot_type
reason
```

Suggested values:

- numeric/integer continuous variables: `histogram` or `density`;
- categorical variables: `bar`;
- date variables: `time` or `skip` with a reason;
- free-text/comment variables: `skip` with a reason;
- unsupported variables: `skip` with a reason.

Do not test visual appearance in this PR.

## `penguins_raw_spec.csv` guidance

Create a manually reviewed fixture data dictionary. Use the existing spec format already used by `blood_storage_spec.csv` where practical:

```text
name
label
type
role
units
levels
min
max
missing_codes
required
group
description
```

Use source variable names exactly as they appear in `penguins_raw`. Do not silently clean column names in the fixture.

Recommended type mapping:

- character identifiers and labels: `categorical` or `text`, depending intended EDA handling;
- sex: `categorical`;
- island/species/region/study/stage: `categorical`;
- clutch completion: `binary` or `categorical`, but document the chosen convention;
- dates: `date`;
- body size measurements: `numeric`;
- egg count: `integer`;
- comments: `text`.

The spec should be manually reviewed. Do not infer it entirely from the future implementation.

## `SOURCE.md` minimum contents

Include:

```markdown
# penguins_raw fixture provenance

## Source

- Dataset: `penguins_raw`
- Source package: `palmerpenguins`
- Source package version used for fixture generation: <version>
- Observations: 344
- Variables: 17
- Dataset type: ecological observational dataset
- Purpose: standard EDA truth fixture for specification-first EDA tests

## Source documentation

- https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
- https://allisonhorst.github.io/palmerpenguins/articles/examples.html

## Fixture files

- `penguins_raw.csv`: pinned data exported from `palmerpenguins::penguins_raw`.
- `penguins_raw_spec.csv`: manually reviewed fixture data dictionary.
- `expected_missing.csv`: independently computed expected missingness result.
- `expected_summary_numeric.csv`: independently computed numeric summaries.
- `expected_summary_categorical.csv`: independently computed categorical summaries.
- `expected_plot_inventory.csv`: independently defined plot-dispatch expectation.

## Regeneration

Run from the repository root:

```sh
Rscript data-raw/test-fixtures/make_penguins_raw_fixture.R
```

The script computes expected outputs with base R and does not call `episcout` EDA functions.
```

## Documentation updates

Update `spec_driven_EDA_plan/docs/sdd/tdd-external-fixtures.md` to state:

- `blood_storage` remains the first biomedical fixture.
- `scurvy` remains useful for tiny small-n edge-case tests.
- `penguins_raw` is added as the second standard EDA truth fixture.
- `penguins_raw` is intended for summary, missingness, plot-dispatch and report examples.
- `penguins_raw` should not displace `blood_storage` for biomedical tests.
- `penguins_raw` should not be used to justify implementation by matching implementation-generated output.

Update `spec_driven_EDA_plan/docs/codex/review-checklist.md` to add explicit anti-circularity checks:

```markdown
## External truth fixtures

- [ ] Expected outputs were committed before implementation code that consumes them.
- [ ] Expected outputs were computed without calling `episcout` functions under test.
- [ ] Regeneration scripts do not call `episcout::`, `library(episcout)`, or new EDA functions.
- [ ] Fixture `SOURCE.md` records source package, version, row count, column count and source documentation.
- [ ] Routine tests use committed fixture files, not live downloads.
- [ ] Plot tests check contracts first, not visual appearance, unless a later visual-regression test is explicitly introduced.
```

Update `tests/testthat/fixtures/README.md` to include `penguins_raw` and explain the intended distinction:

```text
blood_storage: biomedical fixture for clinical schema, missingness and workflow tests.
penguins_raw: standard public EDA fixture for mixed raw columns, common summaries, missingness and plot-dispatch contracts.
```

## Optional guardrail test

If adding `tests/testthat/test-fixture-generation-guardrails.R`, keep it simple and deterministic. It should scan regeneration scripts and fail if they call package code under test.

Example:

```r
test_that("fixture regeneration scripts do not call episcout EDA functions", {
  scripts <- c(
    "data-raw/test-fixtures/make_external_fixtures.R",
    "data-raw/test-fixtures/make_penguins_raw_fixture.R"
  )
  scripts <- scripts[file.exists(scripts)]

  text <- unlist(lapply(scripts, readLines, warn = FALSE), use.names = FALSE)

  forbidden <- c(
    "library\\(episcout\\)",
    "episcout::",
    "epi_eda_spec\\(",
    "epi_eda_validate_spec\\(",
    "epi_eda_check_schema\\(",
    "epi_eda_profile_missing\\(",
    "epi_eda_profile_summaries\\(",
    "epi_eda_profile_plots\\(",
    "epi_eda_run\\(",
    "epi_eda_generate_synthetic_data\\("
  )

  for (pattern in forbidden) {
    expect_false(
      any(grepl(pattern, text)),
      info = paste("Forbidden fixture-generation dependency found:", pattern)
    )
  }
})
```

Do not run regeneration scripts in this test.

## Acceptance criteria

The PR is acceptable if:

1. `penguins_raw` fixture files are committed.
2. Fixture provenance is documented.
3. Expected missingness, numeric summaries, categorical summaries and plot inventory are committed.
4. Expected outputs are generated without `episcout`.
5. Routine tests do not require internet or `palmerpenguins`.
6. No EDA implementation files are changed.
7. Documentation clearly explains why `blood_storage` and `penguins_raw` both exist.
8. Review checklist includes anti-circularity checks.
9. `R CMD check` is not made worse by this PR.

## PR title

```text
Add penguins_raw external EDA truth fixture
```

## PR description template

```markdown
## Summary

Adds `palmerpenguins::penguins_raw` as a pinned external EDA truth fixture for specification-first EDA tests.

This complements the existing `medicaldata::blood_storage` biomedical fixture. `blood_storage` remains the primary clinical fixture; `penguins_raw` is added for standard public EDA behaviour, including mixed raw variable names, missingness, numeric summaries, categorical summaries and plot-dispatch contracts.

## What changed

- Added a manual regeneration script for `penguins_raw` fixtures.
- Added pinned `penguins_raw` CSV fixture.
- Added manually reviewed `penguins_raw_spec.csv`.
- Added independently computed expected missingness, numeric summaries and categorical summaries.
- Added a plot-dispatch inventory for later plot tests.
- Updated fixture documentation and review guardrails.

## What did not change

- No new EDA implementation code.
- No changes to public package behaviour.
- No runtime dependency on `palmerpenguins`.
- No internet access required by routine tests.

## Anti-circularity

Expected outputs were computed with base R or manually reviewed contracts. The regeneration script does not call `episcout` EDA functions or implementation code under test.
```
