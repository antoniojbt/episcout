# TDD-first Codex instruction blocks

Use these instruction blocks one at a time.

## Standing instruction

Repeat this often:

```text
Keep the change small and reviewable. Do not perform broad refactoring. If you find additional issues, document them rather than fixing them in this PR.
```

## Instruction 1: Documentation update for TDD-first order

```text
Read:
- AGENTS.md
- docs/sdd/0001-spec-first-eda.md
- docs/sdd/mvp-scope.md
- docs/sdd/data-dictionary-spec.md
- docs/codex/pr-plan.md
- docs/codex/review-checklist.md

Task:
Add the TDD-first documentation update.

Add:
- docs/sdd/tdd-external-fixtures.md
- docs/codex/revised-pr-plan-tdd-first.md
- docs/codex/tdd-first-codex-instructions.md
- docs/codex/update-existing-docs.md
- docs/adr/0005-external-fixture-tdd.md

Do not change R code.
Do not change DESCRIPTION.
Do not change NAMESPACE.
Do not change generated man/ files.
```

## Instruction 2: Update older docs to reference the revised order

```text
Read docs/codex/update-existing-docs.md.

Task:
Update the older documentation files so they point to the TDD-first plan.

Files to update:
- docs/codex/pr-plan.md
- docs/roadmap.md
- docs/codex/initial-instructions.md
- docs/sdd/mvp-scope.md
- README.md if it lists the old order

Do not change R code.
Do not alter the substance of the SDD except to reference the TDD-first order.
```

## Instruction 3: Add external fixture files

```text
Task:
Add external real-data test fixtures for specification-first EDA.

Create:
- data-raw/test-fixtures/make_external_fixtures.R
- tests/testthat/fixtures/README.md
- tests/testthat/fixtures/blood_storage/SOURCE.md
- tests/testthat/fixtures/blood_storage/blood_storage.csv
- tests/testthat/fixtures/blood_storage/blood_storage_spec.csv
- tests/testthat/fixtures/blood_storage/expected_schema.csv
- tests/testthat/fixtures/blood_storage/expected_missing.csv

Use medicaldata::blood_storage as the source.

Requirements:
1. Do not use new episcout EDA functions to compute expected outputs.
2. Compute expected values using simple independent base R code.
3. Include source metadata and provenance.
4. Keep fixtures small.
5. Do not add medicaldata to Imports.
6. If medicaldata is needed, use it only in the regeneration script or Suggests.
7. Add no new package behaviour in this PR.
```

## Instruction 4: Add failing tests for spec, schema and missingness

```text
Task:
Add fixture-backed tests before implementing the new functions.

Add:
- tests/testthat/test-eda_spec-fixtures.R
- tests/testthat/test-eda_schema-fixtures.R
- tests/testthat/test-eda_missing-fixtures.R

Use:
- tests/testthat/fixtures/blood_storage/blood_storage.csv
- tests/testthat/fixtures/blood_storage/blood_storage_spec.csv
- tests/testthat/fixtures/blood_storage/expected_schema.csv
- tests/testthat/fixtures/blood_storage/expected_missing.csv

Requirements:
1. Tests must be deterministic.
2. Tests must not require internet.
3. Tests must not depend on private data.
4. Tests must compare against independently computed expected outputs.
5. Do not implement eda_spec(), check_schema() or profile_missing() in this PR.
```

## Instruction 5: Implement spec, schema and missingness

```text
Task:
Implement only the functions required to pass the fixture-backed tests from the previous PR.

Add:
- R/eda_spec.R
- R/eda_schema.R
- R/eda_missing.R

Functions:
- eda_spec()
- validate_eda_spec()
- check_schema()
- profile_missing()

Requirements:
1. Keep the API small.
2. Validate inputs clearly.
3. Return machine-readable tibbles or data frames.
4. Add roxygen2 documentation.
5. Do not implement synthetic data, summaries, plots, reports or run_eda() in this PR.
```

## Instruction 6: Add failing synthetic-data tests

```text
Task:
Add fixture-backed tests for synthetic data generation.

Add:
- tests/testthat/test-eda_synthetic-fixtures.R

Use:
- tests/testthat/fixtures/blood_storage/blood_storage_spec.csv

Requirements:
1. Synthetic data has the same variable names as the spec.
2. Synthetic data has the requested row count.
3. Synthetic values respect levels where provided.
4. Synthetic values respect min/max where provided.
5. Fixed seed gives identical output.
6. Do not implement generate_synthetic_data() in this PR.
```

## Instruction 7: Implement synthetic-data generation

```text
Task:
Implement generate_synthetic_data() only.

Add:
- R/eda_synthetic.R

Requirements:
1. Use the validated EDA specification.
2. Support numeric, integer, categorical, binary, date, datetime and text variables.
3. Use deterministic output with a fixed seed.
4. Do not add heavy dependencies.
5. Do not implement summaries, plots, reports or run_eda() in this PR.
```

## Instruction 8: Add failing summary and plot tests

```text
Task:
Add fixture-backed tests for summaries and plot dispatch.

Add:
- tests/testthat/test-eda_summaries-fixtures.R
- tests/testthat/test-eda_plots-fixtures.R

Optional expected outputs:
- tests/testthat/fixtures/blood_storage/expected_summary_numeric.csv
- tests/testthat/fixtures/blood_storage/expected_summary_categorical.csv

Requirements:
1. Expected summary outputs must be independently computed.
2. Plot tests should check object classes and names, not visual appearance.
3. Do not implement profile_summaries() or profile_plots() in this PR.
```

## Instruction 9: Implement summaries and plots

```text
Task:
Implement summary profiling and plot dispatch.

Add:
- R/eda_summaries.R
- R/eda_plots.R

Functions:
- profile_summaries()
- profile_plots()

Requirements:
1. Use the EDA specification to decide variable handling.
2. Use existing epi_stats_* helpers where suitable.
3. Return machine-readable outputs.
4. Return plot objects without printing them.
5. Do not implement run_eda() or reporting in this PR.
```

## Instruction 10: Add failing run_eda tests

```text
Task:
Add fixture-backed end-to-end tests for run_eda().

Add:
- tests/testthat/test-run_eda-fixtures.R

Requirements:
1. Test real fixture data workflow.
2. Test synthetic data workflow.
3. Use temporary directories for output-file tests.
4. Check expected named components: metadata, schema, missing, summaries, plots.
5. Do not implement run_eda() in this PR.
```

## Instruction 11: Implement run_eda()

```text
Task:
Implement run_eda().

Add:
- R/run_eda.R

Requirements:
1. Accept data and spec.
2. Validate the spec.
3. Run schema checks.
4. Run missingness profiling.
5. Run summary profiling.
6. Run plot profiling.
7. Save machine-readable outputs where an output directory is provided.
8. Return a named list.
9. Support mode = "real" and mode = "synthetic".
10. Clearly label synthetic-mode outputs.
11. Do not render reports in this PR.
```

## Instruction 12: Report template and project template

```text
Task:
Continue with the revised PR plan.

Next steps:
1. Add failing report-rendering tests.
2. Implement render_eda_report().
3. Add project template.
4. Add large-data design note.

Follow docs/codex/revised-pr-plan-tdd-first.md.
```
