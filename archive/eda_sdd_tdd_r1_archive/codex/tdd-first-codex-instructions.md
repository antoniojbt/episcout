# TDD-first Codex instruction blocks

Use these instruction blocks one at a time.

## Standing instruction

Repeat this often:

```text
Keep the change small and reviewable. Do not perform broad refactoring. If you find additional issues, document them rather than fixing them in this PR.
```

## Standing closeout rule

Every instruction must end by updating
`archive/eda_sdd_tdd_r1_archive/START_HERE.md`.

At closeout:

```text
1. Mark the completed PR as Done.
2. Mark the next PR as Active.
3. Update the Active PR section.
4. Update must-edit and must-not-edit lists.
5. Record whether failing tests are expected.
6. Leave SDD and ADR files unchanged unless the design changed.
```

If a PR intentionally does not update `START_HERE.md`, record that exception in
the PR description.

## Instruction 1: Add external fixture files

```text
Read:
- AGENTS.MD
- archive/eda_sdd_tdd_r1_archive/START_HERE.md
- archive/eda_sdd_tdd_r1_archive/sdd/0001-spec-first-eda.md
- archive/eda_sdd_tdd_r1_archive/sdd/data-dictionary-spec.md
- archive/eda_sdd_tdd_r1_archive/sdd/tdd-external-fixtures.md
- archive/eda_sdd_tdd_r1_archive/codex/revised-pr-plan-tdd-first.md
- archive/eda_sdd_tdd_r1_archive/codex/review-checklist.md

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

Closeout:
Update START_HERE.md to mark PR 1 Done and PR 2 Active.
```

## Instruction 2: Add failing tests for spec, schema and missingness

```text
Task:
Add fixture-backed tests before implementing the new functions.

Add:
- tests/testthat/test-epi_eda_spec-fixtures.R
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
5. Do not implement epi_eda_spec(), epi_eda_check_schema() or epi_eda_profile_missing() in this PR.

Closeout:
Update START_HERE.md to mark PR 2 Done and PR 3 Active. Record that PR 2 may
leave tests failing until PR 3 implements the missing functions.
```

## Instruction 3: Implement spec, schema and missingness

```text
Task:
Implement only the functions required to pass the fixture-backed tests from the previous PR.

Add:
- R/epi_eda_spec.R
- R/eda_schema.R
- R/eda_missing.R

Functions:
- epi_eda_spec()
- epi_eda_validate_spec()
- epi_eda_check_schema()
- epi_eda_profile_missing()

Requirements:
1. Keep the API small.
2. Validate inputs clearly.
3. Return machine-readable tibbles or data frames.
4. Add roxygen2 documentation.
5. Do not implement synthetic data, summaries, plots, reports or epi_eda_run() in this PR.

Closeout:
Update START_HERE.md to mark PR 3 Done and PR 4 Active. Tests should no longer
fail because epi_eda_spec(), epi_eda_validate_spec(), epi_eda_check_schema() or
epi_eda_profile_missing() are missing.
```

## Instruction 4: Add failing synthetic-data tests

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
6. Do not implement epi_eda_generate_synthetic_data() in this PR.

Closeout:
Update START_HERE.md to mark PR 4 Done and PR 5 Active. Record that PR 4 may
leave tests failing until PR 5 implements epi_eda_generate_synthetic_data().
```

## Instruction 5: Implement synthetic-data generation

```text
Task:
Implement epi_eda_generate_synthetic_data() only.

Add:
- R/eda_synthetic.R

Requirements:
1. Use the validated EDA specification.
2. Support numeric, integer, categorical, binary, date, datetime and text variables.
3. Use deterministic output with a fixed seed.
4. Do not add heavy dependencies.
5. Do not implement summaries, plots, reports or epi_eda_run() in this PR.

Closeout:
Update START_HERE.md to mark PR 5 Done and PR 6 Active.
```

## Instruction 6: Add failing summary and plot tests

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
3. Do not implement epi_eda_profile_summaries() or epi_eda_profile_plots() in this PR.

Closeout:
Update START_HERE.md to mark PR 6 Done and PR 7 Active. Record that PR 6 may
leave tests failing until PR 7 implements epi_eda_profile_summaries() and
epi_eda_profile_plots().
```

## Instruction 7: Implement summaries and plots

```text
Task:
Implement summary profiling and plot dispatch.

Add:
- R/eda_summaries.R
- R/eda_plots.R

Functions:
- epi_eda_profile_summaries()
- epi_eda_profile_plots()

Requirements:
1. Use the EDA specification to decide variable handling.
2. Use existing epi_stats_* helpers where suitable.
3. Return machine-readable outputs.
4. Return plot objects without printing them.
5. Do not implement epi_eda_run() or reporting in this PR.

Closeout:
Update START_HERE.md to mark PR 7 Done and PR 8 Active.
```

## Instruction 8: Add failing epi_eda_run tests

```text
Task:
Add fixture-backed end-to-end tests for epi_eda_run().

Add:
- tests/testthat/test-epi_eda_run-fixtures.R

Requirements:
1. Test real fixture data workflow.
2. Test synthetic data workflow.
3. Use temporary directories for output-file tests.
4. Check expected named components: metadata, schema, missing, summaries, plots.
5. Do not implement epi_eda_run() in this PR.

Closeout:
Update START_HERE.md to mark PR 8 Done and PR 9 Active. Record that PR 8 may
leave tests failing until PR 9 implements epi_eda_run().
```

## Instruction 9: Implement epi_eda_run()

```text
Task:
Implement epi_eda_run().

Add:
- R/epi_eda_run.R

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

Closeout:
Update START_HERE.md to mark PR 9 Done and PR 10 Active.
```

## Instruction 10: Report template and project template

```text
Task:
Continue with the revised PR plan.

Next steps:
1. Add failing report-rendering tests.
2. Implement epi_eda_render_report().
3. Add failing project-template and epi_eda_create_project() contract tests.
4. Implement project template and epi_eda_create_project().
5. Add large-data design note.

Follow archive/eda_sdd_tdd_r1_archive/codex/revised-pr-plan-tdd-first.md.

Closeout:
Update START_HERE.md after each remaining PR so it stays the live status
dashboard.
```

## Instruction 12: Add project-template contract tests

```text
Task:
Add failing tests for the project template and epi_eda_create_project() contract.

Add:
- tests/testthat/test-project-template.R

Requirements:
1. Tests define the bundled inst/project-template/ scaffold.
2. Tests define epi_eda_create_project(path, overwrite = FALSE).
3. Tests require reports/eda.qmd to culminate in epi_eda_render_report().
4. Do not add inst/project-template/ in this PR.
5. Do not implement epi_eda_create_project() in this PR.

Closeout:
Update START_HERE.md to mark PR 12 Done and PR 13 Active. Record that PR 12
may leave tests failing until PR 13 implements the scaffold and helper.
```

## Instruction 13: Implement project template

```text
Task:
Implement the project template and epi_eda_create_project().

Add:
- inst/project-template/
- R/epi_eda_create_project.R

Requirements:
1. Include metadata/data_dictionary.csv, config/eda.yml, _targets.R,
   reports/eda.qmd, R/project-derivations.R and outputs/.
2. epi_eda_create_project(path, overwrite = FALSE) creates the destination
   project and copies the scaffold.
3. Refuse to overwrite existing files unless overwrite = TRUE.
4. Return the normalized project path invisibly.
5. Keep the report path connected to epi_eda_render_report().

Closeout:
Update START_HERE.md to mark PR 13 Done and PR 14 Active.
```
