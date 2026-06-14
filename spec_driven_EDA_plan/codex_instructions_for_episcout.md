# Codex instructions for episcout

Use this file to instruct Codex one step at a time.

The repository now contains the new SDD documentation folder at the repository root. Codex should read those documents and work through the plan in small pull requests.

## General rule to repeat often

```text
Keep the change small and reviewable. Do not perform broad refactoring. If you find additional issues, document them rather than fixing them in this PR.
```

## Instruction 1: Initial repository review

Use this first.

```text
Read AGENTS.MD and the new documentation folder I added at the repository root.

Start with:
- docs/sdd/0001-spec-first-eda.md
- docs/sdd/mvp-scope.md
- docs/sdd/data-dictionary-spec.md
- docs/roadmap.md
- docs/codex/initial-instructions.md
- docs/codex/pr-plan.md
- docs/codex/review-checklist.md

Do not change R code yet.

Task:
1. Review the new documentation for consistency with the current repository.
2. Check whether any paths, function names or assumptions are inconsistent with the current package.
3. Propose a documentation-only PR if needed.
4. Do not modify package behaviour.
5. Do not remove or rename existing exported functions.
6. Summarise any issues before making changes.
```

## Instruction 2: Documentation-only PR

Use this if Codex asks what to do first, or after it has completed the initial review.

```text
Create PR 1 from docs/codex/pr-plan.md.

Scope:
- documentation only
- no R code changes
- no DESCRIPTION changes unless strictly needed
- no generated man/ or NAMESPACE changes

Goal:
Commit the SDD, ADRs, roadmap, Codex instructions and review checklist as the controlling specification for future development.

Do not modify package behaviour.
```

## Instruction 3: Baseline checks PR

Use this after PR 1 is merged.

```text
Proceed to PR 2 from docs/codex/pr-plan.md.

Task:
Run the package checks and produce a baseline audit.

Do:
1. Run tests.
2. Run R CMD check or devtools::check(manual = FALSE).
3. Identify failing tests, warnings, notes and fragile functions.
4. Create docs/baseline-checks.md with the results.
5. Do not make functional changes unless required to document the baseline.
6. Do not refactor unrelated code.
7. Do not remove or rename existing exported functions.

Deliverable:
A documentation-only or mostly-documentation PR that records the current package baseline.
```

## Instruction 4: Harden current helper functions

Use this after the baseline is documented.

```text
Proceed to PR 3 from docs/codex/pr-plan.md.

Task:
Harden existing core statistics and missingness helpers before adding the new EDA layer.

Targets:
- R/epi_stats_numeric.R
- R/epi_stats_na_perc.R
- R/epi_stats_summary.R
- R/epi_stats_chars.R
- R/epi_stats_factors.R

Focus:
- all-missing inputs
- zero-length inputs
- mixed-type data frames
- coefficient of variation when mean is zero
- clearer errors
- stable tibble/data.frame outputs
- preserve existing public function names and broad behaviour

Add or update tests.

Do not add the new spec-first API in this PR.
Do not perform broad refactoring.
If you find additional issues, document them rather than fixing them in this PR.
```

## Instruction 5: Add the EDA specification parser

Use this after current helper functions are stable.

```text
Proceed to PR 4 from docs/codex/pr-plan.md.

Task:
Implement the EDA specification parser.

Add:
- eda_spec()
- validate_eda_spec()

New file:
- R/eda_spec.R

Tests:
- tests/testthat/test-eda_spec.R

Requirements:
1. Read a CSV data dictionary.
2. Validate required columns.
3. Validate allowed variable types.
4. Check for duplicate or missing variable names.
5. Return a stable tibble-like object.
6. Provide clear error messages.
7. Add roxygen2 documentation.
8. Do not modify unrelated functions.
9. Preserve existing epi_* functions.
```

## Instruction 6: Add synthetic-data generation

Use this after the specification parser is merged.

```text
Proceed to PR 5 from docs/codex/pr-plan.md.

Task:
Implement synthetic data generation from the EDA specification.

Add:
- generate_synthetic_data()

New file:
- R/eda_synthetic.R

Tests:
- tests/testthat/test-eda_synthetic.R

Requirements:
1. Use the validated EDA specification.
2. Support numeric, integer, categorical, binary, date, datetime and text variables.
3. Support deterministic output with a fixed seed.
4. Use plausible ranges and levels where provided.
5. Clearly mark outputs as synthetic where practical.
6. Synthetic data are for pipeline preparation only, not inference.
7. Add tests for deterministic output and unsupported variable types.
8. Do not add heavy dependencies unless justified.
```

## Instruction 7: Add schema and missingness profiles

Use this after synthetic data generation is merged.

```text
Proceed to PR 6 from docs/codex/pr-plan.md.

Task:
Add schema checking and missingness profiling.

Add:
- check_schema()
- profile_missing()

New files:
- R/eda_schema.R
- R/eda_missing.R

Tests:
- tests/testthat/test-eda_schema.R
- tests/testthat/test-eda_missing.R

Requirements:
1. Compare expected variables with observed variables.
2. Flag missing variables.
3. Flag unexpected variables.
4. Summarise missing count and percentage per variable.
5. Include non-missing counts and distinct non-missing counts where useful.
6. Return machine-readable tibbles or data frames.
7. Do not generate plots in this PR.
8. Do not implement run_eda() in this PR.
```

## Instruction 8: Add summaries and plot dispatch

Use this after schema and missingness profiling are merged.

```text
Proceed to PR 7 from docs/codex/pr-plan.md.

Task:
Add summary profiling and automatic plot dispatch.

Add:
- profile_summaries()
- profile_plots()

New files:
- R/eda_summaries.R
- R/eda_plots.R

Tests:
- tests/testthat/test-eda_summaries.R
- tests/testthat/test-eda_plots.R

Requirements:
1. Summarise variables according to the specification type.
2. Use existing epi_stats_* helpers where suitable.
3. Produce one basic plot per variable where appropriate.
4. Numeric variables should get histograms.
5. Categorical and binary variables should get bar plots.
6. Date variables should get date-frequency plots where practical.
7. Return plot objects and/or save paths in a machine-readable object.
8. Keep plotting dependencies controlled.
9. Do not implement report rendering in this PR.
```

## Instruction 9: Add run_eda orchestration

Use this after summary and plot dispatch are merged.

```text
Proceed to PR 8 from docs/codex/pr-plan.md.

Task:
Implement run_eda() as the main orchestration function.

Add:
- run_eda()

New file:
- R/run_eda.R

Tests:
- tests/testthat/test-run_eda.R

Requirements:
1. Accept data and spec.
2. Validate the spec.
3. Run schema checks.
4. Run missingness profiling.
5. Run summary profiling.
6. Run plot profiling.
7. Save machine-readable outputs where an output directory is provided.
8. Return a named list of outputs.
9. Support mode = "real" and mode = "synthetic".
10. Clearly label synthetic-mode outputs.
11. Do not render Quarto reports in this PR.
```

## Instruction 10: Add Quarto report template

Use this after run_eda() is merged.

```text
Proceed to PR 9 from docs/codex/pr-plan.md.

Task:
Add a Quarto report template and report rendering function.

Add:
- render_eda_report()

New files:
- R/eda_report.R
- inst/report-template/eda.qmd

Tests:
- tests/testthat/test-eda_report.R

Requirements:
1. Report consumes saved outputs from run_eda().
2. Report should not recompute expensive summaries.
3. HTML output first.
4. Report sections should include schema, missingness, summaries and plots.
5. Synthetic reports must be clearly labelled as synthetic.
6. Keep Quarto as an optional dependency if practical.
```

## Instruction 11: Add project template

Use this after report rendering is merged.

```text
Proceed to PR 10 from docs/codex/pr-plan.md.

Task:
Add a reusable project template.

Add files under:
- inst/project-template/

Template contents:
- metadata/data_dictionary.csv
- config/eda.yml
- _targets.R
- reports/eda.qmd
- R/project-derivations.R
- outputs/

Optional helper:
- use_episcout_project()

Requirements:
1. Template supports both synthetic-data and real-data workflows.
2. Template uses the same EDA specification for both workflows.
3. Template should be minimal and easy to modify.
4. Do not add complex project-specific analysis.
```

## Instruction 12: Large-data design note

Use this only after the MVP workflow is functional.

```text
Proceed to PR 11 from docs/codex/pr-plan.md.

Task:
Document the large-data backend strategy.

Do not fully implement Arrow, DuckDB or data.table backends yet unless explicitly requested.

Deliverables:
1. Add or update documentation explaining:
   - data.frame/tibble first;
   - data.table backend second;
   - DuckDB backend later;
   - Arrow backend later;
   - sampled data for plotting;
   - full-data summaries;
   - cached outputs.
2. Identify functions where backend abstraction will be needed.
3. Do not break the MVP API.
```

## Review checklist for every Codex PR

Before merging, check:

```text
Does this PR do only one thing?
Did it change unrelated files?
Did it remove or rename existing epi_* functions?
Did it add tests for new behaviour?
Did it update roxygen docs for exported functions?
Did it add heavy dependencies unnecessarily?
Does it preserve backwards compatibility?
Are synthetic-data outputs clearly labelled where relevant?
Are outputs machine-readable where relevant?
```

## Recommended sequence

```text
1. Initial repository review
2. Documentation-only PR
3. Baseline checks PR
4. Harden current helper functions
5. Add eda_spec()
6. Add synthetic data generation
7. Add schema and missingness profiling
8. Add summaries and plot dispatch
9. Add run_eda()
10. Add report template
11. Add project template
12. Document large-data strategy
```

## Notes for manual oversight

Do not ask Codex to implement the whole plan at once.

The safest approach is one PR per instruction block. If Codex finds issues outside the current PR scope, ask it to document them and stop.
