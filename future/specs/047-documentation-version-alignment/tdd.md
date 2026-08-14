# Test Design

Spec ID: `047-documentation-version-alignment`
Status: Active

## Executable Tests

- Extend `tests/testthat/test-project-template.R` to locate the installed starter dictionary, load it through `epi_eda_spec()` and assert the exact current 16-field names plus the example row's separate storage and analysis declarations.
- Run existing EDA specification and schema tests to retain required-field validation and rejection of the removed `type` field.
- Do not add wording-only tests for prose that is better covered by complete-source review and rendered inspection.

## Baseline And Focused Command

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'project-template|eda-spec-scaffold|eda_spec|eda_schema', reporter = 'summary')"
```

## Render And Walkthrough Verification

- Build every vignette from current source, inspect all five rendered outputs for errors, missing content and stale interface claims, and verify the installed package lists all intended vignettes.
- Install the built package and run the complete database-to-report walkthrough against a disposable PostgreSQL service with automatic cleanup enabled. Inspect its completed delivery status, manifest and entry points without retaining its transient output.
- Search all maintained user-facing sources and generated help for obsolete `15-field`, removed specification `type`, and stale published-version claims; historical completed release records remain unchanged where they accurately describe their own version.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'project-template|eda-spec-scaffold|eda_spec|eda_schema', reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::build_vignettes()"
scripts/check-local.sh
scripts/check-cran.sh
scripts/check-workflow-state.sh
```

Hosted macOS, Ubuntu, PostgreSQL integration, coverage, CodeFactor and Codecov checks must pass or receive evidence-based disposition before merge.
