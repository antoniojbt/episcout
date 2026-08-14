# AGENTS.md

This file defines conventions for contributing to `episcout`, an R package for cleaning, exploring and visualising epidemiological data. Apply these instructions to code, tests, documentation, package checks and repository operations.

## Core approach

- Prefer the smallest coherent change that satisfies a demonstrated requirement. Do not add dependencies, abstractions, services, databases or orchestration for hypothetical future use.
- Inspect the relevant functions, callers, tests, documentation and configuration before editing.
- Identify the cause of a defect rather than repairing only its visible symptom.
- Never conceal changed behaviour inside a cleanup or style refactor.
- Preserve unrelated user work and review material changes before handoff.
- Ask only when unresolved ambiguity could materially change behaviour, scientific meaning, safety, compatibility or architecture. Otherwise choose the simplest reversible option and state consequential assumptions.
- Do not hard-wrap prose. Keep each prose paragraph and standalone prose comment on one line while preserving intentional line breaks in lists, tables, code examples and other structured content.

## Instruction and checklist routing

Use the checklists in `checklists/` as routed below. They support self-review and do not make a review independent.

- Apply `checklists/software-verification.md` to every code, script, pipeline or package-interface change.
- Apply `checklists/truth-and-semantics.md` when code or documentation interprets data, missingness, domain rules, statistical meaning or consequential requirements.
- Apply `checklists/analysis-and-statistics.md` to analytical functions, metrics, tables, models, derived data and analytical conclusions.
- Apply `checklists/figures.md` to plots, charts and report graphics.
- Apply `checklists/copy-edit.md` to README, NEWS, vignettes, roxygen documentation, captions and other user-facing prose.
- Apply `checklists/render-and-release.md` to rendered vignettes, HTML reports, package tarballs and release artifacts.
- Use a written plan for multi-component changes, migrations, uncertain architecture or operations that may fail partway. State the observable outcome, scope, affected interfaces, implementation order, validation and recovery.
- Consult authoritative primary or official sources when implementing statistical or epidemiological methods or making scientific claims. Distinguish sourced facts from inference and state unresolved uncertainty.

For ordinary work, report the applicable checks and material exceptions at handoff. Save a separate review record only for high-stakes, unusually consequential or explicitly audited work.

## Project structure

| Path | Purpose |
| --- | --- |
| `R/` | R package source and roxygen documentation |
| `man/` | Generated Rd documentation; do not edit directly |
| `tests/testthat/` | Unit, integration, fixture and snapshot tests |
| `tests/figs/` | Figure comparison artifacts |
| `vignettes/` | Authored tutorials and worked examples |
| `inst/` | Files installed with the package, including project and report templates |
| `data-raw/` | Development sources and scripts for package or test data |
| `scripts/` | Repository development and verification entry points |
| `.github/workflows/` | GitHub Actions checks and coverage workflows |
| `future/` | Planning, specifications, references and scratch material; not current package behaviour unless implemented elsewhere |
| `archive/` and `legacy/` | Historical material; do not treat as current implementation or interfaces |

Before adding or changing a helper command, inspect `scripts/`, `.github/workflows/`, `README.md` and this file. Do not add a script when an existing entry point already performs the task. Use names that describe the project action and are not confusable with system commands.

## R environment and canonical commands

Use the repository wrapper for R commands:

```bash
scripts/rscript_env_caller.R -e "R.home(); .libPaths()"
```

The wrapper uses the project mamba environment by default. Set `EPISCOUT_RSCRIPT` to use another verified R binary; do not use bare `Rscript` without confirming its environment.

Set a real CRAN mirror in direct `devtools` commands rather than leaving `repos` as `@CRAN@`.

Run the repository entry points instead of duplicating their command sequences:

```bash
# Documentation, lint, tests and local package check
scripts/check-local.sh

# Source build and R CMD check --as-cran
scripts/check-cran.sh
```

Run focused tests first when practical, for example:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda_schema', reporter = 'summary')"
```

Use coverage to identify untested changed behaviour, not as a substitute for meaningful assertions:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); covr::report()"
```

## Coding conventions

- Use `snake_case` for new objects and functions. Preserve released argument names and other public interfaces unless a breaking change is authorised.
- Use 2-space indentation and no tabs.
- Add roxygen2 documentation for every exported function.
- Use `styler::style_file()` on changed R files and package-loaded `lintr::lint_package()` for linting. Reserve `styler::style_pkg()` for an explicitly scoped style-only change.
- Retain `%>%` to preserve the current R compatibility floor. Line-length linting remains disabled because prose is not hard-wrapped.
- Avoid `<<-`, `assign()` and hidden interactive state unless strictly necessary.
- Keep paths and changing parameters explicit, keep side effects at boundaries and validate inputs before processing.
- Prefer base R or dependencies already declared in `DESCRIPTION`. Add every required dependency to the appropriate `Imports` or `Suggests` field and explain why the existing implementation is insufficient.
- Keep outputs deterministic where practical. Control randomness explicitly and test filesystem writes in temporary directories.

Determine interface maturity from releases, known external consumers, documentation, tests, repository callers and Git history. Preserve released or externally consumed contracts unless a breaking change is authorised. For experimental or unconsumed code, prefer a clean current interface and update its callers, tests and documentation together.

## Scientific and data correctness

Passing tests and successful execution do not establish that analytical behaviour is correct. Treat existing code, tests, fixtures, snapshots, schemas, documentation and previous outputs as evidence of prior behaviour rather than independent authority.

- Establish intended behaviour from an authoritative source, explicit domain rule or independently justified expectation before preserving or changing consequential logic.
- Make filters, exclusions, joins, recodes, grouping units, aggregation rules, units and denominators explicit.
- Validate required columns, input types and ranges, key uniqueness and non-empty inputs where they affect results.
- Avoid silent consequential coercion. Make conversions and their failure behaviour explicit.
- Preserve source missingness by default. Never silently convert blanks, `NA`, unknown, censored, suppressed or not-applicable values into `No`, `False`, zero or another observed value.
- Define missingness propagation for recodes, composites and grouped summaries. In particular, do not allow all-missing inputs to become zero solely because `na.rm = TRUE` was used.
- Make important row counts, duplicates, unmatched keys, dropped records and missing-to-observed or observed-to-missing transitions inspectable.
- Report numerators and denominators when percentages or proportions could otherwise be ambiguous.
- Set seeds when randomness affects results. Synthetic data are for workflow preparation and testing unless a documented requirement establishes another use.
- Keep confidential or restricted data out of source control, fixtures, examples, logs and public outputs. Use neutral reusable examples rather than personal names, institutions, local paths or project-specific identifiers, except where package metadata legitimately requires maintainer information.

## Testing

- Test user-visible behaviour, validation failures, important boundaries and observed regressions.
- Define important expected values independently of the production path under test. Never regenerate a fixture with the same implementation and present agreement as validation.
- Do not mirror implementation details in tests. Test intended behaviour and stop when that behaviour is unclear, undocumented or potentially incorrect.
- Use small fixtures with explicit provenance and independently justified expectations.
- Do not weaken a valid requirement or regression test merely to make an implementation pass. Report the discrepancy before changing the requirement.
- Run targeted checks first and then the broader relevant suite.
- Add tests for new exported functions and for material behavioural changes. Investigate unexplained coverage regressions rather than enforcing an unmeasured blanket percentage target.
- Keep compatibility guards, duplicated checks and generated fixtures only when they protect a demonstrated contract, data integrity, safety or an observed regression.

## Documentation and generated files

- Edit roxygen comments in `R/` and regenerate `man/` with `devtools::document()`; do not edit generated Rd files directly.
- Keep README, NEWS, vignettes, examples, schemas and commands consistent with observed package behaviour.
- Write documentation for users and maintainers, not as a narrative of agent activity or incidental machine-specific details.
- Match the established British English spelling and package terminology.
- Inspect rendered output when changing vignettes, HTML reports, plot snapshots or other user-facing generated artifacts.
- Do not hand-edit `NAMESPACE` when roxygen owns the relevant declaration.

## Reviews

- Treat code, implementation and pull-request review as read-only unless the user also requests changes.
- Inspect changed material with relevant callers, configuration, tests and documentation.
- Prioritise correctness, data integrity, reproducibility, privacy, security, compatibility and tests for changed behaviour.
- List actionable findings first, ordered by severity, with a location, impact and the smallest practical fix.
- Distinguish confirmed defects from risks, questions and optional suggestions. Do not block on personal style preferences or request a broad rewrite when a local fix is adequate.
- If there are no findings, say so and identify residual risks or checks that were not run.
- State whether the review was independent of the implementation.

## CodeFactor follow-up

- A CodeFactor failure is non-blocking by default. Inspect the changed code and the reported finding when available; do not infer that a finding is minor merely from the check status.
- Treat a CodeFactor finding as blocking when there is concrete evidence that it affects correctness, data integrity, security, privacy, performance, concurrency, resource lifetime, public-interface compatibility or a consequential documentation claim. Fix it in the current slice or create a separate tracked issue before proceeding.
- For style, naming, formatting, duplication or local maintainability findings without such impact, report the finding and address it only when the current change benefits or a focused maintenance task is warranted.

## Tracking and optional specifications

GitHub issues and pull requests are the live work record. Use `future/` for design notes, optional specifications and retained completion evidence; it does not govern package behaviour or prevent unrelated work when a record is stale.

- Use a numbered specification when consequential semantics, several dependent components, migration risk or cross-session hand-off make a written contract useful. Small and well-defined changes may proceed directly from an issue or user request.
- When a task uses a specification, keep its manifest and supporting records accurate for that task and run `scripts/check-workflow-state.sh` as a focused consistency check. The script is not a prerequisite for work that does not use this workflow.
- Use one coherent branch and pull request for a reviewable change. Link the relevant issue or specification when one exists; do not create a successor issue, roadmap entry, lifecycle state or closeout pull request solely to satisfy process.
- After merge, update only records whose current claims became inaccurate. Historical completed specifications remain retained evidence and need not be rewritten to match later process changes.

In prose, plans and status reports, prefix identifiers with their artefact type so references are unambiguous: for example, `issue-278`, `PR-280`, `spec-034`, `commit-cc05cb0` and `release-0.3.0`. Avoid bare numeric or hash-style references such as `278`, `#278` or `034` except where GitHub syntax requires them, such as `Closes #278` in a pull-request description.

## Git and GitHub

- The current origin is a public GitHub repository; recheck its visibility when publication or disclosure decisions depend on it. Treat ignored data, outputs, secrets and agent state as private by default.
- Do not force-add ignored files without explicit authorisation and a disclosure review.
- Inspect `git status` and relevant diffs before editing, staging, committing and handing off. Preserve unrelated changes.
- Before committing, inspect `git diff --staged` and use a focused commit with a short imperative subject.
- Run relevant checks before committing or opening a pull request.
- Do not publish credentials, personal or restricted data, or exploitable details in commits, issues or pull requests.

## Pull requests

Use branch names `feature/<desc>`, `bugfix/<desc>` or `refactor/<desc>` as appropriate. Format PR titles as `[Type] Summary of change`.

A PR description should state:

1. What changed and why.
2. Tests and checks run.
3. Backward-compatibility impact.
4. Material coverage change, if measured.
5. Unresolved limitations or failures, explicitly stating when none remain.

Link the tracking issue or specification when one exists. Use `Closes #<issue>` only when the pull request delivers the issue's complete outcome.

Before requesting review, run `scripts/check-local.sh`. Run `scripts/check-cran.sh` for release-oriented changes or when CRAN behaviour is in scope.

## GitHub Actions

The repository workflows run R package checks, linting and Codecov coverage. There is no package website deployment workflow.

When changing a workflow, inspect its actual job name before invoking `act`. The package-check job is currently:

```bash
act -j R-CMD-check
```
