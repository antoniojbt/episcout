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

## Work lifecycle and continuity

GitHub issues and the current roadmap issue are the authoritative live task state. `future/TODOs.md`, specification manifests, `future/changelog.md` and `PROJECT_MAP.md` are synchronised repository records; when they disagree with GitHub, stop new work and reconcile them first.

Use this lifecycle for tracked work: `candidate` (issue only), `draft` (specification unresolved), `active` (accepted contract and implementation in progress), `review` (pull request open), `merged/pending-closeout` (derived transient state) and `completed` (default branch verified and all closeout conditions satisfied). Commit only `draft`, `active`, `review` or `completed` in versioned manifests. Record blocked or deferred scheduling in the tracking issue and roadmap rather than inventing additional manifest states.

- Run `scripts/check-workflow-state.sh` at the start of tracked work, before pull-request handoff and during post-merge closeout. Use `--offline` only when GitHub access is genuinely unavailable and report that limitation.
- Keep at most one implementation specification in `active` or `review`. Draft design research may coexist, but it must not overtake the active lane, rewrite shared status files independently or merge ahead without explicit owner approval.
- Give every non-trivial change one tracking issue. Use a numbered specification when required by the core approach, then use one scoped branch and one pull request for the accepted slice.
- Use `Closes #<issue>` only when the pull request completes the issue's stated outcome. Before closing a planning-only issue, create and link its next implementation issue or record an explicit terminal reason.
- Treat pull-request creation as `review`, not completion. At handoff, state the tracking issue, specification, checks, compatibility impact, unresolved limits, successor disposition and that merge/closeout remains pending.
- After merge, verify the canonical `upstream/master` commit and required checks; close or update the tracking issue and roadmap; finalize acceptance/review evidence; set the manifest to `completed`; move the spec to `future/specs/done/`; update `future/TODOs.md` and `future/changelog.md`; update `PROJECT_MAP.md` only when architecture or durable pointers changed; and create or confirm the successor tracker.
- Use a focused closeout pull request when default-branch files require merge-derived evidence. A closeout-only pull request is terminal maintenance and does not recursively require another documentation pull request.
- Do not begin the successor task until prior closeout passes the workflow-state check. If a merge occurs after a session ends, the next session must perform closeout before any new planning or implementation.
- After closeout merges, synchronize local and fork `master` with canonical `upstream/master`. Remove only clean, fully merged worktrees and branches, and delete remote branches only when the owner has authorized that cleanup.

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

It should also identify the tracking issue, specification when present, current lifecycle state, successor or terminal disposition and post-merge closeout owner. A PR that only plans later implementation must not close the implementation tracker.

Before requesting review, run `scripts/check-local.sh`. Run `scripts/check-cran.sh` for release-oriented changes or when CRAN behaviour is in scope.

## GitHub Actions

The repository workflows run R package checks, linting and Codecov coverage. There is no package website deployment workflow.

When changing a workflow, inspect its actual job name before invoking `act`. The package-check job is currently:

```bash
act -j R-CMD-check
```
