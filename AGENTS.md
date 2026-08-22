# Episcout Repository Instructions

Episcout is a public R package for cleaning, exploring, and visualising epidemiological data. Apply the repository-specific rules below.

## End goal and instruction hierarchy

Treat the user's stated end goal, required deliverables, and stop condition as the acceptance contract. Before accepting any design decision, ask: does this change, narrow, defer or substitute any part of the stated end goal?

Never use package conventions, specifications, GitHub records, workflows, tests, snapshots, schemas, implementation constraints, or internal consistency to override a higher-priority instruction or silently alter that contract. If a higher-priority instruction requires a limit, surface the conflict; otherwise obtain the user's explicit agreement before changing the end goal.

## Start here

Consult `PROJECT_MAP.md` when current, then inspect the relevant functions, callers, tests, documentation, configuration, `scripts/`, and CI workflow before changing behaviour or adding a helper command. Treat `future/`, `archive/`, and `legacy/` as planning or historical evidence, not current package behaviour.

Use the repository R wrapper rather than an unverified bare `Rscript`:

```bash
scripts/rscript_env_caller.R -e "R.home(); .libPaths()"
```

Set `EPISCOUT_RSCRIPT` only for another verified R binary. Run repository entry points instead of duplicating their command sequences:

```bash
scripts/check-local.sh
scripts/check-cran.sh
```

Use `scripts/check-local.sh` before review. Use `scripts/check-cran.sh` for release-oriented or CRAN-sensitive changes. Focused tests may call `devtools::test()` through the wrapper with an explicit CRAN mirror.

## Package conventions

- Use `snake_case`, two-space indentation, no tabs, and roxygen2 documentation for exported functions.
- Preserve released arguments and consumed interfaces unless a breaking change is authorised. Update experimental interfaces, callers, tests, and documentation together.
- Retain `%>%` for the current compatibility floor. Avoid `<<-`, `assign()`, and hidden interactive state unless required.
- Prefer declared dependencies. Record necessary additions in `DESCRIPTION` and explain the unmet current requirement.
- Edit roxygen source in `R/`, regenerate `man/`, and do not hand-edit generated Rd files, `NAMESPACE`, or other generated declarations.
- Match established British English and do not hard-wrap prose.

For analytical and missingness semantics, expected values must come from authoritative methods, explicit domain rules, or independently justified fixtures; passing package tests and snapshots are not independent scientific validation.

## Project checks and records

- Apply `checklists/software-verification.md` to code and interfaces; add truth, analysis, figure, copy-edit, or render checklists when their subject is in scope.
- Apply `checklists/statistical-test-audit.md` only when explicitly auditing whether tests independently establish consequential statistical, epidemiological or longitudinal behaviour; keep that audit read-only and bounded to 2–4 selected functions by default.
- Use coverage to find untested changed behaviour, not as a substitute for assertions or as an unmeasured blanket target.
- Treat CodeFactor as non-blocking unless its evidenced finding affects correctness, data integrity, security, privacy, performance, resource lifetime, compatibility, or a consequential claim.
- GitHub issues and PRs are the live work record. Use optional numbered specifications only when semantics, dependencies, migration risk, or cross-session handoff justify them. Do not create successor records merely to satisfy process.
- When a selected specification is active, keep its records accurate and run `scripts/check-workflow-state.sh`; the script is not a prerequisite for unrelated work.

## GitHub and publication

The canonical repository is public; recheck visibility before disclosure-sensitive work. Keep ignored data, outputs, secrets, and agent state private and use neutral fixtures.

Push contribution branches to the configured fork and target canonical `master`. Use `feature/<desc>`, `bugfix/<desc>`, or `refactor/<desc>` branches and `[Type] Summary` PR titles. A PR description states the change and reason, checks, compatibility impact, measured coverage impact when available, and unresolved limitations. Use `Closes #<issue>` only for the complete issue outcome.
