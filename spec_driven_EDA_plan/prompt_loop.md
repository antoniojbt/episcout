-----------
Reading order and mapping for SDD/TDD in this directory:
- START_HERE.md for what to do now.
- revised-pr-plan-tdd-first.md for where the current PR sits in the sequence.
- tdd-first-codex-instructions.md for the exact task block.
- roadmap.md for why this work exists and which phase it belongs to.
- review-checklist.md before closing any PR.



The mapping is hierarchical, not one-to-one:

**Control Files**
`roadmap.md` is the strategic map: phases and broad intent.

`START_HERE.md` is the live operational state. It explicitly says it is the “operational source of truth” for what happens next, including active PR, allowed files, and expected test state ([START_HERE.md](</Users/antoniob/Library/Mobile Documents/com~apple~CloudDocs/Documents/work/science/devel/github/antoniojbt/episcout/spec_driven_EDA_plan/docs/START_HERE.md:15>)).

`revised-pr-plan-tdd-first.md` is the stable PR sequence. It translates roadmap phases into PR-sized work and enforces the pattern: fixture/expected output, failing tests, implementation ([revised-pr-plan-tdd-first.md](</Users/antoniob/Library/Mobile Documents/com~apple~CloudDocs/Documents/work/science/devel/github/antoniojbt/episcout/spec_driven_EDA_plan/docs/codex/revised-pr-plan-tdd-first.md:11>)).

`tdd-first-codex-instructions.md` is the executable instruction bank. Each instruction corresponds roughly to one PR, especially PRs 1-9. Later work is collapsed into Instruction 10 ([tdd-first-codex-instructions.md](</Users/antoniob/Library/Mobile Documents/com~apple~CloudDocs/Documents/work/science/devel/github/antoniojbt/episcout/spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md:272>)).

`review-checklist.md` applies to every PR before closeout/review ([review-checklist.md](</Users/antoniob/Library/Mobile Documents/com~apple~CloudDocs/Documents/work/science/devel/github/antoniojbt/episcout/spec_driven_EDA_plan/docs/codex/review-checklist.md:3>)).

**Phase Mapping**

| Roadmap phase | Actual operational mapping |
|---|---|
| Phase 0: Documentation and baseline | Done. This produced SDD, ADRs, roadmap, Codex instructions, and PR plan. Tracked in `START_HERE.md` as done. |
| Phase 1: Stabilise current package | Not clearly mapped to PRs. It has audit material, but no active PR sequence or completion evidence. Looks bypassed/deferred. |
| Phase 2: TDD fixtures for spec-first EDA | PR 1 and PR 2. PR 1 adds external fixtures and expected outputs. PR 2 adds failing tests for spec/schema/missingness. |
| Phase 3: Specification-first MVP | PR 3 through PR 9, but in test-first slices: implement spec/schema/missingness, test synthetic data, implement synthetic data, test summaries/plots, implement summaries/plots, test `epi_eda_run()`, implement `epi_eda_run()`. Current active PR is PR 4. |
| Phase 4: Reporting and project template | PR 10, PR 11, PR 12. Report tests, report implementation, project template. |
| Phase 5: Large-data optimisation | PR 13 only documents large-data strategy. Actual `data.table`, DuckDB, Arrow implementation is explicitly not planned yet. |
| Phase 6: Biomedical extensions | No PRs/instructions yet. It is future scope only. |

-----------


-----------
Prompt that can be re-used at each stage/step:

You are running locally in my Positron terminal.

Repository: antoniojbt/episcout
Base branch: episcout2

Task:
Complete only the active PR-sized task in spec_driven_EDA_plan/docs/START_HERE.md.
For the current active task, PR 2, expected failures from intentionally missing
EDA functions are allowed and must not be fixed in this PR.

A local mamba env episcout should be available.

Required sequence:
1. Confirm the git worktree is clean.
2. Confirm GitHub CLI availability:
   - command -v gh
   - gh auth status
   If gh is unavailable or unauthenticated, continue through local changes and
   commit, then stop before push/PR/check steps and summarise what remains manual.
3. Ensure the current branch is episcout2 and up to date with origin/episcout2.
4. Create a feature branch from episcout2 using feature/<short-desc>.
5. Read:
   - spec_driven_EDA_plan/docs/AGENTS.md
   - spec_driven_EDA_plan/docs/START_HERE.md
   - all files listed as required reading in START_HERE.md
6. Make only the scoped changes from START_HERE.md.
7. Respect must-edit and must-not-edit lists exactly.
8. Run:
   - Rscript -e "devtools::document()"
   - Rscript -e "devtools::test(reporter = 'summary')"
   - Rscript -e "devtools::check(manual = FALSE)"
9. Fix only errors/warnings/notes caused by this PR, except expected failures
   explicitly documented in START_HERE.md.
10. Do not implement functions reserved for later PRs.
11. Do not commit changes to files listed as must-not-edit.
12. Commit the final scoped change.
13. If gh is available and authenticated:
   - push the branch
   - create a PR against episcout2 with gh
   - run gh pr checks --watch --fail-fast
14. If CI fails only because of expected PR 2 failing tests, record that and stop.
15. If CI fails because of an unintended issue introduced by this PR, fix,
    commit and push once.
16. Stop and summarise files changed, checks run, expected failures, and any
    remaining unrelated/pre-existing issues or manual GitHub steps.
-----------
