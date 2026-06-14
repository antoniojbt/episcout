-----------
Follow Instruction 2 in:

spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md

for PR 2: Add failing tests for specification, schema and missingness
-----------


-----------
You are running locally in my Positron terminal.

Repository: antoniojbt/episcout
Base branch: episcout2

Task:
Complete only the active PR-sized task in spec_driven_EDA_plan/docs/START_HERE.md.
For the current active task, PR 2, expected failures from intentionally missing
EDA functions are allowed and must not be fixed in this PR.

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
