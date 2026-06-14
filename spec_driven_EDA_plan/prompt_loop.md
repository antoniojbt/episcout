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
Complete only the active PR-sized task in START_HERE.md.

Required sequence:
1. Confirm the git worktree is clean.
2. Create a feature branch from episcout2.
3. Read AGENTS.md and START_HERE.md.
4. Make only the scoped changes.
5. Run:
   - Rscript -e "devtools::document()"
   - Rscript -e "devtools::test(reporter = 'summary')"
   - Rscript -e "devtools::check(manual = FALSE)"
6. Fix errors, warnings and notes caused by this PR.
7. Do not fix unrelated pre-existing issues.
8. Commit the final change.
9. Push the branch.
10. Create a PR against episcout2 with gh.
11. Run gh pr checks --watch --fail-fast.
12. If CI fails because of this PR, fix, commit and push once.
13. Stop and summarise files changed, checks run and remaining issues.
-----------
