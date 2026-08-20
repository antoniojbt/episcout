# Acceptance

Spec ID: 052
Status: Completed

- [x] The planning PR is accepted before either implementation PR merges.
- [x] Issue #361 delivers only declaration, preparation, candidates and comparisons.
- [x] Issue #362 delivers scoring, classification, validation and documentation on top of #361.
- [x] Every public role, field, block, comparison, probability and threshold is caller-declared.
- [x] Raw sources remain unchanged and derived representations are explicit.
- [x] No unbounded Cartesian candidate path exists.
- [x] Candidate and validation counts reconcile to independently authored truth.
- [x] Similarity, comparison state, linkage weight, model posterior and decision are distinct.
- [x] The package makes no empirical calibration claim.
- [x] Missing evidence is explicit and its modelling assumption is documented.
- [x] Routine outputs are value-free; record IDs and comparison values require opt-in.
- [x] The synthetic Mexican-name workflow covers the full issue reference set without a Mexico-specific engine.
- [x] No probabilistic result enters an exact crosswalk or identity registry automatically.
- [x] Required focused, local, CRAN-oriented and hosted checks pass or any exception is explicitly accepted.
- [ ] Review notes record independent comparison of code, statistical truth, privacy behaviour and rendered documentation.
- [x] Each issue, branch, commit, PR, merge and successor/terminal state is read back and reconciled.

## Local implementation evidence

- Focused linkage selection: 233 expectations passed, including the existing exact `epi_sec_*` linkage contract.
- Focused coverage across the three linkage source files: 92.98%; `R/epi_linkage_score.R` measured 95.56%. Coverage guided edge-test additions and is not treated as independent statistical validation.
- `scripts/check-local.sh`: all tests and six vignettes passed; R CMD check completed with 0 errors, 0 warnings and the two existing current-time/top-level-`docs` notes.
- `scripts/check-cran.sh`: build, tests, vignette rebuild and PDF/HTML manuals passed with 0 errors and 0 warnings; three expected notes cover incoming feasibility, current time and top-level `docs`.
- `scripts/check-workflow-state.sh --offline` is structurally consistent and `git diff --check` passes.
- Planning PR #363, foundation PR #364 and terminal implementation PR #365 passed Ubuntu R CMD check, PostgreSQL integration, coverage, Codecov project/patch and CodeFactor at their recorded implementation heads. The one initial foundation coverage run failed in an unrelated existing civil-date transaction test; its single retry passed all steps.
- PR #363 merged as `a92ed8e81bfe1699862fdb2b0932d77c5b939fec`, PR #364 as `3feb5c89a0e6bd82d9e460a172e3795922a2f376` and PR #365 as `a9e81770a107ab0c604db136170266afc40f1efe`. Issues #361 and #362 closed automatically; parent #360 closed during lifecycle reconciliation.
- Canonical post-merge CodeQL, Ubuntu R CMD check, PostgreSQL integration and coverage workflows passed at `a9e81770a107ab0c604db136170266afc40f1efe`.
- No separate independent GitHub review artefact is recorded for the behaviour-sensitive comparison. The owner merged the stack after its required checks passed; this closeout records, rather than retroactively fills, that acceptance-evidence gap.
