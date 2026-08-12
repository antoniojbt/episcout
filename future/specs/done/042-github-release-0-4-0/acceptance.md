# Acceptance

Spec ID: `042`
Status: Completed pending GitHub tag and release publication

- [x] The SDD and TDD identify this as a GitHub-only `0.4.0` release and retain CRAN work under issue-81.
- [x] `DESCRIPTION` and `NEWS.md` accurately describe version `0.4.0`.
- [x] `scripts/check-workflow-state.sh`, `scripts/check-local.sh` and `scripts/check-cran.sh` pass or recorded non-blocking Notes have an evidenced disposition.
- [x] The exact source archive is inspected for intended contents and no confidential or developer artefacts.
- [x] The exact source archive installs into a clean temporary library; package loading and vignette discovery are verified.
- [x] Hosted macOS, Ubuntu, PostgreSQL, coverage, CodeFactor and Codecov checks pass for the release-preparation merge commit.
- [x] Preparation PR-308 merged to canonical `master` as `commit-9b4d5df7e8367c28aba59b8f1c412a5b9845f0f4`.
- [ ] Tag `0.4.0` and its GitHub release target that exact canonical commit.
- [ ] Issue-307 is closed with release evidence; issue-81 remains open and deferred for CRAN readiness.
- [x] The manifest is `completed` and this specification is moved under `future/specs/done/`.
