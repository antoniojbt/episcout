# Acceptance

Spec ID: `042`
Status: Review

- [x] The SDD and TDD identify this as a GitHub-only `0.4.0` release and retain CRAN work under issue-81.
- [x] `DESCRIPTION` and `NEWS.md` accurately describe version `0.4.0`.
- [x] `scripts/check-workflow-state.sh`, `scripts/check-local.sh` and `scripts/check-cran.sh` pass or recorded non-blocking Notes have an evidenced disposition.
- [x] The exact source archive is inspected for intended contents and no confidential or developer artefacts.
- [x] The exact source archive installs into a clean temporary library; package loading and vignette discovery are verified.
- [ ] Hosted macOS, Ubuntu, PostgreSQL, coverage and static-analysis checks pass for the release-preparation merge commit.
- [ ] The preparation pull request is merged to canonical `master` and the exact merge commit is recorded.
- [ ] Tag `0.4.0` and its GitHub release target that exact canonical commit.
- [ ] Issue-307 is closed with release evidence; issue-81 remains open and deferred for CRAN readiness.
- [ ] The manifest is `completed` and this specification is moved under `future/specs/done/`.
