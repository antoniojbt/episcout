# Test Design

Spec ID: `042`
Status: Active

## Test Files

No new executable package tests are planned unless release verification finds a defect. Existing package, vignette and PostgreSQL tests provide the regression suite.

## Baseline Commands

```bash
scripts/check-workflow-state.sh
scripts/check-local.sh
scripts/check-cran.sh
```

## Behaviour Tests

- [ ] `DESCRIPTION` identifies the candidate as `0.4.0`.
- [ ] `NEWS.md` contains a `0.4.0` section that accurately summarises shipped changes.
- [ ] The source archive is versioned `episcout_0.4.0.tar.gz`, contains no prohibited development artefacts and installs into a clean temporary library.
- [ ] The installed package loads and exposes its documented vignettes.

## Edge-case Tests

- [ ] Candidate artifacts are generated from the exact preparation commit and inspected before publication.
- [ ] The tag and GitHub release target the exact canonical merge commit.

## Failure Tests

- [ ] If verification detects a package defect, add a focused regression test before correcting it.

## Acceptance Commands

```bash
scripts/check-workflow-state.sh
scripts/check-local.sh
scripts/check-cran.sh
```
