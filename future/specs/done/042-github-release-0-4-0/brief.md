# Brief

Spec ID: `042`
Status: Active
Owner: Codex
Tracking issue: issue-307

## Problem

The current canonical branch contains substantial additive package work after GitHub release `0.3.0`, but `DESCRIPTION` still identifies the package as `0.3.0`; live projects need one immutable, verifiable installation target.

## Goal

Prepare, verify and publish a GitHub-only `0.4.0` release from one canonical commit, with accurate release notes and a reviewable source-package artifact.

## Non-goals

- CRAN submission, pretest, or any representation that CRAN readiness is complete.
- New package behaviour beyond defects found during release verification.
- Release of confidential data, credentials, local paths or developer artefacts.

## Candidate Files

- `DESCRIPTION`
- `NEWS.md`
- `future/specs/042-github-release-0-4-0/`

## Risks

- Release checks may reveal an actual package, documentation, tarball or installation defect.
- A release tag must refer to the exact verified canonical commit, not the branch head before merge.

## Successor or Terminal Outcome

- Successor issue: none.
- Terminal reason: the GitHub release creates the requested immutable live-evaluation baseline; CRAN work remains independently tracked by issue-81.
