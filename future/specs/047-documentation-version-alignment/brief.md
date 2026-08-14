# Brief

Spec ID: `047-documentation-version-alignment`
Status: Active
Owner: Codex
Tracking issue: issue-327

## Goal

Bring episcout's version metadata, NEWS, help, guides, templates, installed examples and lifecycle records into agreement with canonical `master` after issue-323, without changing exported behaviour.

## Scope

Set the development version to `0.4.1.9000`; preserve the existing release-0.4.1 tag and explain its retained `Version: 0.4.0`; assign NEWS entries to the release or development version in which they were introduced; update roxygen sources and regenerate owned help; align every vignette, starter-project file and installed walkthrough; and reconcile issue-327 lifecycle records.

## Exclusions

This work does not retag release-0.4.1, begin release-0.4.2, change a public function or data-processing contract, add dependencies, or add package-owned approval, disclosure or governance decisions.

## Risks

The main risks are attributing work to the wrong release, hand-editing generated help, leaving a runnable shipped example on the removed EDA `type` field, or overstating cross-run pseudonym stability without the same persisted registry.

## Successor or Terminal Outcome

- Successor issue: none.
- Terminal reason if no successor is needed: this issue completes the bounded documentation and version reconciliation; later code or release work requires a new tracker.
