# Acceptance

Spec ID: `031-canonical-eda-delivery`  
Status: Active

## Planning

- [x] Issue #245 and roadmap #249 identify this as the ready-next package work.
- [x] Superseded issue #196 contributes technical renderer requirements only.
- [x] The SDD fixes public API, compatibility, ownership and failure contracts before implementation.
- [x] The TDD covers integrity, atomicity, relocation, database absence and rendered truth.
- [x] Planning PR #251 is green at `4b7b10b` with no actionable review feedback before package changes.

## Implementation

- [x] Default database bundle and data-frame report APIs remain backward compatible.
- [x] Delivery layout contains one owned copy of every artifact and a clear HTML/README entry point.
- [x] Existing valid database bundles render without database or source-row access.
- [x] Manifest/checksum/path validation and atomic restore fail closed for technical inconsistency.
- [x] Aggregate plot data are published by default and exclude map/source observations.
- [x] Report, README, migration notes and walkthrough are user-friendly and contain no core governance behavior.

## Verification And Closeout

- [x] Focused offline and live PostgreSQL tests pass.
- [x] Lint, local, CRAN-oriented and workflow checks pass with reconciled notes only.
- [x] Exact generated HTML/SVG/CSV artifacts are independently reconciled and visually inspected.
- [ ] Required PR checks are green with no actionable review feedback.
- [ ] Implementation PR closes #245 and records #248 as successor only after all outcomes are delivered.
- [ ] Canonical merge, issue closure, roadmap, TODO, changelog and local/fork master are reconciled.
- [ ] Manifest is completed and spec 031 is moved under `future/specs/done/`.
