# Review

Spec ID: `055-pseudonymisation-consolidation`
Status: Completed

The design compares the current public contracts, registry schema, token-allocation implementation and PostgreSQL integration tests with a sanitised maintained-consumer capability inventory. The current implementation is atomic and preserves exact identifiers, but allocates the full token set in R, performs per-token collision queries, lacks state-aware privilege evidence and cannot import prior assignments. Those are concrete replacement gaps rather than reasons to copy downstream workflow code.

Compatibility is protected by keeping the four-component linkage object unchanged and placing new preparation metadata in a separate optional contract. Registry schema evolution is transactional and explicit. The public repository records only neutral behaviour; consumer-specific configuration and migration evidence remain private.

This is repository-owner self-review supported by source and test inspection, not independent human review. Implementation acceptance requires disposable PostgreSQL evidence and hosted CI.

Successors #394, #395, #396 and #397 merged through PRs #398, #399, #400 and #401. Source-only release 0.6.0 published the accepted replacement. A neutral downstream rehearsal then exposed a regex-backed registry-import SQL-scope defect; #402/PR #403 corrected it with a live PostgreSQL regression, and source-only 0.6.1 published that patch. The maintained downstream transition subsequently passed its own repository checks and a disposable PostgreSQL walkthrough covering token preservation, all-source enrolment, declared-column removal, reconciliation and fingerprints. No project-specific configuration or data is retained here.
