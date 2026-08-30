# Review

Spec ID: `055-pseudonymisation-consolidation`
Status: Active

The design compares the current public contracts, registry schema, token-allocation implementation and PostgreSQL integration tests with a sanitised maintained-consumer capability inventory. The current implementation is atomic and preserves exact identifiers, but allocates the full token set in R, performs per-token collision queries, lacks state-aware privilege evidence and cannot import prior assignments. Those are concrete replacement gaps rather than reasons to copy downstream workflow code.

Compatibility is protected by keeping the four-component linkage object unchanged and placing new preparation metadata in a separate optional contract. Registry schema evolution is transactional and explicit. The public repository records only neutral behaviour; consumer-specific configuration and migration evidence remain private.

This is repository-owner self-review supported by source and test inspection, not independent human review. Implementation acceptance requires disposable PostgreSQL evidence and hosted CI.
