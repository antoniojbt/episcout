# Review Notes

Spec ID: `025-curp-validation-and-reconciliation`
Status: Review

## Findings

The repository baseline does not satisfy its documented vector contract and does not implement CURP validity. The hard-coded `YY <= 22` rule conflicts with the official position-17 century discriminator. Length-only acceptance allows malformed values to be sliced, and coercion can emit input-dependent warnings. Existing evidence is therefore extraction smoke coverage, not authoritative validation.

The current official instruction is sufficient to design position classes, date/century behaviour, the distinction between local structure and registry validation, and the confidentiality boundary. It is not sufficient by itself to implement the verification-digit algorithm because it refers to an algorithm without publishing its complete calculation.

## Owner Decisions

1. Use the proposed `epi_clean_curp_audit()` list contract.
2. Retain `epi_clean_curp()` as a length-only 13-column compatibility extractor for one release cycle while fixing its documented vector failure.
3. Reject lowercase and whitespace without silent normalisation.
4. Support local derivation only for 1900–2099 and document that a possible pre-1900 key cannot be distinguished from the corresponding 1900s key.
5. Pin the birthplace catalogue published with the 2021 RENAPO assignment rules.
6. Report checksum status as `not_verified`; issue #230 owns authoritative evidence and any later verification implementation.
7. Do not use the missing photo or real personal data.

## Closeout Notes

Planning PR #224 merged and source issue #217 closed. Owner direction on 2026-08-09 resolved the activation gates by explicitly deferring checksum verification to issue #230. Tracking issue #225 now owns the active structural audit, comparison and one-cycle legacy compatibility implementation under roadmap #227.

## Implementation Review On 2026-08-09

The implementation adds `epi_clean_curp_audit()` with a fixed four-component result, stable input indices, exact structural issue codes, safe aggregate printing and typed comparison states. It does not return the supplied CURP, calculate a check digit, contact a registry or perform file/network writes. The derived row-level fields remain documented as restricted personal data.

The legacy `epi_clean_curp()` remains a positional extractor with its exact 13-column Spanish schema. Its documented vector interface now returns one row per input, empty and missing inputs are deterministic, and malformed year text no longer emits input-derived coercion warnings. It remains explicitly separate from structural validation.

The birthplace catalogue is pinned from the RENAPO rules published 2021-12-17. The official attachment was accessed 2026-08-09 with SHA-256 `c41fe8044a73e12802ca615d33dc3660c7e120d39603c77faa7bb50ab4d39be8`; the installed CSV contains only the 32 entity codes plus `NE` and no personal data.

Focused tests passed 30 legacy compatibility expectations and 69 audit expectations. Package lint found no issues, the complete test suite passed with only documented opt-in integration/graphics skips, and `scripts/check-local.sh` completed with 0 errors, 0 warnings and 0 notes. Draft PR #231 passed Ubuntu, macOS, PostgreSQL integration, coverage, both Codecov gates and CodeFactor. The final diff has no whitespace errors and excludes unrelated generated documentation and snapshot cleanup.

The unresolved boundary is deliberate: `valid` means only the documented local structural contract passed, position 18 remains `not_verified`, and registry assignment, certification, authenticity and identity are not inferred. Issue #230 is the non-blocking successor for authoritative checksum evidence. Draft PR #231 carries the implementation; owner review, merge and post-merge closeout remain pending.
