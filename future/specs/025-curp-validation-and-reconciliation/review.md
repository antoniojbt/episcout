# Review Notes

Spec ID: `025-curp-validation-and-reconciliation`
Status: Draft for owner review

## Findings

The repository baseline does not satisfy its documented vector contract and does not implement CURP validity. The hard-coded `YY <= 22` rule conflicts with the official position-17 century discriminator. Length-only acceptance allows malformed values to be sliced, and coercion can emit input-dependent warnings. Existing evidence is therefore extraction smoke coverage, not authoritative validation.

The current official instruction is sufficient to design position classes, date/century behaviour, the distinction between local structure and registry validation, and the confidentiality boundary. It is not sufficient by itself to implement the verification-digit algorithm because it refers to an algorithm without publishing its complete calculation.

## Open Questions For Owner Review

1. Accept the proposed `epi_clean_curp_audit()` list contract, or prefer separate validate/extract/compare functions?
2. Retain `epi_clean_curp()` unchanged for one compatibility cycle, or approve a staged stricter migration now?
3. Reject lowercase input, or normalise it while recording an explicit issue?
4. Support only an explicitly documented 1900–2099 date domain, or define another evidence-backed treatment for possible pre-1900 historical keys?
5. Which official birthplace catalogue artifact should be pinned in the repository?
6. Can RENAPO provide the official verification-digit algorithm or official test vectors?
7. Does the missing photo contain non-sensitive requirements not captured in issue #217? It must not be used if it contains a real CURP.

## Closeout Notes

Planning only. No package code, tests, generated documentation, API or behaviour changed. PR #224 merged and source planning issue #217 closed. Tracking issue #225 now owns the remaining gates and implementation under roadmap #227; the draft remains blocked from activation until the source and owner gates in `acceptance.md` are resolved.
