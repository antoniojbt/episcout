# Review Notes

Spec ID: `012-data-frame-eda-spec-scaffold`
Status: Completed

## Planning Findings

- The current public EDA workflow validates and consumes a specification but provides no data-frame-to-draft bridge. The similarly named database scaffold accepts only `epi_db_inventory` and has a distinct extended dictionary contract.
- `summary_infer_type()` supplies compatible low-level storage mapping, while the new public contract needs stricter rejection of decorated numeric and nested storage and must not inherit typed `epi_stats_summary()` partial-skip behaviour.
- Existing strict ISO helpers treat empty vectors as vacuously valid for schema compatibility, so scaffold candidate logic must require at least one observed value.
- Existing semicolon-delimited level parsing cannot safely encode semicolons, empty declared levels or leading/trailing whitespace without semantic changes.
- Base CSV reading treats a sole quoted `"NA"` cell as missing; the approved `NA;` field representation prevents that loss, and the existing level parser recovers the intended literal level by discarding the trailing empty token.
- Enumerating observed low-cardinality values would conflict with the no-raw-values privacy boundary, so candidates expose only type and aggregate cardinality. Safe factor and logical declarations appear in core `levels`; `candidate_levels` remains blank in v1.
- Spec ID 011 remains conditionally reserved for the separately deferred Codecov history-remediation decision, so this feature uses spec ID 012.

## Semantic Decisions

- Storage class determines only the conservative initial type for explicitly supported classes.
- Value evidence may create a candidate type but never changes the initial type.
- Standard `NA` and `NaN` are missing; no string or numeric sentinel is inferred.
- `n_unique` follows the canonical non-missing character-representation convention; infinities remain observed.
- `required` is typed `NA`, and all other semantic specification fields remain blank.
- Observed candidate values are never emitted. Safe factor metadata and fixed logical metadata appear only in core `levels`; `candidate_levels` remains a blank reserved field in v1.
- Unsafe factor metadata causes an all-or-nothing error rather than a new escaping format or silent corruption.
- Every returned row remains `review_required`; a successful scaffold is neither analytical approval nor privacy approval.

## Approval And Checklist Routing

On 2026-08-03 the repository owner explicitly instructed implementation of all four ordered EDA issues, including issue #181. That instruction approves this completed brief, SDD, TDD and acceptance contract and activates spec 012, subject to stopping for any later ambiguity that would materially change privacy, scientific meaning or the public interface.

| Checklist | Application | Required evidence |
| --- | --- | --- |
| `software-verification.md` | Exported formals, stable typed output, failure behaviour, no side effects and integration | Focused and full tests, realistic invocation, inspected returned objects and compatibility assessment |
| `truth-and-semantics.md` | Missingness, storage-derived types, candidate meaning and distinction between evidence and semantic declaration | Explicit policies, hand-authored expectations, privacy limitations and unresolved questions |
| `analysis-and-statistics.md` | Counts, cardinality and candidate thresholds | Source-to-output reconciliation, denominator definitions, boundary tests and independent expected values |
| `copy-edit.md` | Roxygen, README, NEWS and vignette prose | Terminology, public formals, privacy caveats, British English and contextual review |

All checklist evidence recorded here is implementation self-review unless an independent reviewer later repeats it.

## Baseline Evidence

- Date: 2026-08-03.
- Command: `scripts/check-local.sh`.
- Result: passed with zero errors, zero warnings and zero notes.
- Test suite: passed with the two known environment skips.
- Worktree effects: generated artifacts affected by the check were inspected and restored before implementation.
- Interpretation: the passing baseline establishes executable prior state only and is not independent evidence for scaffold inference semantics.

## Open Questions

None blocking implementation.

## Implementation Review

- `epi_eda_spec_scaffold()` now returns the fixed 21-column draft/evidence contract in source order, including stable zero-column types. Initial type follows only explicitly supported storage; low-cardinality, whole-valued and strict temporal evidence remains a candidate requiring review.
- Standard `NA` and `NaN` are the only inferred missing states. Literal strings, empty strings, numeric codes and infinities remain observed, and every count reconciles to the source vector.
- Candidate levels remain blank, so low-cardinality observed values are not copied into the draft. Safe factor and logical storage metadata appears only in core `levels`; unsafe factor metadata fails during aggregated preflight without revealing its content.
- Unsupported nested, matrix, raw, complex, decorated numeric and arbitrary labelled columns fail before any partial result. Duplicate and empty names fail explicitly, while non-syntactic and Unicode names remain unchanged.
- Scaffold-signature CSVs are read with stable character fields and validated integer evidence, while ordinary specification CSVs retain their previous inferred column types and arbitrary extra fields such as `n` remain untouched.
- An independent review identified broad CSV type coercion, generic-extra-column capture, count overflow/reconciliation, timezone evidence and integer-range gaps. Each was corrected and covered by regression tests before full validation.
- A final independent review reproduced loss of a literal source name `"NA"` under base CSV missing-token parsing. Scaffold-signature CSV reading now preserves literal text in every character field and treats only the serialized `required` token as missing; default and blank-NA write modes are regression-tested.
- README, NEWS, roxygen and the rendered specification-first vignette distinguish aggregate structural evidence from scientific or privacy approval and demonstrate the received-data-to-reviewed-spec-to-canonical-EDA path.

## Verification Evidence

- The focused scaffold, API, specification and canonical-summary suite passed with hand-authored expectations covering exact field types, mixed supported storage, threshold boundaries, strict temporal parsing, sentinel-like values, empty inputs, unsafe metadata, unsupported storage, privacy, CSV round-trip and integration.
- A realistic in-memory invocation was inspected directly. It returned numeric, character-binary and character-date candidates without candidate values, round-tripped identically through CSV, and produced all six canonical summary components after explicit review edits.
- Package-loaded `lintr::lint_package()` reported no findings, and `git diff --check` passed.
- `scripts/check-local.sh` passed after one review-found vignette ordering correction. Documentation, lint, the full test suite, source build, installed examples, all vignettes and package check completed with zero errors, warnings or notes; the two known environment test skips remained unchanged.
- `scripts/check-cran.sh` completed the source build, tests, vignette rebuild, PDF manual and HTML manual with zero errors or warnings and one incoming-feasibility NOTE. The NOTE arose from unavailable external CRAN indexes, the inherited absence of a prebuilt vignette index and two inherited Stack Overflow URLs returning 403; it is unrelated to this feature.
- The final source tree contains the generated export and Rd file. Full checks again removed disabled visual snapshots and regenerated unrelated legacy Rd files; those known check side effects were restored before review.

## Closeout Notes

- The implementation meets the approved privacy and structural contract. Factor metadata and source column names can still be sensitive, so every output remains explicitly review-required before saving or sharing.
- Quarto and local GitHub Actions emulation are not required by this R Markdown/package workflow and were not installed. The repository R environment, Pandoc, compilers, TeX and package dependencies were verified available.
- No tag or release was created.
