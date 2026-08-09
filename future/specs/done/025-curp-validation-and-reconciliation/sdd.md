# Software Design

Spec ID: `025-curp-validation-and-reconciliation`
Status: Completed and accepted through PR #231

## Authoritative Basis Reviewed On 2026-08-09

1. Secretaría de Relaciones Exteriores, [Instructivo Normativo para la Asignación de la Clave Única de Registro de Población, texto vigente, modified in DOF on 2021-10-18](https://sre.gob.mx/component/phocadownload/category/2-marco-normativo?download=1116%3Ainstructivo-normativo-para-la-asignacion-de-la-clave-unica-de-registro-de-poblacion-dof-18-10-2021-texto-vigente).
2. RENAPO, [Reglas para la ejecución de los procedimientos para la asignación de la CURP](https://www.gob.mx/segob%7Crenapo/documentos/reglas-para-la-ejecucion-de-los-procedimientos-para-la-asignacion-de-la-clave-unica-de-registro-de-poblacion), published 2021-12-17, with its [official PDF attachment](https://www.gob.mx/cms/uploads/attachment/file/681698/reglas_para_la_ejecucion_de_los_procedimientos_asignacion_de_la_curp.pdf).
3. Diario Oficial de la Federación, [2026 Rules of Operation for strengthening civil registries](https://www.dof.gob.mx/abrirPDF.php?anio=2026&archivo=20022026-MAT.pdf&repo=), which continues to refer to the governing CURP instruction as current.

The current instruction establishes the claims this draft may rely on:

- the key is alphanumeric and has 18 positions;
- positions 5–10 encode birth date as `YYMMDD`;
- position 11 is the recorded code `H` or `M`;
- positions 12–13 encode birthplace under the official catalogue;
- positions 14–16 are internal consonants;
- position 17 is numeric for dates through 1999 and `A`–`J` from 2000 onward;
- position 18 is numeric and assigned through a Secretaría de Gobernación verification algorithm;
- government online/offline mechanisms—not local parsing—validate a CURP against official records; and
- BDNCURP information is confidential and subject to applicable personal-data law.

The public instruction reviewed here does not state the complete position-18 algorithm. By owner direction on 2026-08-09, checksum evidence is no longer a blocking gate for structural validation: this implementation reports position 18 as `not_verified` and does not calculate it. Issue #230 closed through that explicit rescope without adopting an unofficial algorithm; a future checksum change requires new authoritative evidence and a separately reviewed tracker.

## Proposed Public Boundary

Add one audit-first API:

```r
epi_clean_curp_audit(
  curp,
  birth_date = NULL,
  sex_code = NULL,
  birthplace_code = NULL,
  initials = NULL
)
```

The function accepts parallel vectors of length one or `length(curp)`. Optional comparison vectors are explicit; recycling other than length one is rejected. Names, full birth records or other fields are not accepted.

Return an `epi_curp_audit` list with:

- `records`: one row per input with `input_index`, structural status, derived date, encoded sex code, birthplace code, initials segment, century-marker class and check-digit status;
- `issues`: zero or more rows with `input_index`, stable issue code, stage and severity, but no CURP or comparison value;
- `comparison`: one row per input containing only typed match states for supplied reference fields; and
- `summary`: aggregate counts by status and issue code.

The original CURP is not echoed in the result. `input_index` provides alignment with caller-controlled data. The custom print and condition methods display only row counts, status counts and fixed guidance. The returned derived fields are still personal data and must be documented as restricted.

Stable record statuses are `valid`, `invalid` and `missing`; checksum status is separately `not_verified` for structurally valid records and unavailable otherwise. `valid` means only the locally defined structural contract passed; it must never be labelled `registered`, `certified`, `authentic` or equivalent.

## Compatibility Boundary

Retain `epi_clean_curp()` for at least one released compatibility cycle. This implementation fixes its documented vector interface while retaining the historical 13-column extraction schema and permissive length-only extraction behaviour. It must not be presented as validation. A later release may refactor it over the strict parser after a staged migration if all of these remain explicit:

- the existing 13 Spanish column names and order;
- one output row per input element, including missing and invalid elements under a reviewed rule;
- the documented sensitivity of the returned `CURP` column; and
- a staged deprecation or migration note if stricter invalid-input behaviour changes historical output.

The current vector error and stale century rule are defects, but correcting them must not silently redefine the legacy schema in the same commit as the new audit API without owner approval.

## Validation Stages

1. **Input shape:** require a character vector; retain length and positions without trimming silently. Missing input is `missing`, not malformed.
2. **Lexical structure:** require exactly 18 permitted uppercase characters under the official position classes. Lowercase, surrounding whitespace, punctuation and Unicode are rejected without normalisation.
3. **Date and century:** parse positions 5–10 as an actual month/day and two-digit year. A numeric position 17 maps to the supported 1900–1999 domain and `A`–`J` maps to 2000–2099. Dates later than the local current date are structurally invalid. A possible pre-1900 key is indistinguishable from the corresponding 1900s key using the CURP alone, so the audit cannot support or identify it and documents that limitation rather than guessing.
4. **Encoded fields:** validate `H`/`M` and positions 12–13 against the catalogue published with the 2021 RENAPO assignment rules and pinned in `inst/extdata`. `NE` remains a birthplace code, not a Mexican state.
5. **Name-derived segments:** validate character classes only unless the caller provides already reviewed initials. Generating a CURP from names remains out of scope because official exception rules are consequential.
6. **Check digit:** report `not_verified` for structurally valid records and do not calculate position 18. Issue #230 closed through owner rescope; a later implementation requires new authoritative evidence and a separately reviewed tracker.
7. **Comparison:** compare only supplied reviewed reference values. Each field returns `match`, `mismatch`, `reference_missing`, `curp_unavailable` or `not_requested`.

## Comparison Semantics

- Birth date comparison uses exact `Date` equality after successful CURP date derivation.
- `sex_code` compares an explicitly mapped source code to CURP's encoded `H`/`M`; documentation must call this the CURP recorded sex code, not infer gender identity.
- Birthplace compares reviewed CURP catalogue codes, not free-text state names.
- Initials compare a separately derived, caller-reviewed four-character segment; the package does not generate it from names in this scope.
- A malformed CURP never produces a mismatch: the comparison state is `curp_unavailable`.
- A missing reference never produces a match or mismatch.

## Privacy And Security

CURP and its derived birth date, birthplace and encoded sex are personal data. The implementation must:

- avoid values in errors, warnings, messages, print methods, snapshots and telemetry;
- avoid automatic files, network calls or registry lookups;
- use only source-cited official examples or owner-approved synthetic fixtures;
- document that the caller controls storage, access, retention and linkage;
- make row-level comparison results explicitly restricted; and
- avoid claims of anonymity, authentication or proof of identity.

## Dependencies

No new dependency is expected. Base R can perform fixed-position parsing and exact `Date` validation. Any proposed dependency requires a separate justification and provenance review.

## Activation Decision

The owner activated this specification on 2026-08-09 with the audit object, one-cycle legacy boundary, strict uppercase/no-whitespace input, 1900–2099 local date domain, 2021 RENAPO birthplace catalogue and restricted synthetic-fixture policy accepted. PR #231 merged the resulting implementation to canonical `master` as `7e42f228969dbc62060f0660c43119882140052f`. Checksum calculation remains explicitly deferred as `not_verified`; issue #230 closed through owner rescope without authorising an unofficial algorithm.
