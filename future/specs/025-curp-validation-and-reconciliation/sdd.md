# Software Design

Spec ID: `025-curp-validation-and-reconciliation`
Status: Draft for owner review

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

The public instruction reviewed here does not state the complete position-18 algorithm. The later implementation must not claim check-digit validation until an official algorithm or official test-vector set is obtained and independently reconciled. A popular or copied implementation is insufficient evidence.

## Proposed Public Boundary

Add one audit-first API after owner acceptance:

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

Candidate stable statuses are `valid`, `invalid`, `missing` and `not_verified`. `valid` means only the locally defined structural contract passed; it must never be labelled `registered`, `certified`, `authentic` or equivalent.

## Compatibility Boundary

Retain `epi_clean_curp()` for at least one released compatibility cycle. A later implementation may refactor it over the shared parser if all of these remain explicit:

- the existing 13 Spanish column names and order;
- one output row per input element, including missing and invalid elements under a reviewed rule;
- the documented sensitivity of the returned `CURP` column; and
- a staged deprecation or migration note if stricter invalid-input behavior changes historical output.

The current vector error and stale century rule are defects, but correcting them must not silently redefine the legacy schema in the same commit as the new audit API without owner approval.

## Validation Stages

1. **Input shape:** require a character vector; retain length and positions without trimming silently. Missing input is `missing`, not malformed.
2. **Lexical structure:** require exactly 18 permitted uppercase characters under the official position classes. Case-normalisation, if allowed, must be reported rather than silent.
3. **Date and century:** parse positions 5–10 as an actual month/day and two-digit year. Position 17 constrains the official century class: numeric for births through 1999 and `A`–`J` from 2000 onward. It does not, by itself, prove that every numeric marker means 19xx in historical data. The owner must approve a supported year domain and explicit pre-1900 behavior before the parser returns a full date. No rule may depend silently on the current year.
4. **Encoded fields:** validate `H`/`M` and positions 12–13 against a pinned, provenance-recorded official birthplace catalogue. `NE` remains a birthplace code, not a Mexican state.
5. **Name-derived segments:** validate character classes only unless the caller provides already reviewed initials. Generating a CURP from names remains out of scope because official exception rules are consequential.
6. **Check digit:** calculate only after the official algorithm/test vectors pass the activation gate. Until then, report `not_verified`; do not substitute a guessed algorithm.
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

## Activation Gates

Package-code work must not start until the owner reviews and accepts:

1. the proposed audit object and legacy compatibility boundary;
2. the exact official birthplace catalogue and version to pin;
3. an official check-digit algorithm or official test vectors sufficient for independent verification;
4. synthetic/official example provenance and privacy handling;
5. whether lowercase input is rejected or normalised with an explicit issue; and
6. the supported year domain and treatment of possible pre-1900 historical keys.
