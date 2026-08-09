# Brief

Spec ID: `025-curp-validation-and-reconciliation`
Status: Completed and accepted through PR #231
Owner: repository-owner

## Problem

The exported `epi_clean_curp()` is described as accepting one or more CURPs, but its scalar `if` condition errors for vectors longer than one. It checks only character count, slices fields by position, accepts structurally invalid values, warns while coercing a non-numeric birth-year segment and assigns the century with a hard-coded `YY <= 22` rule. It does not validate calendar dates, official codes, the position-17 century marker or the verification digit.

The function also returns the original direct identifier without a documented restricted-data boundary. Existing tests cover two examples and length rejection but do not cite the governing instruction or establish boundary, vector, validity or privacy behaviour.

Source planning issue #217 requested quality comparisons between CURP-derived fields and separately collected birth date, registered sex/gender, birthplace and initials. Tracking issue #225 now owns the remaining activation gates and implementation. Those comparisons need explicit missing, mismatch and confidentiality semantics before code.

## Goal

Define an authoritative, vector-safe and privacy-aware contract for CURP structural validation, field derivation and optional comparison with reviewed collected fields. Preserve a deliberate compatibility path for `epi_clean_curp()` while introducing a result that distinguishes:

- malformed or missing input;
- internally consistent CURP structure;
- independently verified check digit;
- agreement or disagreement with separately collected fields; and
- official registry validation, which local parsing cannot establish.

## Non-goals

- Calculating or claiming verification of position 18 without an authoritative published algorithm or sufficient official vectors; checksum status is `not_verified` in this slice. Issue #230 closed through owner rescope without adopting an unofficial algorithm.
- Generating or assigning CURPs.
- Calling public or restricted government services.
- Claiming that syntactic/check-digit validity proves assignment, certification or identity.
- Fuzzy identity matching, record linkage or automatic correction.
- Writing CURPs, names, dates or row-level discrepancies to logs, snapshots or package artifacts.
- Changing pseudonymisation, PostgreSQL identity-universe or EDA workflows.

## Candidate Files For A Later Implementation

- `R/epi_clean_curp.R`
- `tests/testthat/test-curp-validation.R`
- `tests/testthat/test-curp_misc.R`
- `man/epi_clean_curp.Rd`
- `NEWS.md`
- a focused user guide if the public audit result is introduced

## Risks

- Treating an unofficial checksum implementation as the government algorithm; this slice deliberately does not calculate the checksum.
- Confusing local structural validity with authoritative registry validation.
- Deriving the wrong century or accepting an impossible date.
- Misclassifying collected sex/gender values by conflating CURP's encoded `H`/`M` field with broader identity concepts.
- Leaking direct identifiers or derived personal data through errors, printing, examples, snapshots or diagnostics.
- Breaking callers that rely on the existing Spanish column names and scalar output.
