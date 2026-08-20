# Probabilistic record linkage

Status: Active design. Parent issue: #360. Ordered implementation issues: #361 and #362.

## Problem

Episcout has exact identifier resolution and persistent pseudonymisation, but no explicit workflow for records whose stable identifiers are absent or inconsistent. Similar strings are evidence, not identity, and an unbounded pairwise comparison is unsafe at realistic sizes.

## Goal

Provide a generic two-source, in-memory workflow in which the caller declares record keys, normalisation, blocking, comparisons, Fellegi-Sunter parameters and decision thresholds. Preserve source values, expose derived field evidence for authorised review, report blocking and validation performance, and keep uncertain pairs in an explicit review state.

The first worked configuration uses entirely synthetic Mexican-name variations. It demonstrates generic text primitives rather than a Mexico-specific engine.

## Slices

1. Issue #361 implements declarations, derived preparation, bounded candidate generation and field comparison.
2. Issue #362 implements declared Fellegi-Sunter scoring, classification, complete-truth validation and the synthetic vignette.

Each slice has one issue, branch, commit boundary and pull request. The second implementation pull request is stacked on the first until its predecessor is canonical.

## Non-goals

- Inferring field roles, surname semantics, blocking keys, model parameters or thresholds.
- Estimating m/u probabilities or calibrating posterior probabilities in v1.
- Neural, phonetic or country-specific matching algorithms.
- Database-resident linkage, deduplication within one source, or enforcement of one-to-one assignment.
- Automatic linkage of real records or a claim that package output establishes identity.
- Automatic crosswalk, identity-registry or pseudonymisation writes.

## Basis

The scoring method follows Fellegi and Sunter's likelihood-ratio framework and three-way decision concept (DOI `10.1080/01621459.1969.10501049`). Current CRAN documentation was reviewed for `reclin2` 0.6.0, `fastLink` 0.6.1, `RecordLinkage` 0.4-12.6 and `stringdist` 0.9.17. Whole-pipeline packages were not selected for v1 because Episcout still needs its own explicit missing-state, bounded-candidate, value-opt-in and stable-output contracts. `stringi` and `stringdist` are the only specialised runtime requirements proposed for normalisation and string similarity.

## Risks

- Incorrect m/u probabilities or match prevalence produce misleading weights and model posteriors.
- Blocking can irretrievably exclude true matches.
- The conditional-independence and ignorable-missingness assumptions can be false.
- Names and quasi-identifiers remain sensitive even after normalisation.
- Thresholds validated on synthetic records do not transfer automatically to a real population or source system.

The API and vignette must make each limitation observable rather than hiding it behind defaults.
