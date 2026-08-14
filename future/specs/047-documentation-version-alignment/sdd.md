# Software Design

Spec ID: `047-documentation-version-alignment`
Status: Review

## Scope And Sources Of Truth

Canonical `master` after PR-324 defines the current package interface. Git history and the immutable release tags define when work was introduced. Roxygen comments under `R/` remain the source for `man/` and `NAMESPACE`; generated files are never edited directly. `DESCRIPTION` defines the development package version, while `NEWS.md` records release chronology.

## Version And Release Records

Set `DESCRIPTION` to `Version: 0.4.1.9000`. Keep tag `0.4.1` at its existing commit and add an explicit GitHub release-note qualification that the tagged source retained `Version: 0.4.0`. Keep work included in tag `0.4.0` under NEWS 0.4.0, move work merged after tag `0.4.0` and included in tag `0.4.1` under NEWS 0.4.1, and place issue-323 work merged after tag `0.4.1` under the 0.4.1.9000 development heading.

## Documentation And Installed Files

Audit `README.md`, all five vignettes, exported-function roxygen, generated manuals, the starter-project template, installed examples and both database-to-report walkthrough files. Replace obsolete version, 15-field and removed-`type` claims. The 16-field flat EDA specification is `name`, `label`, `database_type`, `analysis_type`, `role`, `units`, `levels`, `min`, `max`, `missing_codes`, `required`, `group`, `description`, `geo_role`, `geo_pair` and `geo_crs`.

The starter dictionary supplies both type fields and must load successfully through the released `epi_eda_spec()` entry point. Documentation distinguishes storage family from analytical treatment and does not restore a compatibility alias.

Longitudinal documentation states that recurring identities receive stable pseudonymous identifiers when runs reuse the same persisted registry. Separate registries do not establish cross-run identity stability. Pseudonymisation remains distinct from anonymity, disclosure approval and package governance.

## Public API And Data Flow

No public function, formals list, return schema or processing behaviour changes. The only executable fixture change is the shipped starter dictionary; its schema catches up with the already-merged public contract. Roxygen regeneration may update owned `man/` content and `NAMESPACE` only as generated consequences of source documentation.

## Failure And Compatibility Boundaries

The existing release tag and its commit must remain byte-for-byte unchanged. Examples that still use the removed `type` field must be corrected rather than supported. Documentation claims must be checked against current formals, validators, tests and Git history. Any discovery requiring behaviour changes, a new dependency or governance policy is out of scope and requires a separate tracker.
