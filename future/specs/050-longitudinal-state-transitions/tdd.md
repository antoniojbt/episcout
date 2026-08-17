# Test Design

Spec ID: 050
Status: Active

## Test Files

- tests/testthat/test-eda-longitudinal-transitions.R
- tests/testthat/test-eda-longitudinal-postgres-transitions.R

## Independent Truth

A neutral hand-authored PostgreSQL panel contains all A→A, A→B, B→A and B→B transitions, missing→B and A→missing exclusions, repeated identical rows, conflicting states, entrants and exits, an unexpected level, a zero-observation period and at least three periods. Assertions state expected counts and denominators directly.

## Behaviour Evidence

- [x] Exact API, class, component order, typed schemas and metadata.
- [x] Period usable, missing and conflicting counts and reconciliation.
- [x] Four transition cells, eligible denominator and pair reconciliation match independent truth.
- [x] Missing and conflict exclusions are explicit; conflict precedence is tested.
- [x] Entrants and exits never appear as transitions.
- [x] Complete declared/unexpected domain order and zero-count cells.
- [x] Unsupported, absent, incompatible and invalid-missing rows preserve exact unavailable schemas, including n_transition_cells = 0L.
- [x] Binary canonical representation and no-declaration flags.
- [x] Zero eligible and zero-observation periods remain explicit.

## Boundary, Locality and Failure Evidence

- [x] max_levels rejects values outside 1–50.
- [x] Declared 51 levels, observed 51 levels and an adjacent union of 51 values each hard-fail without an object.
- [x] Query evidence proves database preflights are bounded at limit = max_levels + 1, including the adjacent union.
- [x] The actual public call retains one snapshot across a concurrent commit.
- [x] A real PostgreSQL error with a private marker is sanitised, rolled back and followed by successful connection reuse.
- [x] Identifier selection and identifier roles hard-fail before state queries.
- [x] Results, print output and errors contain no fixture entity values or row markers.
- [x] No database rows or objects are changed by success or failure.

## Acceptance Commands

    scripts/rscript_env_caller.R -e "parse('R/eda_longitudinal_transitions.R')"
    scripts/rscript_env_caller.R -e "lintr::lint('R/eda_longitudinal_transitions.R')"
    scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-longitudinal-transitions', reporter = 'summary')"
    EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-longitudinal-postgres-transitions', reporter = 'summary')"
    scripts/check-workflow-state.sh --offline
    scripts/check-local.sh
