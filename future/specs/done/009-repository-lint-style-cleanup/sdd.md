# Software Design

Spec ID: `009-repository-lint-style-cleanup`  
Status: Implemented  

## Scope

Correct the lint policy, mechanically format only files with genuine findings, add narrow compatibility exceptions, remove dynamic-eval lint findings from `epi_stats_summary()`, and add local/CI enforcement.

## Baseline Contract

Loading the package before `lintr::lint_package()` yields 163 findings: 106 indentation, 34 pipe consistency, 12 object naming, 4 trailing whitespace, 2 infix spacing, 2 object length, 2 object usage and 1 brace finding. Mechanical findings affect 33 files across `R/`, tests and one vignette.

## Policy

- `%>%` remains the authoritative pipe and line-length linting remains disabled.
- Public exported names, public arguments and established output schemas remain unchanged and receive line-specific documented lint exceptions where necessary.
- Internal and test-only names may be corrected to snake_case.
- The lint command must call `devtools::load_all(quiet = TRUE)` before `lintr::lint_package()`.

## Data And Control Flow

The local check script and CI load the package, run lint, print any findings and exit non-zero when findings exist before running the existing test/check gates. No lint tooling is added to package runtime or test dependencies.

## Compatibility

No exported symbol, formal argument, return schema or minimum R version changes. Existing spec 008 behaviour and all prior tests remain authoritative.
