# Review Notes

Spec ID: `009-repository-lint-style-cleanup`  
Status: Implemented  

## Baseline Findings

- Genuine loaded-package findings: 163.
- Mechanical cleanup set: 33 files.
- A full styler sweep would touch roughly 146 files and is intentionally excluded.
- Public compatibility exceptions are required for exported palette names, long exported function names, dotted forwarding arguments, `logFC`, correlation `Var1`/`Var2` output fields and `.Random.seed` handling.

## Findings

- `.lintr` now makes the established `%>%` policy explicit while leaving line-length linting disabled.
- Mechanical formatting was limited to the 33 files identified by the baseline. The final targeted styler dry run was stable for all 33 files.
- Narrow line-specific exceptions preserve historical exported palettes, long exported function names, dotted forwarding arguments, the public `logFC` argument and `.Random.seed`; internal/test-only `cormat_tri_P` and `logFC` objects were renamed without changing public schemas.
- Current-mode `epi_stats_summary()` now selects columns and filters code values with direct functions, removing the `expression()`/`eval()` indirection while retaining every class/action output contract.
- The local check script and GitHub Actions load the package before linting, print findings and exit non-zero before tests/check when any finding exists. The existing workflow trigger indentation was corrected so the added CI gate is reachable on pushes and pull requests.
- Contributor instructions now use the repository R wrapper, changed-file styling, package-loaded linting, the `%>%` policy and the repository's no-hard-wrap/disabled-line-length policy.
- No runtime dependency, exported name, formal argument, return schema, native pipe or minimum-R requirement was added.

## Open Questions

None blocking implementation.

## Closeout Notes

Spec 009 is implemented and verified. Package-loaded lint reports zero findings, focused and full regression suites pass with the two known skips, and package check reports 0 errors, 0 warnings and only the accepted project-template `.gitkeep` NOTE. Existing spec 008 behaviour remains intact.
