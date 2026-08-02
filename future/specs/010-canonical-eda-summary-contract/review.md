# Review Notes

Spec ID: `010-canonical-eda-summary-contract`  
Status: Active

## Planning Findings

- The `summary_version` interface and v1/v2 terminology were added after release `0.2.0`, while the older two-table output itself was released. The user has explicitly stated that external compatibility does not matter, so the implementation may replace the released return shape without a legacy adapter.
- Current tests establish that the default equals explicit `v1`, but they do not establish exact historical values for important edge cases.
- The current typed path excludes infinities from numeric calculations and records them separately, but reports `sum = 0` when no finite values exist. This conflicts with the repository requirement not to turn all-missing analytical inputs into observed zeroes.
- The current typed path uses the dataset row count as `n` for an absent variable, although no source vector exists for that variable.
- Current categorical core behaviour can treat unused factor levels as declared even when the EDA specification does not declare them. The canonical contract instead treats specification levels as authoritative.

## Open Questions

None blocking planning. Any new ambiguity that materially changes statistical meaning must be recorded and resolved before production code is changed.

## Approval And Checklist Routing

On 2026-08-02 the repository owner authorised selecting and implementing the next planned work autonomously and explicitly authorised continuing after the repository scrub. Spec 010 is the Priority 1 implementation selected under that instruction, so its brief, SDD, TDD and acceptance contract are approved without semantic changes.

| Checklist | Application | Required evidence |
| --- | --- | --- |
| `truth-and-semantics.md` | Missingness, finite-value exclusions, categorical declarations, denominators, temporal parsing, absent variables and all-missing totals | Explicit semantic decisions, hand-derived fixtures and unresolved items |
| `analysis-and-statistics.md` | Numeric, categorical, text and temporal summary values | Independent calculations, source-to-output reconciliations, row counts, denominators, warnings and limitations |
| `software-verification.md` | Public formals, canonical builder, failure behaviour, CSV writes and package integration | Focused tests, realistic invocations, full checks, inspected outputs and compatibility consequences |
| `figures.md` | EDA plots presented by the report and plot dispatch after missing-variable filtering | Reconciled plot-layer data, sentinel handling and rendered inspection |
| `copy-edit.md` | Roxygen, README, NEWS, vignette and report prose | Files reviewed, terminology changes and unresolved editorial issues |
| `render-and-release.md` | HTML report, generated Rd, source tarball and CSV artifacts | Exact artifacts inspected, render method, sections checked and source-package contents |

All checklist work recorded here is implementation self-review unless an independent reviewer later repeats it.

## Baseline Evidence

- Base: `e5514f3d06e8060ce7a30945beb2e1f534a36fd3` on `refactor/canonical-eda-summary-contract`.
- Environment: R 4.5.3 on Ubuntu 24.04.4, using the repository wrapper and the `episcout` mamba environment.
- Focused baseline command: `devtools::test(filter = 'eda-summaries|run_eda|eda_report|db-dictionary', reporter = 'summary')`; passed on 2026-08-02.
- Full baseline command: `scripts/check-local.sh`; all tests and lint passed with two known skips, and package check reported the existing hidden `.gitkeep` NOTE. The check also reproduced the known roxygen warnings for the absent `vignettes/summary_funcs_examples.R` file and the known skipped-snapshot pruning side effect; generated drift was restored after inspection.
- Release-oriented baseline command: `scripts/check-cran.sh`; dependency checking initially failed because the declared environment omitted `targets`. The separate scaffold-packaging PR addresses that environment issue; no spec 010 production change depends on it.
- A passing baseline establishes executable prior state only and is not independent analytical validation.

## Statistical Reference Conventions

- Official installed R 4.5.3 formals confirm `stats::quantile()` type 7 as the default used explicitly by the summary cores.
- `stats::var()` and `stats::sd()` use the sample variance convention for finite observations; expectations use direct hand calculations or the documented base-R method rather than the production summary path.
- `stats::shapiro.test()` is used only for finite samples within its documented size range; the package intentionally applies the stricter existing requirement of more than three values with non-zero variation.
- Installed `e1071` 1.7.17 documentation/formals confirm type 3 as the default for both skewness and kurtosis. Tests that assert these fields record that convention explicitly.

## Implementation Review

Implementation and self-review were completed on 2026-08-02.

### Contract and semantics

- The public EDA summary, workflow and report functions now expose one contract with no version selector. The result component order is fixed as `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped`; the old two-table builder and version adapters were removed.
- The variables table preserves specification order, includes required/optional context and reports unavailable counts as `NA` when a source variable is absent. Every fixture specification variable reconciled to either a typed component or an explicit skipped row.
- Numeric expectations were calculated independently for finite values `1`, `2` and `10`: sum `13`, mean `13 / 3`, sample variance `73 / 3`, quartiles `1.5` and `6`, IQR `4.5`, fences `-5.25` and `12.75`, and zero outliers. The production result matched each value while separately recording one infinity and two missing values (one sentinel and one `NA`).
- Empty, all-missing, sentinel-only and infinity-only numeric vectors each produced `n_finite = 0` and `sum = NA`, rather than an invented observed total of zero.
- A seven-row categorical fixture independently reconciled declared levels `A`, `B`, `C`, unexpected observed values `D` and literal `NA`, one sentinel `UNK` and one actual missing value. Counts, total-row denominators, observed-value denominators and declaration flags matched the hand-derived truth table. Unused factor metadata was not promoted to a declared result row.
- Text checks distinguished missing, empty and whitespace-only states. Date and datetime checks reconciled quartiles, ranges, units and timezone-normalised output. Invalid temporal values and incompatible classes produced explicit skipped reasons.
- Dictionary-derived specifications and the typed `epi_stats_summary()` path produced the same six-component contract. The released factor-table helper retains its existing factor-level behaviour by passing its levels explicitly to the shared core.

### Outputs and editorial review

- A realistic end-to-end invocation wrote the six summary CSV files plus metadata, schema and missingness artifacts to a temporary directory. The returned object and CSV values were inspected directly and reconciled.
- The report rendered successfully to HTML. Its source and rendered text were inspected for the six sections in contract order, explicit empty-section messages, finite-value treatment, categorical denominators and temporal units. No browser was available for an additional visual screenshot review; plot calculations were unchanged, and existing plot-layer sentinel tests passed.
- The source tarball was inspected directly. It contains the canonical R implementation, report template and refreshed Rd file, and excludes `future/`, archive and checklist material. It still contains the two inherited project-template `.gitkeep` files; their removal is isolated in PR #178.
- Roxygen, README, NEWS, the specification-first vignette and report prose use canonical terminology only. A static search found no active `summary_version`, v1/v2 interface or deleted test-name references in package code, tests or user documentation.

### Verification

- Relevant R and test files were styled. Package-loaded lint passed.
- Focused contract, workflow, report, dictionary, shared-core, numeric and fixture tests passed.
- The complete test suite passed with the two known environment skips (installed-package parallel plotting and disabled visual snapshots).
- `scripts/check-local.sh` completed with zero errors, zero warnings and the single inherited hidden-file NOTE fixed by PR #178.
- After installing TinyTeX, the required Courier metrics and `makeindex`, plus HTML Tidy in the local `episcout` environment, `scripts/check-cran.sh` completed with zero errors, zero warnings and two notes. Both PDF and HTML manual checks pass. The remaining notes are the existing CRAN-incoming diagnostics (new-submission status and two Stack Overflow `403` responses) and the two inherited `.gitkeep` files removed by PR #178; neither originates in the canonical-summary implementation.
- `git diff --check` passed.

## Closeout Notes

- This remains an active draft until the release-scrub dependency in PR #178 lands and the release-oriented check is repeated against the combined changes. The local document toolchain is now complete.
- Codecov integration and the separately discovered historical Codecov credential concern are explicitly deferred by the repository owner for later review. No Codecov configuration is added or changed here.
- This specification intentionally changes the released two-table return shape; the owner authorised the compatibility break and no legacy adapter remains.
- No tag or release was created.
