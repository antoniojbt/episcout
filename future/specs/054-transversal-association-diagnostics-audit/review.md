# Review

Spec ID: `054-transversal-association-diagnostics-audit`
Status: Active

## Independent Behaviour Evidence

The following checks were performed with repository entry points and a disposable PostgreSQL 17 service. They use small authored values rather than Oferta output as expected truth.

### Tied-rank Spearman

For complete pairs `x = (1, 1, 2, 3, 4)` and `y = (5, 4, 4, 2, 1)`, average ranks are:

```text
rank(x) = (1.5, 1.5, 3, 4, 5)
rank(y) = (5, 3.5, 3.5, 2, 1)
```

Pearson correlation of those ranks is `-0.921052631578948` with `n = 5`. Base R, `epi_stats_corr()` and the Oferta PostgreSQL mid-rank expression agreed to floating-point tolerance. A constant ranked variable returns an unavailable/`NA` coefficient. PostgreSQL documentation confirms that `corr()` counts rows with both expressions non-null.

The audit also demonstrated a current semantic mismatch that the successor must resolve: Oferta's SQL excludes only `NULL`, so a positive infinity remained eligible and changed an authored three-row coefficient to `0.5`; canonical Episcout numeric summaries exclude non-finite observations, while `Hmisc::rcorr()` rejects infinity. The successor therefore excludes non-finite values explicitly and reports the eligible pair count.

### Cross-tab equivalence

For an authored six-row PostgreSQL table with two ordinary grouping levels, one missing grouping value, two ordinary compared levels and one missing compared value, direct grouped SQL and `epi_eda_profile_stratified(..., include_overall = FALSE, include_missing_stratum = FALSE)` agreed exactly on:

```text
group value, compared level, missing-level flag, cell n, group n and p_total
```

The comparison returned four observed cells across two groups. The current focused PostgreSQL stratification suite also passed all 63 assertions, including data-frame parity and declared/missing/unexpected level behaviour. This establishes that another cross-tab query is unnecessary. It does not make the existing tests independent statistical validation of every categorical rule.

### Cramér's V

Two hand-authored aggregate tables anchored the proposed formula:

| Shape | Counts by row | n | Pearson chi-square | Cramér's V |
| --- | --- | ---: | ---: | ---: |
| 2×2 | `(30, 10)`; `(10, 50)` | 100 | 34.02777777777778 | 0.5833333333333334 |
| 3×2 | `(20, 10)`; `(0, 10)`; `(20, 10)` | 70 | 15.55555555555556 | 0.4714045207910317 |

Appending a declared zero-total column to the 2×2 table retained `V = 0.5833333333333334` after removing inactive margins. This check exposes why the aggregate implementation must not include canonical declared zero levels in the active table dimension.

## Checklist Review

### Software verification

- Observable outcome and source ownership are explicit.
- Released interfaces remain unchanged in this design issue.
- The successor test design covers inputs, failure behaviour, typed output, PostgreSQL side effects, compatibility and realistic invocation.
- No dependency is proposed for calculations available from base R and PostgreSQL primitives.

### Truth and semantics

- Statistical definitions are linked to R, PostgreSQL and NIST documentation.
- Missingness, declared codes, non-finite values, denominators, zero margins and degenerate states are explicit.
- Expected values were independently authored and not generated from Oferta output.
- This is a self-review supported by independent calculations, not independent review by another person.

### Figures

- The audit traces each plot to its aggregate data and selection logic.
- No figure is created or changed, so render, accessibility and visual inspection checks are not applicable to this contribution.

## Limitations

- The audit does not claim that existing historical `epi_stats_*` tests fully validate inferential statistics; their broader truth gaps remain separately documented.
- No real or non-repository Oferta data were inspected.
- Successor names, roxygen wording and exact R column types will be fixed in the successor's pre-code specification; issue 358 fixes the behaviour and ownership boundary only.
