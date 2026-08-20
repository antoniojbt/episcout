# Software design

Spec ID: 052
Status: Completed

## Scope and invariants

V1 links two caller-supplied data frames. It never mutates them, persists a result, enforces a one-to-one assignment or calls the existing registry. All roles are declared by exact column name. A result describes candidate-pair evidence under a caller-declared model; it does not establish identity.

Every public error, warning, print and summary is value-free. Objects may contain sensitive derived values needed for computation, but their print methods expose only schemas and aggregate counts. Pair outputs use one-based `x_index` and `y_index` by default. Source record keys and derived/source field values appear only when the caller sets `include_values = TRUE`.

## Public API

### Declaration

`epi_linkage_text_profile(unicode = "NFC", case = "fold", diacritics = "keep", punctuation = "space", whitespace = "collapse", token_order = "preserve", drop_tokens = character())` returns a validated declarative normaliser. Supported Unicode forms are `NFC` and `NFKC`; case is `keep` or `fold`; diacritics are `keep` or `strip`; punctuation is `keep`, `space` or `drop`; whitespace is `keep`, `trim` or `collapse`; token order is `preserve` or `sort`. `drop_tokens` is an explicit unique character vector and is normalised by the same profile before exact token removal. No default drops particles or any other token.

`epi_linkage_spec(x_id, y_id, profiles, blocks, comparisons, max_candidates, model = NULL, thresholds = NULL)` returns class `c("epi_linkage_spec", "list")` after all-or-nothing validation.

- `x_id` and `y_id` are exact source column names used only when value-bearing output is requested.
- `profiles` is a named list of `epi_linkage_text_profile` objects; reserved profile `identity` performs no character transformation.
- `blocks` has exact columns `pass`, `x_field`, `y_field`, `profile`. Rows sharing `pass` are AND keys; distinct positive integer passes are OR alternatives. At least one row is required.
- `comparisons` has exact columns `comparison`, `x_field`, `y_field`, `profile`, `method`, `parameter`. Names are unique and caller ordered. Methods are `exact`, `jaro_winkler`, `token_jaccard`, `numeric_tolerance` and `date_tolerance`. `parameter` is `1` for exact, an agreement threshold in `[0,1]` for string similarities, a non-negative finite absolute tolerance for numeric values, and a non-negative whole-day tolerance for dates.
- `max_candidates` is one explicit positive whole number no larger than 10,000,000. It is a hard failure bound, never a truncation target.
- Optional `model` is a list with exactly `parameters` and `match_prevalence`. `parameters` has exact columns `comparison`, `m_probability`, `u_probability`, covers every comparison once in comparison order, and requires finite `0 < u_probability < m_probability < 1`. `match_prevalence` is one scalar strictly between zero and one.
- Optional `thresholds` contains `metric = "model_posterior"`, finite `nonmatch_max` and `match_min` in `[0,1]`, with `nonmatch_max < match_min`. No thresholds are supplied by the package.

The implementation may use small constructor helpers for the model and thresholds if they preserve these exact semantics. A foundation-only spec with `model = NULL` and `thresholds = NULL` is valid for issue #361; scoring requires both.

### Composable execution

`epi_linkage_prepare(x, y, spec)` validates ordinary data frames, unique/non-missing/non-blank record IDs, declared columns and supported classes, then returns class `epi_linkage_prepared`. Original objects remain byte-for-byte unchanged. Derived values are stored separately with stable internal names. Character `NA` remains missing; an empty result after declared normalisation becomes missing rather than an agreement token.

`epi_linkage_candidates(prepared)` applies each exact blocking pass, excludes a pair from a pass when any block component is missing on either side, unions passes and removes duplicate pairs. Candidate order is ascending `x_index`, then `y_index`. It fails before returning a partial object when the cumulative unique count exceeds `max_candidates`. It returns an `epi_linkage_candidates` object with pair indices and diagnostics.

`epi_linkage_compare(candidates, include_values = FALSE)` returns `epi_linkage_comparisons`. Its long evidence table has one row per candidate and declared comparison with columns `x_index`, `y_index`, `comparison`, `similarity`, `comparison_state`. `comparison_state` is exactly `agree`, `disagree` or `missing`; either-side missingness, including both sides missing, is `missing` and `similarity = NA_real_`. Value opt-in adds `x_id`, `y_id`, `x_value` and `y_value` without changing the evidence semantics.

`epi_linkage_score(comparisons)` requires a complete declared model and returns `epi_linkage_scores`. It adds `weight_contribution` to each field-evidence row and adds one pair row with `x_index`, `y_index`, `n_agree`, `n_disagree`, `n_missing`, `linkage_weight`, `model_posterior`. Value opt-in is inherited, never silently added.

`epi_linkage_classify(scores)` requires declared thresholds and returns `epi_linkage_result`. `model_posterior <= nonmatch_max` is `non_match`; `model_posterior >= match_min` is `match`; the open interval between them is `review`. Exact boundary behaviour is intentional.

`epi_linkage_run(x, y, spec, include_values = FALSE)` is a thin deterministic composition of prepare, candidates, compare, score and classify. It adds no alternate calculation.

`epi_linkage_validate(result, truth)` accepts a complete synthetic or otherwise independently established truth table with exact columns `x_index`, `y_index`, `is_match`. It must contain every Cartesian pair exactly once and no other rows. V1 deliberately requires complete truth so non-candidate decisions are known rather than assumed. The returned validation object is aggregate-only and never retains truth values or source fields.

## Normalisation semantics

Text conversion is ordered: Unicode normalisation; case folding; optional canonical decomposition, mark removal and recomposition for diacritic stripping; punctuation handling; whitespace handling; tokenisation on one or more Unicode whitespace characters; exact declared token removal; optional lexicographic token sorting; one-space joining. This creates comparison representations only.

The worked Mexican-name configuration explicitly chooses accent stripping and token sorting for selected comparison representations. It does not label tokens paternal, maternal, married or compound, and it never silently removes `de`, `del`, `la` or another particle. Alternative source orderings are represented by a declared profile or separate comparison, not inferred.

## Candidate diagnostics

`diagnostics$overall` has exact columns `n_x`, `n_y`, `n_possible`, `n_candidates`, `reduction_n`, `reduction_ratio`. Counts are exact base-R doubles and `n_possible = n_x * n_y`; inputs whose product exceeds `2^53 - 1` fail. `reduction_n = n_possible - n_candidates`; the ratio is `reduction_n / n_possible`, with typed `NA_real_` when there are zero possible pairs.

`diagnostics$passes` has `pass`, `n_candidates_before_union`, `n_new_candidates`, `n_duplicate_candidates`. Counts reconcile to the overall union. Zero-row sources with declared columns succeed with zero candidates; candidate comparison, score and decisions retain typed zero-row schemas.

## Comparison semantics

- `exact`: similarity is `1` for exact equality and `0` otherwise.
- `jaro_winkler`: similarity is `stringdist::stringsim(..., method = "jw")`; agreement is similarity greater than or equal to the caller threshold.
- `token_jaccard`: similarity is unique-token intersection size divided by union size; agreement uses the caller threshold. A missing normalised side is missing, not an empty set.
- `numeric_tolerance`: both sides must be undecorated finite numeric/integer values; similarity is binary membership in the declared absolute tolerance.
- `date_tolerance`: both sides must inherit `Date`; similarity is binary membership in the declared whole-day tolerance.

NaN, infinities, unsupported classes, recycling and locale-dependent coercion are rejected. Every comparison is vectorised pairwise without randomness.

## Fellegi-Sunter model

For observable comparison `j`, agreement contributes `log2(m_j / u_j)` and disagreement contributes `log2((1 - m_j) / (1 - u_j))`. Missing comparisons contribute zero under an explicit ignorable pairwise-missingness assumption. The pair `linkage_weight` is the sum of field contributions.

With declared prior match prevalence `p`, `model_posterior` is calculated stably from prior log odds plus the natural-log equivalent of the linkage weight. The result is labelled `model_posterior`, never `calibrated_probability`. Metadata fixes `probability_semantics = "fellegi_sunter_model_posterior_not_empirically_calibrated"`, identifies the conditional-independence and ignorable-missingness assumptions, and records every declared parameter. If all fields are missing, the weight is zero and the posterior equals the declared prevalence.

## Validation contract

Every possible pair receives an effective decision: returned candidate decision or implicit `non_match` outside the candidate set. Validation reports exact counts and explicit denominators for:

- candidate recall: true matches retained / all true matches;
- precision: true matches classified `match` / all pairs classified `match`;
- recall: true matches classified `match` / all true matches;
- false matches and false-match proportion among classified matches;
- missed matches: true matches classified `non_match`, including blocking misses;
- true matches in review;
- manual-review burden among candidates and among all possible pairs.

Zero denominators yield typed `NA_real_`, never zero. Counts reconcile to complete truth and decisions. Synthetic fixture expectations are hand-authored independently of production functions.

## Result and privacy boundary

The fixed `epi_linkage_result` components are `metadata`, `candidate_diagnostics`, `pair_scores`, `field_evidence`, `decisions`. Routine print/summary shows only contract version, source row counts, candidate count, reduction ratio and decision counts. It never prints IDs, fields, values, comparison parameters tied to field names, or example pairs.

`include_values = TRUE` is an explicit value-bearing diagnostic action. It adds declared record IDs and derived values but never source columns not used by the workflow. The vignette warns that these remain sensitive and must not enter logs, screenshots, issue bodies or ordinary reports.

No function creates or writes a crosswalk. A caller may separately review accepted matches and construct an approved exact crosswalk outside this API; only that reviewed artefact may enter the existing exact `epi_sec_*` workflow.

## Dependencies and compatibility

Promote existing suggested dependency `stringi` to Imports and add `stringdist` to Imports. No whole-pipeline linkage package is added. The decision keeps the public data-frame API stable and the statistical formula inspectable while avoiding a dependency-specific S4/by-reference result contract. Existing exact identity and pseudonymisation interfaces remain unchanged.
