# Acceptance

Spec ID: `029-eda-reviewed-coordinate-roles`
Status: Review

- [x] Phase-B implementation and closeout are canonical and every required check passed.
- [x] Coordinate roles remain optional reviewed metadata, not an inferred eighth EDA type.
- [x] Exact pair validation, CRS resolution and scaffold non-inference are specified before package-code changes.
- [x] Aggregate fields, missing/non-finite/range semantics and stable eligibility reasons are defined.
- [x] Data-frame and PostgreSQL paths share one result contract; PostgreSQL remains aggregate-only and does not require PostGIS.
- [x] Ordinary return/bundle integration and byte-level coordinate/geometry absence tests are designed.
- [x] Scientific meaning, privacy approval, geometry construction, automatic discovery and spatial inference remain explicitly out of scope.
- [x] Planning PR #240 is green before the stacked implementation begins.
- [x] Executable tests fail for the intended missing behaviour before implementation and pass afterward.
- [x] Focused, live PostgreSQL, lint, local, CRAN-oriented and upstream checks are recorded.
- [x] User documentation and generated help describe the additive contract and safety boundary.
- [ ] Implementation PR is merged to canonical `master`; issue #237 and roadmap #227 are reconciled.
- [ ] The terminal outcome is recorded, the manifest is completed and spec 029 is archived under `future/specs/done/`.
