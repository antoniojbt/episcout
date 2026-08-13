# Software design description

## Contract

`plot_style` is `NULL` or a function of `(plot, context)` that returns one ggplot. Context contains only variable and plot descriptors plus aggregate counts. Invalid callbacks, callback errors and non-ggplot returns stop with fixed actionable errors.

`epi_eda_profile_plots()` and `epi_eda_run()` forward the callback without changing their default behaviour. `epi_eda_db_run()` accepts the callback and `plot_style_id` as final compatibility-preserving arguments. When database plots are enabled, a callback requires one non-empty non-secret identifier. The identifier is written only for styled bundles and is part of overwrite identity.

## Execution order

Database aggregate profiling and compact plot preparation finish inside the existing read-only repeatable-read transaction. Rendering and style application occur after that snapshot closes. SVGs are written only into the existing sibling staging directory; any error removes staging and does not publish a partial root.

## Compatibility

Without a callback, public output, default plot construction and legacy database metadata remain unchanged. The callable itself is not fingerprinted or persisted; `plot_style_id` is the caller-owned provenance claim.
