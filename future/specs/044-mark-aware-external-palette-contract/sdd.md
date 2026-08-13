# Mark-Aware External Palette Contract

Spec ID: `044-mark-aware-external-palette-contract`
Status: Active

## Public Contract

`epi_plot_bar()` and the grouped (`var_x` supplied) path of `epi_plot_box()` receive a new nullable `fill_values` argument. It is explicitly a discrete mapping for the marks' interior fill, not a generic colour argument. The one-variable box-plot path retains its existing scalar `fill` argument because it does not map categories to a scale.

`fill_values` is either `NULL`, an unnamed character vector, or a named character vector. Every supplied value must be a non-missing colour accepted by `grDevices::col2rgb()`. A named mapping is exact: names must be non-empty and unique, must cover every displayed categorical level, and must not contain an extra category. An unnamed vector retains positional compatibility only when it contains at least one value per displayed level, in the declared factor-level order followed by observed character levels. It is never recycled.

`custom_palette` remains accepted by `epi_plot_bar()` for one compatible release cycle. When `fill_values` is `NULL`, it is interpreted as the old positional fill vector and emits a lifecycle deprecation warning. Supplying both arguments is an error. Its documented short-vector recycling remains intact solely for released compatibility; new `fill_values` mappings never recycle. The historical positional result is unchanged.

The first slice intentionally does not add `colour_values`. The inventory found no current public helper with an existing categorical colour-scale input that can be extended without separately deciding its line/point/outline contract. The implementation successor must add such an input only with a concrete helper-specific proposal; issue-311's generic plot hook can apply caller-owned colour scales after this fill contract is available.

## Mark Inventory And Scope

| Helper and path | Actual categorical mark aesthetic | Decision |
| --- | --- | --- |
| `epi_plot_bar()` | `geom_bar()` maps `fill`; `bar_colour` is a constant outline. | Add `fill_values`; retain `custom_palette` compatibility. |
| `epi_plot_box(var_x = ...)` | `geom_boxplot()` maps `fill`; jitter and summary point have constant defaults. | Add `fill_values`. |
| `epi_plot_dates()`, `epi_plot_km()`, `epi_plot_parallel()` | Lines and points are not currently supplied a categorical colour mapping through a palette interface. | Out of scope pending a helper-specific contract. |
| Heatmaps and triangles | Tiles use continuous/derived fills, not this discrete category contract. | Out of scope. |

## Rendering Behaviour

For a valid mapping, the helper adds `ggplot2::scale_fill_manual(values = fill_values, limits = levels)` where `levels` is the validated displayed category order. Named mappings preserve name-to-category correspondence. Positional mappings are converted to that same named mapping before adding the scale, making the rendered result deterministic. `NULL` leaves the current ggplot2/default-scale output unchanged.

Validation derives its displayed levels from the plotted fill column before plot construction. Missing values remain missing and use ggplot2's ordinary missing-value display; they are not assigned a fabricated category or colour. Existing data are neither reordered nor mutated.

## Compatibility And Institutional Exports

`palette_IMSS` and `palette_IMSS_accessible` remain exported unchanged in this non-breaking slice. Their documentation is amended to say that they are legacy institutional vectors rather than a default or required palette contract. No maintained example uses either export. A later deprecation/removal decision requires release and downstream-consumer evidence plus NEWS.

## Errors

Errors identify the helper, argument, and affected categories without printing source data. They cover non-character/non-vector inputs, invalid colours, duplicate or blank names, insufficient positional values, and missing or extra names. An explicit mapping never falls back to the default palette.

## Dependencies

No dependency is added. `ggplot2` and base `grDevices` provide the established rendering and colour-validation boundaries.
