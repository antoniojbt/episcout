# Visual regression contract

The maintained first visual slice covers the public `epi_plot_hist()`, `epi_plot_box()`, `epi_plot_bar()`, `epi_plot_heatmap()`, `epi_plot_heatmap_triangle()`, `epi_plot_list()`/`epi_plots_to_grid()` and compact categorical `epi_eda_profile_plots()` surfaces exercised by their adjacent testthat cases. The former generic ggplot2 and base-graphics demonstration cases and the duplicate unmodified box-colour case are retired because they do not exercise a distinct Episcout rendering contract; their snapshots were removed with those cases.

Object and data assertions beside each visual expectation protect consequential inputs, plot classes, labels, limits, ordering, denominators and layer structure before SVG comparison. The SVGs detect renderer-visible regressions only. Snapshot agreement does not independently validate analytical meaning, accessibility or the correctness of those assertions.

The required hosted execution platform is the Ubuntu `R-CMD-check` job in `.github/workflows/r-cmd-check.yml`, with DejaVu fonts installed explicitly. The repository wrapper executes the same cases locally. `scripts/check-local.sh` performs documentation in a temporary copy and fails if any test or check changes the checkout, so accepted snapshots are never silently rewritten or deleted.

Every SVG under `tests/testthat/_snaps/` must map to an executing `vdiffr::expect_doppelganger()` call. A maintainer reviewing a plot change must inspect the object/data assertions and rendered difference before accepting a new baseline. Retire an obsolete snapshot only in the same change that retires its test case; never regenerate the directory wholesale.
