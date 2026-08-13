# Acceptance evidence

- [ ] `plot_style` receives a completed ggplot and compact non-row context, and returns one ggplot.
- [ ] Data-frame and `epi_eda_run()` styling does not change analytical summaries.
- [ ] PostgreSQL styling occurs after the repeatable-read snapshot and before staged SVG publication.
- [ ] Styled database bundles record and validate `plot_style_id`; defaults retain prior metadata compatibility.
- [ ] Disabled plots do not invoke styling and callback failures publish no partial bundle.
- [ ] Focused, PostgreSQL, local and CRAN checks pass.
