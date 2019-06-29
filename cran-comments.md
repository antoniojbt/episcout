## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci) and macOS for R 3.4, 3.5, release, devel

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

  epi_clean_count_classes: no visible global function definition for
    ‘%>%’
  epi_clean_count_classes: no visible binding for global variable ‘.’
  epi_plot_heatmap: no visible binding for global variable ‘Var1’
  epi_plot_heatmap: no visible binding for global variable ‘Var2’
  epi_plot_heatmap: no visible binding for global variable ‘value’
  epi_plot_heatmap_triangle: no visible binding for global variable
    ‘Var1’
  epi_plot_heatmap_triangle: no visible binding for global variable
    ‘Var2’
  epi_plot_heatmap_triangle: no visible binding for global variable
    ‘value’
  epi_stats_summary: no visible global function definition for ‘%>%’
  epi_stats_tidy: no visible global function definition for ‘%>%’
  epi_stats_tidy: no visible binding for global variable ‘.’
  epi_stats_tidy: no visible binding for global variable ‘x’
  epi_stats_tidy: no visible binding for global variable ‘n’
  epi_stats_tidy: no visible binding for global variable ‘id’
  Undefined global functions or variables:
    %>% . Var1 Var2 id n value x

0 errors ✔ | 0 warnings ✔ | 1 note ✖


## Downstream dependencies

There are no dependencies other than base R. Several suggested packages, I have not tested these directly however.
