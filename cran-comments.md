## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

❯ checking top-level files ... NOTE
  Non-standard file/directory found at top level:
    ‘THANKS.rst’

❯ checking R code for possible problems ... NOTE
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
  epi_plot_heatmap_triangle: no visible global function definition for
    ‘element_text’
  epi_plot_heatmap_triangle: no visible global function definition for
    ‘coord_fixed’
  epi_plot_heatmap_triangle: no visible global function definition for
    ‘element_blank’
  epi_plot_heatmap_triangle: no visible global function definition for
    ‘element_rect’
  epi_plot_heatmap_triangle: no visible global function definition for
    ‘guide_colorbar’
  epi_stats_summary: no visible global function definition for ‘%>%’
  epi_stats_tidy: no visible global function definition for ‘%>%’
  epi_stats_tidy: no visible binding for global variable ‘.’
  epi_stats_tidy: no visible binding for global variable ‘x’
  epi_stats_tidy: no visible binding for global variable ‘n’
  epi_stats_tidy: no visible binding for global variable ‘id’
  Undefined global functions or variables:
    %>% . Var1 Var2 coord_fixed element_blank element_rect element_text
    guide_colorbar id n value x


## Downstream dependencies

There are no dependencies other than base R. Several suggested packages, I have not tested these directly however.
