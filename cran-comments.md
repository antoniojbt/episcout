## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

Full log:

Updating episcout documentation
Writing NAMESPACE
Loading episcout
Writing NAMESPACE
── Building ──────────────────────────────────────────── episcout ──
Setting env vars:
● CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
● CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
● CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
────────────────────────────────────────────────────────────────────
Building the package will delete...
  '/Users/antoniob/Documents/github.dir/AntonioJBT/episcout/inst/doc'
Are you sure?

1: Yes
2: No

Selection: Yes
✔  checking for file ‘/Users/antoniob/Documents/github.dir/AntonioJBT/episcout/DESCRIPTION’ ...
─  preparing ‘episcout’: (37.6s)
✔  checking DESCRIPTION meta-information
─  installing the package to build vignettes
✔  creating vignettes (3.8s)
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
   Removed empty directory ‘episcout/inst/extdata’
─  building ‘episcout_0.1.1.tar.gz’
   
── Checking ──────────────────────────────────────────── episcout ──
Setting env vars:
● _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
● _R_CHECK_CRAN_INCOMING_       : FALSE
● _R_CHECK_FORCE_SUGGESTS_      : FALSE
── R CMD check ─────────────────────────────────────────────────────
   Good day Antonio!
─  using log directory ‘/private/var/folders/tw/wx9dgmp14sz3p1kl3_nn_rlc0000gn/T/RtmpuHstV0/episcout.Rcheck’
─  using R version 3.5.1 (2018-07-02)
─  using platform: x86_64-apple-darwin15.6.0 (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✔  checking for file ‘episcout/DESCRIPTION’
─  this is package ‘episcout’ version ‘0.1.1’
─  package encoding: UTF-8
✔  checking package namespace information ...
✔  checking package dependencies (733ms)
✔  checking if this is a source package ...
✔  checking if there is a namespace
✔  checking for executable files (1.1s)
✔  checking for hidden files and directories
✔  checking for portable file names ...
✔  checking for sufficient/correct file permissions
✔  checking serialization versions
✔  checking whether package ‘episcout’ can be installed (1.8s)
✔  checking installed package size ...
✔  checking package directory ...
✔  checking ‘build’ directory
✔  checking DESCRIPTION meta-information ...
N  checking top-level files
   Non-standard file/directory found at top level:
     ‘THANKS.rst’
✔  checking for left-over files
✔  checking index information ...
✔  checking package subdirectories ...
✔  checking R files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded ...
✔  checking whether the package can be loaded with stated dependencies ...
✔  checking whether the package can be unloaded cleanly ...
✔  checking whether the namespace can be loaded with stated dependencies ...
✔  checking whether the namespace can be unloaded cleanly ...
✔  checking dependencies in R code (1.8s)
✔  checking S3 generic/method consistency (650ms)
✔  checking replacement functions ...
✔  checking foreign function calls ...
N  checking R code for possible problems (2.8s)
   epi_clean_count_classes: no visible global function definition for
     ‘%>%’
   epi_clean_count_classes: no visible binding for global variable ‘.’
   epi_plot_en_masse: no visible global function definition for ‘%>%’
   epi_plot_en_masse: no visible global function definition for ‘%dopar%’
   epi_plot_en_masse: no visible binding for global variable ‘i’
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
   epi_plot_theme_1: no visible global function definition for
     ‘element_text’
   epi_plot_theme_1: no visible global function definition for
     ‘theme_classic’
   epi_plot_theme_1: no visible global function definition for ‘theme’
   epi_plot_theme_1: no visible global function definition for
     ‘element_blank’
   epi_plot_theme_1: no visible global function definition for ‘unit’
   epi_plot_theme_2: no visible global function definition for
     ‘theme_foundation’
   epi_plot_theme_2: no visible global function definition for ‘theme’
   epi_plot_theme_2: no visible global function definition for
     ‘element_text’
   epi_plot_theme_2: no visible global function definition for ‘rel’
   epi_plot_theme_2: no visible global function definition for
     ‘element_rect’
   epi_plot_theme_2: no visible global function definition for
     ‘element_line’
   epi_plot_theme_2: no visible global function definition for
     ‘element_blank’
   epi_plot_theme_2: no visible global function definition for ‘unit’
   epi_stats_summary: no visible global function definition for ‘%>%’
   epi_stats_tidy: no visible global function definition for ‘%>%’
   epi_stats_tidy: no visible binding for global variable ‘.’
   epi_stats_tidy: no visible binding for global variable ‘x’
   epi_stats_tidy: no visible binding for global variable ‘n’
   epi_stats_tidy: no visible binding for global variable ‘id’
   scale_colour_epi_plot_theme_2: no visible global function definition
     for ‘discrete_scale’
   scale_colour_epi_plot_theme_2: no visible global function definition
     for ‘manual_pal’
   scale_fill_epi_plot_theme_2: no visible global function definition for
     ‘discrete_scale’
   scale_fill_epi_plot_theme_2: no visible global function definition for
     ‘manual_pal’
   Undefined global functions or variables:
     %>% %dopar% . Var1 Var2 coord_fixed discrete_scale element_blank
     element_line element_rect element_text guide_colorbar i id manual_pal
     n rel theme theme_classic theme_foundation unit value x
✔  checking Rd files ...
✔  checking Rd metadata ...
✔  checking Rd line widths ...
✔  checking Rd cross-references (1.5s)
✔  checking for missing documentation entries ...
✔  checking for code/documentation mismatches (452ms)
✔  checking Rd \usage sections (895ms)
✔  checking Rd contents ...
✔  checking for unstated dependencies in examples ...
✔  checking installed files from ‘inst/doc’ ...
✔  checking files in ‘vignettes’ ...
✔  checking examples (2.2s)
✔  checking for unstated dependencies in ‘tests’ ...
─  checking tests ...
✔  Running ‘testthat.R’ (5.2s)
✔  checking for unstated dependencies in vignettes (5.4s)
✔  checking package vignettes in ‘inst/doc’
✔  checking re-building of vignette outputs (1.2s)
   
   See
     ‘/private/var/folders/tw/wx9dgmp14sz3p1kl3_nn_rlc0000gn/T/RtmpuHstV0/episcout.Rcheck/00check.log’
   for details.
   
   
── R CMD check results ───────────────────────── episcout 0.1.1 ────
Duration: 24.5s

❯ checking top-level files ... NOTE
  Non-standard file/directory found at top level:
    ‘THANKS.rst’

❯ checking R code for possible problems ... NOTE
  epi_clean_count_classes: no visible global function definition for
    ‘%>%’
  epi_clean_count_classes: no visible binding for global variable ‘.’
  epi_plot_en_masse: no visible global function definition for ‘%>%’
  epi_plot_en_masse: no visible global function definition for ‘%dopar%’
  epi_plot_en_masse: no visible binding for global variable ‘i’
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
  epi_plot_theme_1: no visible global function definition for
    ‘element_text’
  epi_plot_theme_1: no visible global function definition for
    ‘theme_classic’
  epi_plot_theme_1: no visible global function definition for ‘theme’
  epi_plot_theme_1: no visible global function definition for
    ‘element_blank’
  epi_plot_theme_1: no visible global function definition for ‘unit’
  epi_plot_theme_2: no visible global function definition for
    ‘theme_foundation’
  epi_plot_theme_2: no visible global function definition for ‘theme’
  epi_plot_theme_2: no visible global function definition for
    ‘element_text’
  epi_plot_theme_2: no visible global function definition for ‘rel’
  epi_plot_theme_2: no visible global function definition for
    ‘element_rect’
  epi_plot_theme_2: no visible global function definition for
    ‘element_line’
  epi_plot_theme_2: no visible global function definition for
    ‘element_blank’
  epi_plot_theme_2: no visible global function definition for ‘unit’
  epi_stats_summary: no visible global function definition for ‘%>%’
  epi_stats_tidy: no visible global function definition for ‘%>%’
  epi_stats_tidy: no visible binding for global variable ‘.’
  epi_stats_tidy: no visible binding for global variable ‘x’
  epi_stats_tidy: no visible binding for global variable ‘n’
  epi_stats_tidy: no visible binding for global variable ‘id’
  scale_colour_epi_plot_theme_2: no visible global function definition
    for ‘discrete_scale’
  scale_colour_epi_plot_theme_2: no visible global function definition
    for ‘manual_pal’
  scale_fill_epi_plot_theme_2: no visible global function definition for
    ‘discrete_scale’
  scale_fill_epi_plot_theme_2: no visible global function definition for
    ‘manual_pal’
  Undefined global functions or variables:
    %>% %dopar% . Var1 Var2 coord_fixed discrete_scale element_blank
    element_line element_rect element_text guide_colorbar i id manual_pal
    n rel theme theme_classic theme_foundation unit value x

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

## Downstream dependencies
#I have also run R CMD check on downstream dependencies of:
#package
#(webpage) 

#All packages that I could install passed except:

* 
