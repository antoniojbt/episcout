# episcout

episcout is an R package providing helper functions for cleaning, exploring and visualizing large epidemiological datasets. It wraps common preprocessing and descriptive tasks in the tidyverse and data.table ecosystems for fast and flexible data manipulation.

**Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.**

## Working Effectively

### Bootstrap and Setup
- Install R and development tools:
  ```bash
  sudo apt-get update
  sudo apt-get install -y r-base r-base-dev build-essential
  ```
  Time: 5-10 minutes. NEVER CANCEL this installation.

- Install core R development packages:
  ```bash
  Rscript -e "install.packages(c('remotes', 'devtools', 'roxygen2', 'testthat', 'covr', 'lintr', 'styler'), repos='https://cran.r-project.org')"
  ```
  Time: 5-15 minutes depending on network. NEVER CANCEL. Set timeout to 30+ minutes.

- Install package dependencies:
  ```bash
  Rscript -e "remotes::install_deps(dependencies = TRUE)"
  ```
  Time: 10-30 minutes for all tidyverse dependencies. NEVER CANCEL. Set timeout to 45+ minutes.

### Build and Check
- Build the package:
  ```bash
  R CMD build . --no-manual --no-resave-data --compact-vignettes=gs+qpdf
  ```
  Time: ~1 second (very fast). Creates `episcout_X.X.X.tar.gz`.

- Run CRAN-like checks:
  ```bash
  R CMD check --no-manual --as-cran episcout_*.tar.gz
  ```
  Time: 3-5 minutes. NEVER CANCEL. Set timeout to 15+ minutes.

- Alternative build without vignettes (if knitr unavailable):
  ```bash
  R CMD build . --no-manual --no-resave-data --no-build-vignettes
  ```

### Testing
- Run all tests:
  ```bash
  Rscript -e "devtools::test(reporter = 'summary')"
  ```
  Time: 1-3 minutes. NEVER CANCEL. Set timeout to 10+ minutes.

- Run test coverage:
  ```bash
  Rscript -e "covr::report()"
  ```
  Time: 2-5 minutes. NEVER CANCEL. Set timeout to 15+ minutes.

### Documentation and Code Quality
- Regenerate documentation:
  ```bash
  Rscript -e "devtools::document()"
  ```
  Time: ~10 seconds. Updates man/ files from roxygen2 comments.

- Style code:
  ```bash
  Rscript -e "styler::style_pkg()"
  ```
  Time: ~5-10 seconds.

- Lint code:
  ```bash
  Rscript -e "lintr::lint_package()"
  ```
  Time: ~10-30 seconds.

## Validation

- **ALWAYS run these validation steps before committing changes:**
  1. `Rscript -e "devtools::document()"` - Update documentation
  2. `Rscript -e "styler::style_pkg()"` - Format code 
  3. `Rscript -e "lintr::lint_package()"` - Check code style
  4. `Rscript -e "devtools::test()"` - Run tests
  5. `R CMD build . --no-manual --no-resave-data --compact-vignettes=gs+qpdf` - Build package
  6. `R CMD check --no-manual --as-cran episcout_*.tar.gz` - Check package

- **ALWAYS test functionality manually** when making changes to core functions. Load the package in R and test examples from the function documentation.

- The CI (.github/workflows/r-cmd-check.yml) will fail if linting or tests fail, so always run local validation first.

## Package Structure and Navigation

### Function Organization
Functions are organized by purpose with consistent prefixes:
- `epi_clean_*` - Data wrangling and cleanup (20 functions)
- `epi_stats_*` - Descriptive statistics and contingency tables (15 functions)  
- `epi_plot_*` - Plotting wrappers around ggplot2 and cowplot (13 functions)
- `epi_utils_*` - Utilities like parallel processing and logging (3 functions)
- Miscellaneous helpers: `epi_read`, `epi_write`, etc.

### Key Directories
- `R/` - Source code (68 R files)
- `tests/testthat/` - Test suite using testthat framework
- `man/` - Generated documentation
- `vignettes/` - Package vignettes
- `.github/workflows/` - CI/CD configuration

### Coding Conventions
- Use `snake_case` for object and function names
- Use 2-space indentation, no tabs
- Include roxygen2 documentation for every exported function:
  ```r
  #' Title of the function
  #'
  #' Description of what it does.
  #'
  #' @param arg1 Description of arg1
  #' @return Description of return value
  #' @export
  my_function <- function(arg1) {
    # main logic
  }
  ```
- Avoid `<<-`, `assign()`, and hidden state unless strictly necessary

### Dependencies
- **Required imports** (in DESCRIPTION): utils, stats, grDevices, magrittr, rlang, broom, dplyr, tibble, tidyr, e1071, purrr, stringr
- **Suggested packages**: data.table, compare, stringi, lubridate, Hmisc, ggplot2, cowplot, testthat, etc.
- All dependencies must be declared in DESCRIPTION file

## Common Tasks

### Adding New Functions
1. Create new R file in `R/` directory following naming convention
2. Include complete roxygen2 documentation with `@export` tag
3. Add unit tests in `tests/testthat/test-[function-area].R`
4. Run `devtools::document()` to update NAMESPACE and man files
5. Test manually in R console
6. Run full validation sequence

### Modifying Existing Functions
1. Update the function code
2. Update roxygen2 documentation if needed
3. Update or add tests as appropriate
4. Run validation sequence
5. Test functionality manually

### Working with Data
- Package includes helper functions for reading/writing: `epi_read()`, `epi_write()`
- Test data available in `tests/testthat/df.tsv`
- Functions work with tidyverse (tibbles/data.frames) and data.table objects

## Troubleshooting

### Build Issues
- If vignette builder 'knitr' not found: Use `--no-build-vignettes` flag
- If dependencies missing: Install with `remotes::install_deps(dependencies = TRUE)`
- If network issues: Try different CRAN mirror in install.packages()

### Test Issues  
- If tests fail on specific platforms: Use `skip_if_not_installed()` in tests
- If dependencies missing for tests: Add conditional loading in test files
- Check test files use `library(episcout)` and `library(testthat)`

### Package Structure Issues
- Namespace issues: Run `devtools::document()` to regenerate NAMESPACE
- Missing documentation: Ensure all exported functions have `#' @export`
- DESCRIPTION validation: Required fields are Author, Maintainer (use Authors@R instead)

## File Inventory

### Repository Root
```
.
├── .github/workflows/     # CI/CD GitHub Actions
├── R/                     # Source code (68 files)
├── tests/                 # Test suite  
├── man/                   # Generated documentation
├── vignettes/             # Package vignettes
├── DESCRIPTION            # Package metadata
├── NAMESPACE             # Exported functions (auto-generated)
├── README.md             # Package overview
├── AGENTS.MD             # Developer conventions
├── LICENSE.md            # GPL-3 license
└── .lintr               # Linting configuration
```

### Key Configuration Files
- `.lintr` - Linting rules (150 char line length, snake_case, 2-space indent)
- `.github/workflows/r-cmd-check.yml` - R CMD check on Ubuntu/macOS
- `.github/workflows/test-coverage.yaml` - Code coverage via codecov
- `DESCRIPTION` - Package dependencies and metadata
- `tests/testthat.R` - Test runner entry point