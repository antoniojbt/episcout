# AGENTS.md

This file defines conventions and instructions to guide OpenAI Codex and other agents in contributing to this R package.
It standardises formatting, documentation, testing, and pull request workflows.

It should be respected by automated agents when:

* Writing new functions (must include roxygen2 and tests)
* Refactoring existing code (preserve naming and indentation)
* Editing documentation (run `devtools::document()`)
* Opening a PR (follow title/description/branch rules)
* Running checks (use scripts provided above)

Do not overcomplicate and over-engineer. Prefer the simpler solution. Make it easy for a human to review, supervise and monitor work. Do not bloat file content or create numerous files when one would suffice. e.g. use one `TODOs.md`, not a dir with `now.md`, `next.md`, `later.md`.

---

## 📁 Project Structure

| Folder             | Purpose                                          |
| ------------------ | ------------------------------------------------ |
| `/R/`              | R function scripts                               |
| `/man/`            | Auto-generated documentation from roxygen2       |
| `/tests/testthat/` | Unit tests (testthat framework)                  |
| `/vignettes/`      | Tutorials and worked examples (built with knitr) |
| `/data/`           | Data objects for examples (`.rda`, `.rds`)       |
| `/inst/`           | Miscellaneous files (e.g. extdata, configs)      |
| `/docs/`           | Site output (e.g., pkgdown)                      |

---

## R Environment

Use the repo-local R wrapper for all R commands:

```bash
scripts/rscript_env_caller.R -e "R.home(); .libPaths()"
```

The wrapper resolves to the project mamba environment by default:

```text
~/apps/miniforge3/envs/episcout/bin/Rscript
```

Do not use bare `Rscript` unless you have verified it resolves to the
`episcout` mamba environment. To use a different R binary, set
`EPISCOUT_RSCRIPT`; see `scripts/rscript_env_caller.R`.

For `devtools` checks, set a real CRAN mirror inside the R expression. Do not
leave `repos` as the placeholder `@CRAN@`, because package checks may spend a
long time probing package repositories.

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```

## Repo Script Discovery and Naming

Before adding or changing helper commands, inspect existing project entry
points:

```text
scripts/
Makefile
.github/workflows/
AGENTS.MD
```

Do not add a new helper script if an existing script already covers the task.
If an existing helper has an unclear or misleading name, propose a rename or
targeted cleanup instead of adding another wrapper.

Script names must describe the project action, not merely mirror a system
binary. Avoid names that are visually or semantically confusable with common
commands, such as:

```text
rscript
python
R
test
check
```

Prefer explicit project or task names such as:

```text
scripts/rscript_env_caller.R
scripts/check-local.sh
scripts/check-cran.sh
scripts/render-report.sh
```

Before adding a new script, confirm:

1. No existing script already does the job.
2. The name is unambiguous in chat, docs, logs and CI.
3. The script follows the repo's existing shell/R conventions.


## Coding Conventions

* Use `snake_case` for object and function names.

* Use **2-space indentation**, no tabs.

* Include roxygen2 documentation for every exported function.

* Function structure:

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

* Use `styler::style_pkg()` and `lintr::lint_package()` to enforce style.

* Avoid `<<-`, `assign()`, and hidden state unless strictly necessary.

---

## Testing Protocols

All new functions must include unit tests in `/tests/testthat/`.
Run tests locally before committing:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
```

Use `covr` to monitor test coverage:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); covr::report()"
```

Ensure test coverage does not decrease for existing functions.

---

## Documentation & Build Instructions

Always update documentation and verify build integrity before PRs.

```bash
# Regenerate documentation
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::document()"

# Build the package tarball
R CMD build . --no-resave-data --compact-vignettes=gs+qpdf

# Run CRAN-like checks
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```

Optional: rebuild vignettes manually

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::build_vignettes()"
```

---

## Package Website

There is no package website

---

## Pull Request Guidelines

### Branch Naming

* `feature/<desc>` for new features
* `bugfix/<desc>` for bug fixes
* `refactor/<desc>` for internal refactoring

### PR Title Format

```
[Type] Summary of change
```

Examples:

* `[Fix] Handle NA inputs in calculate_rate()`
* `[Feature] Add export_to_parquet() utility`

### PR Description Template

1. **What was changed** and why
2. **Tests added** (brief description or file names)
3. **Backward compatibility** (e.g., no breaking changes)
4. **Code coverage change** (if any)

---

## Pre-Merge Checklist

Run all checks **before requesting a review**:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); styler::style_pkg()"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); lintr::lint_package()"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); covr::report()"
```

All of the above must pass before merging.

---

## Dependency Management

* All dependencies must be declared in `DESCRIPTION` (`Imports` or `Suggests`)
* Avoid suggesting base R functions or commonly installed packages
* CI uses `remotes::install_deps(dependencies = TRUE)`

---

## GitHub Actions CI (if enabled)

The repo uses GitHub Actions to:

* Install dependencies
* Run `R CMD check`
* Generate code coverage via Codecov
* Deploy `pkgdown` site

If modifying `.github/workflows/`, test using:

```bash
act -j check  # if act is installed for local testing
```

---
