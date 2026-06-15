# AGENTS.md

This file defines conventions and instructions to guide OpenAI Codex and other agents in contributing to this R package.
It standardises formatting, documentation, testing, and pull request workflows.

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
Rscript -e "devtools::test(reporter = 'summary')"
```

Use `covr` to monitor test coverage:

```bash
Rscript -e "covr::report()"
```

Ensure test coverage does not decrease for existing functions.

---

## Documentation & Build Instructions

Always update documentation and verify build integrity before PRs.

```bash
# Regenerate documentation
Rscript -e "devtools::document()"

# Build the package tarball
R CMD build . --no-resave-data --compact-vignettes=gs+qpdf

# Run CRAN-like checks
R CMD check --no-manual --as-cran
```

Optional: rebuild vignettes manually

```bash
Rscript -e "devtools::build_vignettes()"
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
Rscript -e "styler::style_pkg()"
Rscript -e "lintr::lint_package()"
Rscript -e "devtools::check(manual = FALSE)"
Rscript -e "covr::report()"
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

## Agent-Specific Instructions

This AGENTS.md should be respected by OpenAI Codex or other automated agents when:

* Writing new functions (must include roxygen2 and tests)
* Refactoring existing code (preserve naming and indentation)
* Editing documentation (run `devtools::document()`)
* Opening a PR (follow title/description/branch rules)
* Running checks (use scripts provided above)

---
