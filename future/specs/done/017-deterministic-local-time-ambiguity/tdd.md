# Test Design

Spec ID: `017-deterministic-local-time-ambiguity`  
Status: Completed  

## Baseline

The inherited spec-016 baseline recorded 1,573 passing expectations plus one local macOS failure at `tests/testthat/test-eda-prepare.R:223`. A direct probe on R 4.5.3 aarch64 macOS returned `TRUE` from the sampled-offset validator for the ambiguous Kwajalein fixture. The initial focused `devtools::test(filter = 'eda-prepare')` invocation could not load because the documented local environment lacked imported package DBI; dependency installation is required before post-change verification.

## Behaviour Tests

- [ ] Modern New York ambiguous and nonexistent wall times block.
- [ ] The 1969 Kwajalein repeated wall time blocks on every supported platform.
- [ ] A neighbouring unique Kwajalein wall time prepares to the independently calculated UTC instant.
- [ ] The 1993 Kwajalein skipped-day wall time blocks.
- [ ] UTC local input, `Z`, positive and negative offsets, mixed values and fractional seconds retain exact output semantics.
- [ ] Missing, invalid and whitespace-padded timezone names block.
- [ ] Process `TZ` changes do not affect status or numeric UTC output.
- [ ] Audit output and captured conditions contain no observed datetime values.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-prepare', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
```

GitHub's existing macOS and Ubuntu package-check jobs provide the supported cross-platform acceptance matrix.
