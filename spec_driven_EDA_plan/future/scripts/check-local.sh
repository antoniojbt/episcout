#!/usr/bin/env bash
set -euo pipefail

Rscript -e 'devtools::document()'
Rscript -e 'devtools::test(reporter = "summary")'
Rscript -e 'devtools::check(manual = FALSE, error_on = "warning")'
