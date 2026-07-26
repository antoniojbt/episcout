#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"
rscript="${script_dir}/rscript_env_caller.R"

cd "$repo_root"

"$rscript" -e 'options(repos = c(CRAN = "https://cloud.r-project.org")); devtools::document()'
"$rscript" -e 'devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)'
"$rscript" -e 'options(repos = c(CRAN = "https://cloud.r-project.org")); devtools::test(reporter = "summary")'
"$rscript" -e 'options(repos = c(CRAN = "https://cloud.r-project.org")); devtools::check(manual = FALSE)'
