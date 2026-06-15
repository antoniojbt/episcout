#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"
rscript="${script_dir}/rscript_env_caller.R"

cd "$repo_root"

"$rscript" -e 'devtools::document()'
"$rscript" -e 'devtools::test(reporter = "summary")'
"$rscript" -e 'devtools::check(manual = FALSE)'
