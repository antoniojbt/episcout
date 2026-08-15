#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"
rscript="${script_dir}/rscript_env_caller.R"

cd "$repo_root"

initial_status="$(git status --porcelain)"
temporary_dir="$(mktemp -d /tmp/episcout-check-local.XXXXXX)"
trap 'rm -rf "$temporary_dir"' EXIT

cp -a "$repo_root/." "$temporary_dir/package"

"$rscript" -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::document(pkg = '$temporary_dir/package')"
"$rscript" -e 'devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)'
"$rscript" -e 'options(repos = c(CRAN = "https://cloud.r-project.org")); devtools::test(reporter = "summary")'
"$rscript" -e 'options(repos = c(CRAN = "https://cloud.r-project.org")); devtools::check(manual = FALSE)'

if [[ "$(git status --porcelain)" != "$initial_status" ]]; then
  echo "check-local.sh changed the checkout; inspect generated files or test snapshots." >&2
  git status --short >&2
  exit 1
fi
