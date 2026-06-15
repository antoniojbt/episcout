#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"
rscript="${script_dir}/rscript_env_caller.R"
check_dir="${repo_root}/build/cran-check"

cd "$repo_root"

mkdir -p "$check_dir"

export PATH="$("$rscript" -e 'cat(Sys.getenv("PATH"))')"

r_home="$("$rscript" -e 'cat(R.home())')"
r_bin="${r_home}/bin/R"

if [[ ! -x "$r_bin" ]]; then
  echo "R executable not found at ${r_bin}" >&2
  exit 127
fi

work_dir="$(mktemp -d "${TMPDIR:-/tmp}/episcout-cran-check.XXXXXX")"

copy_check_artifacts() {
  local status=$?

  rm -f "${check_dir}"/*.tar.gz
  rm -rf "${check_dir}/episcout.Rcheck"
  cp "${work_dir}"/episcout_*.tar.gz "${check_dir}/" 2>/dev/null || true
  if [[ -d "${work_dir}/episcout.Rcheck" ]]; then
    cp -R "${work_dir}/episcout.Rcheck" "${check_dir}/"
  fi
  rm -rf "$work_dir"

  exit "$status"
}

trap copy_check_artifacts EXIT

(
  cd "$work_dir"
  "$r_bin" CMD build "$repo_root" --no-resave-data --compact-vignettes=gs+qpdf
  tarball="$(find . -maxdepth 1 -name 'episcout_*.tar.gz' -print -quit)"
  if [[ -z "$tarball" ]]; then
    echo "R CMD build did not produce an episcout source tarball" >&2
    exit 1
  fi
  "$r_bin" CMD check --as-cran "$tarball"
)
