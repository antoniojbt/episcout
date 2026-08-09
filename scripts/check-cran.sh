#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"
rscript="${script_dir}/rscript_env_caller.R"
check_dir="${repo_root}/build/cran-check"

cd "$repo_root"

mkdir -p "$check_dir"

runtime_path="$("$rscript" -e 'cat(Sys.getenv("PATH"))')"
export PATH="$runtime_path"

runtime_conda_prefix="$("$rscript" -e 'cat(Sys.getenv("CONDA_PREFIX"))')"
runtime_proj_data="$("$rscript" -e 'cat(Sys.getenv("PROJ_DATA"))')"
runtime_gdal_data="$("$rscript" -e 'cat(Sys.getenv("GDAL_DATA"))')"
runtime_gdal_driver_path="$("$rscript" -e 'cat(Sys.getenv("GDAL_DRIVER_PATH"))')"

if [[ -n "$runtime_conda_prefix" ]]; then
  export CONDA_PREFIX="$runtime_conda_prefix"
fi
if [[ -n "$runtime_proj_data" ]]; then
  export PROJ_DATA="$runtime_proj_data"
fi
if [[ -n "$runtime_gdal_data" ]]; then
  export GDAL_DATA="$runtime_gdal_data"
fi
if [[ -n "$runtime_gdal_driver_path" ]]; then
  export GDAL_DRIVER_PATH="$runtime_gdal_driver_path"
fi

r_home="$("$rscript" -e 'cat(R.home())')"
r_bin="${r_home}/bin/R"

if [[ ! -x "$r_bin" ]]; then
  echo "R executable not found at ${r_bin}" >&2
  exit 127
fi

work_dir="$(mktemp -d "${TMPDIR:-/tmp}/episcout-cran-check.XXXXXX")"

inspect_source_archive() {
  local tarball="$1"
  local archive_root
  local member
  local legacy_path='/Users/antoniob/Documents/github.dir/AntonioJBT/episcout/tests/testthat/'

  archive_root="$(tar -tzf "$tarball" | sed -n '1s#/.*##p')"
  if [[ -z "$archive_root" ]]; then
    echo "Unable to determine source archive root" >&2
    return 1
  fi

  for member in \
    "${archive_root}/tests/testthat/Rplots.pdf" \
    "${archive_root}/vignettes/R_datasets.xlsx"; do
    if tar -tzf "$tarball" | grep -Fqx "$member"; then
      echo "Forbidden source archive artifact: ${member}" >&2
      return 1
    fi
  done

  while IFS= read -r member; do
    case "$member" in
      "${archive_root}"/tests/testthat/*.R)
        if tar -xOzf "$tarball" "$member" | grep -Fq "$legacy_path"; then
          echo "Obsolete developer path in source archive member: ${member}" >&2
          return 1
        fi
        ;;
    esac
  done < <(tar -tzf "$tarball")
}

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
  inspect_source_archive "$tarball"
  "$r_bin" CMD check --as-cran "$tarball"
)
