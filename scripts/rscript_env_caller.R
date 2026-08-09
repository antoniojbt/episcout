#!/usr/bin/env bash
set -euo pipefail

default_rscript="${HOME}/apps/miniforge3/envs/episcout/bin/Rscript"
rscript="${EPISCOUT_RSCRIPT:-$default_rscript}"

if [[ ! -x "$rscript" ]]; then
  cat >&2 <<EOF
episcout Rscript was not found or is not executable:
  $rscript

Create or update the dedicated package-development environment with:
  mamba env create -f environment.yml
  # or, if the env already exists:
  mamba env update -n episcout -f environment.yml --prune

To use a different R binary, set EPISCOUT_RSCRIPT:
  EPISCOUT_RSCRIPT=/path/to/Rscript scripts/rscript_env_caller.R -e 'R.version.string'
EOF
  exit 127
fi

env_bin="$(cd "$(dirname "$rscript")" && pwd)"
env_prefix="$(cd "${env_bin}/.." && pwd)"
export PATH="${env_bin}:${PATH}"
export CONDA_PREFIX="${env_prefix}"

if [[ -d "${env_prefix}/share/proj" ]]; then
  export PROJ_DATA="${env_prefix}/share/proj"
fi
if [[ -d "${env_prefix}/share/gdal" ]]; then
  export GDAL_DATA="${env_prefix}/share/gdal"
fi
if [[ -d "${env_prefix}/lib/gdalplugins" ]]; then
  export GDAL_DRIVER_PATH="${env_prefix}/lib/gdalplugins"
fi

exec "$rscript" "$@"
