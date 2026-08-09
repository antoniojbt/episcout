#!/usr/bin/env bash
set -uo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"
rscript="${script_dir}/rscript_env_caller.R"
online=true

usage() {
  echo "Usage: scripts/check-workflow-state.sh [--offline]" >&2
}

if [[ $# -gt 1 ]]; then
  usage
  exit 2
fi

if [[ $# -eq 1 ]]; then
  if [[ $1 != "--offline" ]]; then
    usage
    exit 2
  fi
  online=false
fi

for command_name in git awk grep sed; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "Required command is unavailable: ${command_name}" >&2
    exit 2
  fi
done

if [[ ! -x $rscript ]]; then
  echo "Repository R wrapper is unavailable: ${rscript}" >&2
  exit 2
fi

manifest_output="$($rscript --vanilla - "$repo_root" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)

if (!requireNamespace("yaml", quietly = TRUE)) {
  writeLines("The yaml package is required for workflow-state checks.", stderr())
  quit(status = 2L)
}

manifest_root <- file.path(root, "future", "specs")
paths <- sort(list.files(
  manifest_root,
  pattern = "^manifest[.]yml$",
  recursive = TRUE,
  full.names = TRUE
))

required <- c(
  "id", "slug", "workflow_version", "deliverable", "status",
  "tracking_issue", "source_issues", "pull_request", "merge_commit",
  "successor_issue", "terminal_reason"
)
allowed_status <- c("draft", "active", "review", "completed")
allowed_deliverable <- c("implementation", "design")
errors <- character()
rows <- list()
legacy <- 0L

is_scalar_number <- function(value) {
  length(value) == 1L && is.numeric(value) && !is.na(value)
}

is_scalar_text <- function(value) {
  length(value) == 1L && is.character(value) && !is.na(value) && nzchar(value)
}

for (path in paths) {
  manifest <- yaml::read_yaml(path)
  relative <- substring(normalizePath(path, winslash = "/"), nchar(root) + 2L)

  if (is.null(manifest$workflow_version)) {
    legacy <- legacy + 1L
    next
  }

  missing_fields <- setdiff(required, names(manifest))
  if (length(missing_fields) > 0L) {
    errors <- c(errors, sprintf("%s: missing fields: %s", relative, paste(missing_fields, collapse = ", ")))
    next
  }

  if (grepl("^future/specs/template/", relative)) {
    next
  }

  if (!identical(as.integer(manifest$workflow_version), 1L)) {
    errors <- c(errors, sprintf("%s: unsupported workflow_version", relative))
  }
  if (!is_scalar_text(manifest$status) || !manifest$status %in% allowed_status) {
    errors <- c(errors, sprintf("%s: invalid status", relative))
  }
  if (!is_scalar_text(manifest$deliverable) || !manifest$deliverable %in% allowed_deliverable) {
    errors <- c(errors, sprintf("%s: invalid deliverable", relative))
  }
  if (!is_scalar_number(manifest$tracking_issue)) {
    errors <- c(errors, sprintf("%s: tracking_issue must be one issue number", relative))
  }

  in_done <- grepl("^future/specs/done/", relative)
  if (identical(manifest$status, "completed") && !in_done) {
    errors <- c(errors, sprintf("%s: completed spec is outside future/specs/done", relative))
  }
  if (!identical(manifest$status, "completed") && in_done) {
    errors <- c(errors, sprintf("%s: unfinished spec is under future/specs/done", relative))
  }
  if (identical(manifest$status, "review") && !is_scalar_number(manifest$pull_request)) {
    errors <- c(errors, sprintf("%s: review status requires pull_request", relative))
  }
  if (identical(manifest$status, "completed")) {
    if (!is_scalar_number(manifest$pull_request)) {
      errors <- c(errors, sprintf("%s: completed status requires pull_request", relative))
    }
    if (!is_scalar_text(manifest$merge_commit) || !grepl("^[0-9a-f]{40}$", manifest$merge_commit)) {
      errors <- c(errors, sprintf("%s: completed status requires a 40-character merge_commit", relative))
    }
    has_successor <- is_scalar_number(manifest$successor_issue)
    has_terminal <- is_scalar_text(manifest$terminal_reason)
    if (!has_successor && !has_terminal) {
      errors <- c(errors, sprintf("%s: completed status requires successor_issue or terminal_reason", relative))
    }
  }

  value_or_empty <- function(value) {
    if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) "-" else as.character(value[[1L]])
  }

  rows[[length(rows) + 1L]] <- c(
    relative,
    value_or_empty(manifest$status),
    value_or_empty(manifest$deliverable),
    value_or_empty(manifest$tracking_issue),
    value_or_empty(manifest$pull_request),
    value_or_empty(manifest$merge_commit),
    value_or_empty(manifest$successor_issue)
  )
}

active_implementation <- vapply(rows, function(row) {
  row[[3L]] == "implementation" && row[[2L]] %in% c("active", "review")
}, logical(1L))
if (sum(active_implementation) > 1L) {
  errors <- c(errors, "More than one implementation spec is active or in review.")
}

for (error in errors) {
  cat("ERROR\t", error, "\n", sep = "")
}
cat("META\tlegacy\t", legacy, "\n", sep = "")
for (row in rows) {
  cat("SPEC\t", paste(row, collapse = "\t"), "\n", sep = "")
}

if (length(errors) > 0L) quit(status = 1L)
RSCRIPT
)"
manifest_status=$?

if [[ $manifest_status -eq 2 ]]; then
  exit 2
fi

drift=0
while IFS=$'\t' read -r record_type field_a field_b field_c field_d field_e _; do
  case "$record_type" in
    ERROR)
      echo "Workflow drift: ${field_a}" >&2
      drift=1
      ;;
    META)
      echo "Legacy completed manifests: ${field_b}"
      ;;
    SPEC)
      printf '%-10s %-14s issue #%-5s PR %-5s %s\n' "$field_b" "$field_c" "$field_d" "${field_e:--}" "$field_a"
      ;;
  esac
done <<< "$manifest_output"

if [[ $manifest_status -ne 0 ]]; then
  drift=1
fi

roadmap_issue="$(sed -n 's/.*roadmap issue \[#\([0-9][0-9]*\)\].*/\1/p' "${repo_root}/future/TODOs.md" | awk 'NR == 1 { print; exit }')"
if [[ -z $roadmap_issue ]]; then
  echo "Workflow drift: future/TODOs.md does not identify one roadmap issue" >&2
  drift=1
else
  roadmap_url="issues/${roadmap_issue}"
  for pointer_file in "${repo_root}/future/README.md" "${repo_root}/PROJECT_MAP.md"; do
    if ! grep -Fq "$roadmap_url" "$pointer_file"; then
      echo "Workflow drift: ${pointer_file#"${repo_root}/"} does not point to roadmap issue #${roadmap_issue}" >&2
      drift=1
    fi
  done
fi

if [[ $online == false ]]; then
  if [[ $drift -ne 0 ]]; then
    exit 1
  fi
  echo "Workflow state is structurally consistent (offline)."
  exit 0
fi

if ! command -v gh >/dev/null 2>&1; then
  echo "Required online command is unavailable: gh" >&2
  exit 2
fi

if ! gh auth status >/dev/null 2>&1; then
  echo "GitHub authentication is unavailable; use --offline only when this is expected." >&2
  exit 2
fi

upstream_url="$(git -C "$repo_root" remote get-url upstream 2>/dev/null)"
if [[ -z $upstream_url ]]; then
  echo "Canonical upstream remote is unavailable." >&2
  exit 2
fi

repository="$(gh repo view "$upstream_url" --json nameWithOwner --jq .nameWithOwner 2>/dev/null)"
default_branch="$(gh repo view "$upstream_url" --json defaultBranchRef --jq .defaultBranchRef.name 2>/dev/null)"
if [[ -z $repository || -z $default_branch ]]; then
  echo "Unable to resolve the canonical GitHub repository and default branch." >&2
  exit 2
fi

roadmap_state="$(gh issue view "$roadmap_issue" --repo "$repository" --json state --jq .state 2>/dev/null)"
if [[ $roadmap_state != "OPEN" ]]; then
  echo "Workflow drift: roadmap issue #${roadmap_issue} is not open" >&2
  drift=1
fi

default_ref="refs/remotes/upstream/${default_branch}"
if ! git -C "$repo_root" show-ref --verify --quiet "$default_ref"; then
  echo "Canonical remote-tracking ref is unavailable: ${default_ref}" >&2
  exit 2
fi

while IFS=$'\t' read -r record_type path status _ issue pull_request merge_commit successor_issue; do
  [[ $record_type == "SPEC" ]] || continue

  [[ $pull_request == "-" ]] && pull_request=""
  [[ $merge_commit == "-" ]] && merge_commit=""
  [[ $successor_issue == "-" ]] && successor_issue=""

  issue_state="$(gh issue view "$issue" --repo "$repository" --json state --jq .state 2>/dev/null)"
  if [[ -z $issue_state ]]; then
    echo "Workflow drift: ${path}: tracking issue #${issue} is unavailable" >&2
    drift=1
  elif [[ $status == "completed" && $issue_state != "CLOSED" ]]; then
    echo "Workflow drift: ${path}: completed spec has open tracking issue #${issue}" >&2
    drift=1
  elif [[ $status != "completed" && $issue_state != "OPEN" ]]; then
    echo "Workflow drift: ${path}: unfinished spec has closed tracking issue #${issue}" >&2
    drift=1
  fi

  if [[ -n $pull_request ]]; then
    pr_state="$(gh pr view "$pull_request" --repo "$repository" --json state --jq .state 2>/dev/null)"
    pr_merge="$(gh pr view "$pull_request" --repo "$repository" --json mergeCommit --jq '.mergeCommit.oid // ""' 2>/dev/null)"
    if [[ -z $pr_state ]]; then
      echo "Workflow drift: ${path}: PR #${pull_request} is unavailable" >&2
      drift=1
    elif [[ $status == "review" && $pr_state != "OPEN" ]]; then
      echo "Workflow drift: ${path}: PR #${pull_request} is ${pr_state}; post-merge closeout is required" >&2
      drift=1
    elif [[ $status == "completed" && $pr_state != "MERGED" ]]; then
      echo "Workflow drift: ${path}: completed spec PR #${pull_request} is not merged" >&2
      drift=1
    elif [[ $status == "completed" && $pr_merge != "$merge_commit" ]]; then
      echo "Workflow drift: ${path}: recorded merge commit does not match PR #${pull_request}" >&2
      drift=1
    fi
  fi

  if [[ $status == "completed" ]]; then
    if ! git -C "$repo_root" merge-base --is-ancestor "$merge_commit" "$default_ref"; then
      echo "Workflow drift: ${path}: merge commit is not reachable from ${default_ref}" >&2
      drift=1
    fi
    if [[ -n $successor_issue ]] && ! gh issue view "$successor_issue" --repo "$repository" --json number >/dev/null 2>&1; then
      echo "Workflow drift: ${path}: successor issue #${successor_issue} is unavailable" >&2
      drift=1
    fi
  fi
done <<< "$manifest_output"

if [[ $drift -ne 0 ]]; then
  exit 1
fi

echo "Workflow state matches GitHub (${repository}@${default_branch})."
