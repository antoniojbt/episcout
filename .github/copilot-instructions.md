# GitHub Copilot instructions for episcout

Read [`AGENTS.md`](../AGENTS.md) before inspecting or changing this repository. It is the authoritative contributor contract for conventions, checks, generated files, privacy and the GitHub work lifecycle. Use [`PROJECT_MAP.md`](../PROJECT_MAP.md) for current repository orientation.

- Use the `episcout` mamba environment declared in [`environment.yml`](../environment.yml). Run R commands through `scripts/rscript_env_caller.R`; do not assume that bare `Rscript` uses the project environment.
- Run focused checks first when practical. Use `scripts/check-local.sh` for documentation, linting, tests and the local package check, and `scripts/check-cran.sh` for release-oriented or CRAN-scope changes instead of duplicating their command sequences.
- Edit roxygen comments in `R/` and regenerate generated help with `devtools::document()` through the repository wrapper. Do not edit `man/` or `NAMESPACE` directly when roxygen owns them.
- For tracked GitHub work, run `scripts/check-workflow-state.sh` before starting, before pull-request handoff and during post-merge closeout. GitHub issues and roadmap issue #249 are the live task records.
- Treat ignored data, outputs, secrets and agent state as private. Do not add them to commits, examples, logs, issues or pull requests.

Keep changes focused, preserve unrelated work and follow the applicable checklist routing in `AGENTS.md`.
