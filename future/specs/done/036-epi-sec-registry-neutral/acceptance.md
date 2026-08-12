# Acceptance

Spec ID: `036-epi-sec-registry-neutral`
Status: Completed

- [x] Registry result has no `schema_restricted` field; incompatible existing objects use `incompatible`.
- [x] Registry initialisation issues no privilege query, GRANT or REVOKE statement.
- [x] PUBLIC privileges neither block audit nor apply, and schema grants are identical before and after apply in the disposable PostgreSQL regression.
- [x] Structure/version/token-setting, transaction, rollback and sanitised database-error behaviour remain covered by focused tests.
- [x] Focused offline/live PostgreSQL tests, lint, workflow state and diff checks pass; the broad local test run was started but its command transport did not return a final completion result.
- [x] Directly affected help and vignette prose describe observable technical behaviour.
- [x] PR-286 merged to canonical `master` as `commit-6312b8d` on 2026-08-12. Hosted macOS/Ubuntu R CMD checks, PostgreSQL integration, test coverage and Codecov statuses passed; CodeFactor failed and is owner-designated informational rather than merge-blocking.
- [x] Post-merge closeout records issue-285 as the staged successor and moves this completed specification under `future/specs/done/`.
