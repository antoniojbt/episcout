# Software Design

The registry result keeps `status`, `mode`, `writes`, `registry_schema`, `metadata`, `objects` and neutral `next_action`; it removes `schema_restricted`. Valid statuses are `initialisation_required`, `incompatible` and `ready`. Compatibility depends only on the exact relation set/kind, physical structure, version and immutable token settings.

Registry inspection must not query relation ownership or PUBLIC privileges. Initialisation retains the existing repeatable-read transaction and inside-transaction empty-state recheck, but emits no GRANT or REVOKE SQL. PostgreSQL itself reports denied access through the established sanitised database boundary.
