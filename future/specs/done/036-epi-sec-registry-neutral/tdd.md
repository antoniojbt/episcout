# Test Design

- A disposable schema with PUBLIC CREATE/USAGE remains initialisation-ready and successfully creates a compatible registry when the connected role has permission.
- Before/after schema and table grant state is unchanged.
- Wrong relation kind and incompatible physical structure remain `incompatible`.
- Token-setting mismatch, transaction ownership, atomic rollback and existing registry behaviour remain covered by the focused suite.
