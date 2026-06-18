# Software Design

Spec ID: `005`  
Status: Implemented

## Public API

```r
epi_sec_pseudonym(
  participant_id,
  n_bytes = 24,
  prefix = "P",
  bridge_path = NULL,
  overwrite = FALSE
)
```

## Behaviour

- Accept character, numeric or factor participant IDs.
- Require participant IDs to be non-missing and unique.
- Require `n_bytes` to be a scalar whole number of at least 16.
- Generate lowercase hexadecimal token IDs from `openssl::rand_bytes()`.
- Return a tibble with `participant_id` and `token_id`, preserving input order.
- Write a CSV bridge file only when `bridge_path` is supplied.
- Refuse to overwrite an existing bridge file unless `overwrite = TRUE`.
- Do not accept a seed and do not provide deterministic output.

## Security Notes

The bridge table remains re-identifying information. Documentation must state
that it should be stored separately from pseudonymised analysis data and
protected with appropriate access controls.
