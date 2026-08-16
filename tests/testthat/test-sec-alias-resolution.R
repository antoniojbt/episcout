test_that("exact alias specifications are value-free and deterministic", {
  pairs <- data.frame(
    source_schema = "identity_data",
    source_table = "observations",
    alias_id_column = "secondary_id",
    canonical_id_column = "primary_id",
    alias_namespace = "secondary",
    canonical_namespace = "primary",
    provenance = "fixture",
    stringsAsFactors = FALSE
  )

  spec <- epi_sec_alias_spec(pairs, validity_regex = "^[A-Z0-9]+$")

  expect_s3_class(spec, "epi_sec_alias_spec")
  expect_named(
    spec,
    c("pairs", "validity_regex", "contract_version", "fingerprint_sha256")
  )
  expect_equal(spec$contract_version, "exact-alias-1")
  expect_equal(spec$pairs, pairs)
  expect_false(any(grepl("ABC123", unlist(spec), fixed = TRUE)))
  expect_identical(spec, epi_sec_alias_spec(pairs, validity_regex = "^[A-Z0-9]+$"))
})

test_that("exact alias specifications reject ambiguity in metadata", {
  pairs <- data.frame(
    source_schema = "identity_data",
    source_table = "observations",
    alias_id_column = "secondary_id",
    canonical_id_column = "primary_id",
    alias_namespace = "secondary",
    canonical_namespace = "primary",
    provenance = "fixture",
    stringsAsFactors = FALSE
  )

  expect_error(
    epi_sec_alias_spec(transform(pairs, canonical_namespace = "secondary")),
    "must differ"
  )
  expect_error(
    epi_sec_alias_spec(rbind(pairs, pairs)),
    "must not repeat"
  )
  expect_error(
    epi_sec_alias_spec(pairs[, -1L]),
    "missing required"
  )
})
