test_that("identifier specification defaults preserve exact behaviour", {
  tables <- data.frame(
    source_schema = c("source", "source"),
    source_table = c("people", "visits"),
    id_column = c("person_id", "person_id"),
    identity_namespace = c("people", "people"),
    can_enrol = c(TRUE, FALSE),
    one_row_per_entity = c(TRUE, FALSE),
    destination_table = c("people_safe", "visits_safe"),
    provenance = c("synthetic", "synthetic"),
    stringsAsFactors = FALSE
  )
  columns <- do.call(rbind, lapply(seq_len(nrow(tables)), function(i) {
    data.frame(
      source_schema = tables$source_schema[[i]], source_table = tables$source_table[[i]],
      source_column = "person_id", output_action = "pseudonymise",
      stringsAsFactors = FALSE
    )
  }))
  linkage <- epi_sec_linkage_spec(
    tables, columns,
    data.frame(source_schema = character(), source_table = character(), key_column = character(), key_order = integer()),
    data.frame(crosswalk_schema = character(), crosswalk_table = character(), alias_namespace = character(), alias_id_column = character(), canonical_namespace = character(), canonical_id_column = character(), provenance = character())
  )
  spec <- epi_sec_identifier_spec(linkage)
  expect_s3_class(spec, "epi_sec_identifier_spec")
  expect_true(all(spec$rules$normalization == "identity"))
  expect_true(all(is.na(spec$rules$validity_regex)))
  expect_identical(spec, epi_sec_identifier_spec(linkage, spec$rules))

  rules <- spec$rules
  rules$normalization <- c("trim", "trim_upper")
  rules$validity_regex <- "^[A-Z0-9]+$"
  rules$invalid_policy[[2]] <- "retain_and_flag"
  rules$validity_column[[2]] <- "identifier_valid"
  prepared <- epi_sec_identifier_spec(linkage, rules)
  expect_identical(prepared$rules$normalization, c("trim", "trim_upper"))

  expect_error(epi_sec_identifier_spec(linkage, rules[-1, ]), "cover")
  bad <- rules
  bad$normalization[[1]] <- "lower"
  expect_error(epi_sec_identifier_spec(linkage, bad), "normalization")
  bad <- rules
  bad$validity_column[[1]] <- "unexpected"
  expect_error(epi_sec_identifier_spec(linkage, bad), "required only")
})
