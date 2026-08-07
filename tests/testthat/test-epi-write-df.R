raw_file <- function(path) {
  readBin(path, what = "raw", n = file.info(path)$size)
}

test_that("epi_write_df makes suffix and delimiter agree", {
  skip_if_not_installed("data.table")
  output_dir <- withr::local_tempdir()
  df <- data.frame(a = 1:2, b = c(3, NA_real_))

  expect_message(
    csv_path <- epi_write_df(df, output_dir, "values", "csv"),
    "File saved to:"
  )
  tsv_path <- epi_write_df(df, output_dir, "values", "tsv")

  expect_identical(
    raw_file(csv_path),
    charToRaw("a,b\n1,3\n2,NA\n")
  )
  expect_identical(
    raw_file(tsv_path),
    charToRaw("a\tb\n1\t3\n2\tNA\n")
  )
  expect_identical(csv_path, file.path(output_dir, "values.csv"))
  expect_identical(tsv_path, file.path(output_dir, "values.tsv"))
})

test_that("epi_write_df accepts recognized upper-case suffixes", {
  skip_if_not_installed("data.table")
  output_dir <- withr::local_tempdir()

  path <- epi_write_df(data.frame(a = 1), output_dir, "values", "CSV")

  expect_identical(raw_file(path), charToRaw("a\n1\n"))
  expect_identical(path, file.path(output_dir, "values.CSV"))
})

test_that("epi_write_df writes zero-row headers and overwrites existing files", {
  skip_if_not_installed("data.table")
  output_dir <- withr::local_tempdir()

  empty_path <- epi_write_df(
    data.frame(a = integer(), b = character()),
    output_dir,
    "empty",
    "csv"
  )
  expect_identical(raw_file(empty_path), charToRaw("a,b\n"))

  path <- epi_write_df(data.frame(a = 1), output_dir, "replace", "tsv")
  epi_write_df(data.frame(a = 2:3), output_dir, "replace", "tsv")
  expect_identical(raw_file(path), charToRaw("a\n2\n3\n"))
})

test_that("epi_write_df validates its path and format contract", {
  skip_if_not_installed("data.table")
  output_dir <- withr::local_tempdir()
  missing_dir <- file.path(output_dir, "missing")
  df <- data.frame(a = 1)

  expect_error(
    epi_write_df(df, missing_dir, "values", "csv"),
    "existing directory",
    fixed = TRUE
  )
  expect_false(dir.exists(missing_dir))

  invalid_suffixes <- list("txt", "", NA_character_, c("csv", "tsv"), 1)
  for (invalid_suffix in invalid_suffixes) {
    expect_error(
      epi_write_df(df, output_dir, "values", invalid_suffix),
      "`suffix` must be either",
      fixed = TRUE
    )
  }

  invalid_names <- list("", NA_character_, c("one", "two"), 1)
  for (invalid_name in invalid_names) {
    expect_error(
      epi_write_df(df, output_dir, invalid_name, "csv"),
      "`file_n` must be one non-empty string.",
      fixed = TRUE
    )
  }
  expect_error(
    epi_write_df(df, output_dir, "nested/values", "csv"),
    "must not contain a directory path",
    fixed = TRUE
  )

  invalid_separators <- list("", "||", NA_character_, c(",", "\t"), 1)
  for (invalid_sep in invalid_separators) {
    expect_error(
      epi_write_df(df, output_dir, "values", "csv", sep = invalid_sep),
      "`sep` must be NULL or one character.",
      fixed = TRUE
    )
  }
  expect_error(
    epi_write_df(df, output_dir, "values", "csv", sep = "\t"),
    "must match the delimiter required by `suffix`",
    fixed = TRUE
  )
  expect_silent(
    suppressMessages(
      epi_write_df(df, output_dir, "values", "csv", sep = ",")
    )
  )
})
