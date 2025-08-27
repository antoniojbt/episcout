skip_if_not_installed("openxlsx")

test_that("Excel file created and sheet names are unique", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  df <- data.frame(a = 1:2)
  file1 <- file.path(tmp, "this_is_a_very_long_file_name_number_1.txt")
  file2 <- file.path(tmp, "this_is_a_very_long_file_name_number_2.txt")
  file3 <- file.path(tmp, "short.txt")

  write.table(df, file1, sep = "\t", row.names = FALSE)
  write.table(df, file2, sep = "\t", row.names = FALSE)
  write.table(df, file3, sep = "\t", row.names = FALSE)

  out_path <- grob_files_to_excel(tmp, "combined")
  expect_true(file.exists(out_path))

  wb <- openxlsx::loadWorkbook(out_path)
  sheets <- openxlsx::sheets(wb)

  expect_equal(length(sheets), 3)
  expect_equal(length(unique(tolower(sheets))), 3)
  expect_true(all(nchar(sheets) <= 31))
})
