
# Test for wide-format output with multiple date columns

test_that("epi_stats_dates_multi returns wide-format summary for multiple date columns", {
  df <- data.frame(
    date1 = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    date2 = as.Date(c("2021-02-01", NA, "2021-02-03")),
    value = 1:3
  )

  result <- epi_stats_dates_multi(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$Column, c("date1", "date2"))

  expected_names <- c(
    "Column", "N", "N Missing", "N Unique", "Min", "25%", "Median",
    "75%", "Max", "IQR", "Most Common", "Range (Days)"
  )
  expect_true(all(expected_names %in% names(result)))

  expect_equal(
    result$Min[result$Column == "date1"],
    as.character(min(df$date1))
  )
  expect_equal(
    result$`N Missing`[result$Column == "date2"],
    as.character(sum(is.na(df$date2)))
  )
})
