context("explicit-pair Spearman association contract")

association_spearman_spec <- function(names,
                                      types = rep("numeric", length(names)),
                                      roles = rep("measure", length(names)),
                                      missing_codes = rep("", length(names))) {
  data.frame(
    name = names,
    label = paste("Label", names),
    database_type = ifelse(types == "integer", "integer", "numeric"),
    analysis_type = types,
    role = roles,
    missing_codes = missing_codes,
    stringsAsFactors = FALSE
  )
}

test_that("tied-rank Spearman truth is independently anchored", {
  data <- data.frame(
    x = c(1, 1, 2, 3, 4),
    positive = c(1, 1, 2, 3, 4),
    negative = c(4, 4, 3, 2, 1),
    nonperfect = c(5, 4, 4, 2, 1)
  )
  spec <- association_spearman_spec(names(data))
  pairs <- data.frame(
    first = rep("x", 3L),
    second = c("positive", "negative", "nonperfect"),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_profile_spearman(data, spec, pairs)

  expect_named(observed, c(
    "left", "left_label", "right", "right_label", "n", "rho",
    "status", "reason"
  ))
  expect_identical(observed$left, pairs$first)
  expect_identical(observed$right, pairs$second)
  expect_identical(observed$left_label, rep("Label x", 3L))
  expect_identical(
    observed$right_label,
    c("Label positive", "Label negative", "Label nonperfect")
  )
  expect_identical(observed$n, rep(5, 3L))
  expect_equal(observed$rho, c(1, -1, -0.921052631578948), tolerance = 1e-14)
  expect_identical(observed$status, rep("available", 3L))
  expect_true(all(is.na(observed$reason)))
  expect_true(is.double(observed$n))
  expect_true(is.double(observed$rho))
})

test_that("pairwise eligibility excludes every missing and non-finite state", {
  data <- data.frame(
    left = c(1, 2, NA, 999, NaN, Inf, -Inf, 8, 9),
    right = c(1, 2, 3, 4, 5, 6, 7, NA, 999)
  )
  spec <- association_spearman_spec(
    names(data), missing_codes = c("999", "999")
  )

  observed <- epi_eda_profile_spearman(
    data,
    spec,
    data.frame(a = "left", b = "right", stringsAsFactors = FALSE)
  )

  expect_identical(observed$n, 2)
  expect_equal(observed$rho, 1)
  expect_identical(observed$status, "available")
  expect_true(is.na(observed$reason))
})

test_that("unavailable pairs distinguish eligibility and constant sides", {
  data <- data.frame(
    insufficient_left = c(1, NA, NA),
    insufficient_right = c(1, 2, 3),
    constant_left = c(1, 1, 1),
    varying_right = c(1, 2, 3),
    varying_left = c(1, 2, 3),
    constant_right = c(2, 2, 2),
    constant_both_left = c(3, 3, 3),
    constant_both_right = c(4, 4, 4)
  )
  spec <- association_spearman_spec(names(data))
  pairs <- data.frame(
    left = c(
      "insufficient_left", "constant_left", "varying_left",
      "constant_both_left"
    ),
    right = c(
      "insufficient_right", "varying_right", "constant_right",
      "constant_both_right"
    ),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_profile_spearman(data, spec, pairs)

  expect_identical(observed$n, c(1, 3, 3, 3))
  expect_true(all(is.na(observed$rho)))
  expect_identical(observed$status, rep("unavailable", 4L))
  expect_identical(observed$reason, c(
    "Fewer than two eligible observations.",
    "The left variable is constant among eligible observations.",
    "The right variable is constant among eligible observations.",
    "Both variables are constant among eligible observations."
  ))
})

test_that("empty pairs return the exact typed schema", {
  data <- data.frame(value = numeric())
  spec <- association_spearman_spec("value")
  pairs <- data.frame(a = character(), b = character())

  observed <- epi_eda_profile_spearman(data, spec, pairs)

  expect_identical(observed, data.frame(
    left = character(), left_label = character(),
    right = character(), right_label = character(),
    n = numeric(), rho = numeric(), status = character(), reason = character(),
    stringsAsFactors = FALSE
  ))
})

test_that("invalid explicit pairs fail before calculation", {
  data <- data.frame(
    x = 1:3,
    y = 3:1,
    identifier = 1:3,
    category = c("a", "b", "c")
  )
  spec <- association_spearman_spec(
    names(data),
    types = c("numeric", "numeric", "integer", "categorical"),
    roles = c("measure", "measure", "identifier", "measure")
  )
  pair <- function(left, right) {
    data.frame(left = left, right = right, stringsAsFactors = FALSE)
  }

  expect_error(
    epi_eda_profile_spearman(data, spec, data.frame(x = "x")),
    "two-column"
  )
  expect_error(
    epi_eda_profile_spearman(data, spec, pair("x", NA_character_)),
    "non-missing"
  )
  expect_error(
    epi_eda_profile_spearman(data, spec, pair("x", "x")),
    "self pair"
  )
  expect_error(
    epi_eda_profile_spearman(
      data, spec,
      rbind(pair("x", "y"), pair("y", "x"))
    ),
    "duplicate unordered"
  )
  expect_error(
    epi_eda_profile_spearman(data, spec, pair("x", "absent")),
    "specification"
  )
  expect_error(
    epi_eda_profile_spearman(data, spec, pair("x", "category")),
    "numeric or integer"
  )
  expect_error(
    epi_eda_profile_spearman(data, spec, pair("x", "identifier")),
    "identifier-role"
  )
  expect_error(
    epi_eda_profile_spearman(data[-2L], spec, pair("x", "y")),
    "present in data"
  )
})

test_that("non-missing incompatible storage fails but all-missing storage is unavailable", {
  spec <- association_spearman_spec(
    c("left", "right"), missing_codes = c("999", "")
  )
  pair <- data.frame(left = "left", right = "right", stringsAsFactors = FALSE)

  expect_error(
    epi_eda_profile_spearman(
      data.frame(left = c("1", "2"), right = c(1, 2)), spec, pair
    ),
    "numeric vector storage"
  )

  observed <- epi_eda_profile_spearman(
    data.frame(left = c("999", NA_character_), right = c(1, 2)), spec, pair
  )
  expect_identical(observed$n, 0)
  expect_true(is.na(observed$rho))
  expect_identical(observed$reason, "Fewer than two eligible observations.")
})
