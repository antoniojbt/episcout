context("episcout CURP and misc cleaning functions")

library(episcout)
library(testthat)
library(tibble)
library(data.table)

# epi_clean_curp

test_that("epi_clean_curp extracts components", {
  curp <- "XEXX900514HDFXXX00"
  res <- epi_clean_curp(curp)
  expect_true(tibble::is_tibble(res))
  expect_equal(nrow(res), 1)
  expect_equal(res$AnoNacimiento, "1990")
  expect_equal(res$MesNacimiento, "05")
  expect_equal(res$DiaNacimiento, "14")
  expect_equal(res$Sexo, "H")
  expect_equal(res$EntidadFederativa, "DF")
  expect_equal(res$PrimerasConsonantes, "XXX")
  expect_equal(res$Homoclave, "0")
  expect_equal(res$DigitoVerificador, "0")
})

test_that("epi_clean_curp handles post-2000 years", {
  curp <- "XEXX020715MASXXXA0"
  res <- epi_clean_curp(curp)
  expect_equal(res$AnoNacimiento, "2002")
})

test_that("epi_clean_curp errors on wrong length", {
  expect_error(epi_clean_curp("SHORT"))
})

test_that("epi_clean_curp preserves its documented vector schema", {
  curps <- c("XEXX900514HDFXXX00", "XEXX020715MASXXXA0")
  result <- epi_clean_curp(curps)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
  expect_equal(result$CURP, curps)
  expect_equal(
    names(result),
    c(
      "CURP", "PrimeraLetraApellidoPaterno", "PrimeraVocalApellidoPaterno",
      "PrimeraLetraApellidoMaterno", "PrimeraLetraNombre", "AnoNacimiento",
      "MesNacimiento", "DiaNacimiento", "Sexo", "EntidadFederativa",
      "PrimerasConsonantes", "Homoclave", "DigitoVerificador"
    )
  )
})

test_that("epi_clean_curp handles empty and missing compatibility inputs", {
  empty <- epi_clean_curp(character())
  missing <- epi_clean_curp(NA_character_)

  expect_equal(nrow(empty), 0L)
  expect_equal(names(empty), names(missing))
  expect_equal(nrow(missing), 1L)
  expect_true(all(vapply(missing, function(value) all(is.na(value)), logical(1L))))
})

# epi_clean_drop_zero_levels_vector

test_that("epi_clean_drop_zero_levels_vector removes unused levels", {
  f <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  res <- epi_clean_drop_zero_levels_vector(f)
  expect_equal(levels(res), c("a", "b"))
})

test_that("epi_clean_drop_zero_levels_vector errors for non-factor", {
  expect_error(epi_clean_drop_zero_levels_vector(c(1, 2)), "not a factor")
})

# epi_clean_fct_to_na

test_that("epi_clean_fct_to_na converts values", {
  vec <- factor(c("M", "F", "Unknown", "M"), levels = c("M", "F", "Unknown"))
  res <- getFromNamespace("epi_clean_fct_to_na", "episcout")(vec, "Unknown")
  expect_equal(as.character(res), c("M", "F", NA, "M"))
  expect_false("Unknown" %in% levels(res))
})

test_that("epi_clean_fct_to_na errors for non-factor", {
  expect_error(getFromNamespace("epi_clean_fct_to_na", "episcout")(1:3, 1))
})

# epi_clean_int_to_factor

test_that("epi_clean_int_to_factor converts integer columns", {
  dt <- data.table(
    a = 1:3,
    b = as.integer(c(4, 5, 6)),
    c = as.IDate("2020-01-01") + 0:2
  )
  res <- getFromNamespace("epi_clean_int_to_factor", "episcout")(dt)
  expect_true(is.factor(res$a))
  expect_true(is.factor(res$b))
  expect_true(inherits(res$c, "IDate"))
})
