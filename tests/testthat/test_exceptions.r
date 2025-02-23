test_that("Mismatch between cas numbers and checksums throws error", {
  expect_error({
    cas_data <- as.cas(c("64175", "71432", "58082"))
    attributes(cas_data)$checksum <- attributes(cas_data)$checksum[-3]
    is.cas(cas_data)
  })
})

test_that("Non-numeric characters in a CAS throws error", {
  expect_error({
    as.cas("foobar")
  })
})

test_that("Faulty checksum in CAS throws error", {
  expect_error({
    as.cas("129-00-1")
  })
})

test_that("Cannot ask in a non-interactive session", {
  expect_error({
    if (interactive()) {
      stop("this test doesn't work in an interactive environment")
    } else {
      download_ecotox_data(tempdir(), ask = TRUE)
    }
  })
})
test_that("Cannot pass multiple versions to get_ecotox_sqlite_file", {
  expect_error({
    get_ecotox_sqlite_file(version = c("2024-06-13", "2024-12-12"))
  })
})

test_that("Message when database is not available", {
  expect_message({
    if (check_ecotox_availability()) {
      message("db available")
    } else {
      check_ecotox_version(tempdir())
    }
  })
})

test_that("Error for unsupported method", {
  expect_error({
    search_ecotox(list(latin_name = list(terms = "foobar", method = "foobar")))
  })
})

test_that("Can only convert characters to ECOTOX numerics", {
  expect_error({
    as_numeric_ecotox(1L)
  })
})

test_that("Can only convert characters to ECOTOX dates", {
  expect_error({
    as_date_ecotox(1L)
  })
})