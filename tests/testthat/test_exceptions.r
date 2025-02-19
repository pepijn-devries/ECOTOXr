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

test_that("Cannot pass multiple versions to get_ecotox_sqlite_file", {
  expect_error({
    get_ecotox_sqlite_file(version = c("2024-06-13", "2024-12-12"))
  })
})

test_that("Without a build, no citation can be returned", {
  expect_error({
    if (check_ecotox_availability()) {
      stop("database is build")
    } else{
      cite_ecotox()
    }
  })
})
