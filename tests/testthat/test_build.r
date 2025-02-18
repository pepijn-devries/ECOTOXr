source_path <- tempfile()
dir.create(source_path)
source_path <- file.path(source_path, format(Sys.Date(), "ecotox_ascii_%m_%d_%Y"))
dir.create(source_path)
source_file <- system.file("ecotox-test.zip", package = "ECOTOXr")
unzip(source_file, exdir = source_path)

test_that("Local build can be created from a small mockup file", {
  expect_no_error({
    build_ecotox_sqlite(source_path, tempdir()) |>
      suppressMessages()
  })
})

test_that("After building the attach function shows message", {
  expect_message({
    ECOTOXr:::.onAttach(NULL, NULL)
  })
})

test_that("sqlite file is returned after build", {
  expect_true({
    get_ecotox_sqlite_file(tempdir()) |>
      endsWith(".sqlite")
  })
})

test_that("Can open and close newly build database", {
  expect_no_error({
    con <- dbConnectEcotox(tempdir())
    dplyr::tbl(con, "results") |> dplyr::collect()
    dbDisconnectEcotox(con)
  })
})

test_that("Newly build database can be queried", {
  expect_no_error({
    search_ecotox(
      search = list(
        result_id = (list(terms = 2565223L, method = "exact"))
      ),
      path = tempdir()
    ) |>
      suppressMessages()
  })
})

test_that("The newly build database can be checked", {
  expect_no_error({
    check_ecotox_build(tempdir()) |> suppressMessages()
  })
})

test_that("Newly build version is faulty as it is a mockup", {
  skip_if_offline()
  expect_false({
    check_ecotox_version(tempdir(), verify_ssl = FALSE) |>
      suppressMessages()
  })
})

test_that("Numerics in search results can be processed", {
  expect_no_error({
    search_ecotox(
        search = list(
          result_id = (list(terms = 1, method = "contains"))
        ),
        path = tempdir()
      ) |>
      suppressMessages() |>
      process_ecotox_numerics(warn = FALSE)
  })
})

unlink(sprintf("%s.sqlite", source_path))
unlink(source_path, recursive = TRUE)
