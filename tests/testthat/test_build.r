source_path <- tempfile()
dir.create(source_path)
source_file <- file.path(source_path, "ecotox_ascii_01_01_2024.zip")
file.copy(system.file("ecotox-test.zip", package = "ECOTOXr"),
          source_file)

test_that("Ecotox zip file can be unzipped", {
  expect_no_error({
    ECOTOXr:::.unzip_ecotox(FALSE, source_file, source_path, remove = TRUE) |>
      suppressMessages()
  })
})

source_path <- file.path(source_path, "ecotox_ascii_01_01_2024")

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

test_that("Dates in search results can be processed", {
  expect_no_error({
    search_ecotox(
      search = list(
        result_id = (list(terms = 1, method = "contains"))
      ),
      path = tempdir()
    ) |>
      suppressMessages() |>
      process_ecotox_dates(warn = FALSE)
  })
})

test_that("A SQL query can be obtained from a search", {
  expect_no_error({
    search_query_ecotox (
      search = list(
        result_id = (list(terms = 1, method = "contains"))
      ),
      path = tempdir()
    ) |>
      suppressMessages()
  })
})

test_that("Unknownfields are ignored", {
  expect_warning({
    search_ecotox(
      search = list(
        result_id = (list(terms = 1, method = "contains"))
      ),
      output_fields = "foobar",
      path = tempdir()
    ) |>
      suppressMessages()
  })
})

unlink(sprintf("%s.sqlite", source_path))
unlink(file.path(source_path, "ecotox_12_12_20224"), recursive = TRUE)
