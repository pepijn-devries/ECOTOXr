source_path <- tempfile()
dir.create(source_path)
source_file <- file.path(test_path(), "test_data", "ecotox-test.zip")
unzip(source_file, exdir = source_path)

test_that("Local build can be created from a small mockup file", {
  expect_no_error({
    build_ecotox_sqlite(source_path, tempdir()) |>
      suppressMessages()
  })
})

unlink(sprintf("%s.sqlite", source_path))
unlink(source_path, recursive = TRUE)