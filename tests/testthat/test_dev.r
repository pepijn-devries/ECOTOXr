test_that("Source code should not have things on TODO list", {
  skip_on_ci()
  skip_on_cran()
  expect_false({
    skip_if(length(unclass(packageVersion("ECOTOXr"))[[1]]) > 3,
            "Skipping during development")
    r_path <- normalizePath(test_path()) |> dirname() |> dirname()
    files_to_check <- list.files(r_path, pattern = ".[r|R]$|NEWS|DESCRIPTION|README\\.md",
                                 recursive = TRUE, full.names = TRUE)
    files_to_check <- files_to_check[!endsWith(files_to_check, "test-dev.r")]
    any(
      unlist(
        lapply(files_to_check, function(file) {
          content <- suppressWarnings(readLines(file))
          result  <- grepl("TODO", content) & !grepl("grepl\\(\"TODO\"", content) & !grepl("on TODO list", content)
          if (any(result)) {
            warning(sprintf("File `%s` has items on TODO list at lines `%s`",
                            file, paste(which(result), collapse = "`, `")))
          }
          any(result)
        })
      )
    )
  })
})