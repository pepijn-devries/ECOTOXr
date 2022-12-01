test_that("Source code does not have things on TODO list", {
  expect_false({
    files_to_check <- list.files(pattern = ".r$|NEWS|DESCRIPTION", recursive = T)
    any(
      unlist(
        lapply(files_to_check, function(file) {
          content <- suppressWarnings(readLines(file))
          any(grepl("TODO", content) & !grepl("grepl\\(\"TODO\"", content))
        })
      )
    )
  })
})
