test_that("Source code does not have things on TODO list", {
  expect_false({
    files_to_check <- list.files(pattern = ".r$|NEWS|DESCRIPTION|README", recursive = T)
    any(
      unlist(
        lapply(files_to_check, function(file) {
          content <- suppressWarnings(readLines(file))
          result  <- grepl("TODO", content) & !grepl("grepl\\(\"TODO\"", content) & !grepl("on TODO list", content)
          if (any(result)) {
            warning(sprintf("File `%s` has items on TODO list at lines `%s`", file, paste(which(result), collapse = "`, `")))
          }
          any(result)
        })
      )
    )
  })
})
