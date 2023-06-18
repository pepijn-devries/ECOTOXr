test_that("Source code does not have things on TODO list", {
  expect_false({
    files_to_check <- list.files("../..", pattern = ".r$|NEWS|DESCRIPTION|README", recursive = TRUE, full.names = TRUE)
    files_to_check <- files_to_check[!endsWith(files_to_check, "test_dev.r")]
    any(
      unlist(
        lapply(files_to_check, function(file) {
          content <- suppressWarnings(readLines(file))
          if (is.null(content)) return (FALSE)
          result  <- grepl("TODO", content)
          if (any(result)) {
            warning(sprintf("File `%s` has items on TODO list at lines `%s`", file, paste(which(result), collapse = "`, `")))
          }
          any(result)
        })
      )
    )
  })
})
