test_that("Text is sanitised to numerics as expected", {
  expect_true({
    char_num <- c("10", " 2", "3 ", "~5", "9.2*", "2,33",
                  "2,333", "2.1(1.0 - 3.2)", "1-5", "1e-3") |>
      as_numeric_ecotox(range_fun = median)
    all(char_num == c(10, 2, 3, 5, 9.2, 2.33, 2333, 2.1, 3, 0.001))
  })
})
