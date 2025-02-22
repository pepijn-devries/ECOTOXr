test_that("Text is sanitised to numerics as expected", {
  expect_true({
    char_num <- c("10", " 2", "3 ", "~5", "9.2*", "2,33",
                  "2,333", "2.1(1.0 - 3.2)", "1-5", "1e-3") |>
      as_numeric_ecotox(range_fun = median)
    all(char_num == c(10, 2, 3, 5, 9.2, 2.33, 2333, 2.1, 3, 0.001))
  })
})

test_that("Text is sanitised to numerics as expected", {
  expect_true({
    char_date <- c("5-19-1987   ", "5/dd/2021", "3/19/yyyy", "1985", "mm/19/1999",
                   "October 2004", "nr/nr/2015")
    
    identical(
      as_date_ecotox(char_date),
      as.Date(c("1987-05-19", "2021-05-01", "", "1985-01-01",
                "1999-01-19", "2004-10-01", "2015-01-01"))
    ) &&
      identical(
        as_date_ecotox(char_date, dd = 15L),
        as.Date(c("1987-05-19", "2021-05-15", "", "1985-01-15",
                  "1999-01-19", "2004-10-15", "2015-01-01"))
      ) &&
      identical(
        as_date_ecotox(char_date, dd = -1L),
        as.Date(c("1987-05-19", "", "", "", "1999-01-19", "", "2015-01-01"))
      ) &&
      identical(
        as_date_ecotox(char_date, mm = 6L),
        as.Date(c("1987-05-19", "2021-05-01", "", "1985-06-01",
                  "1999-06-19", "2004-10-01", "2015-01-01"))
      ) &
      identical(
        as_date_ecotox(char_date, nr = 6L),
        as.Date(c("1987-05-19", "2021-05-01", "", "1985-01-01",
                  "1999-01-19", "2004-10-01", "2015-06-06"))
      )
  })
})
