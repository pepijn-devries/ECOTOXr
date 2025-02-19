test_that("Function cas works and generates the correct length", {
  expect_length(cas(10), 10)
})

test_that("Converting either a numerical or a character to cas, results in the same object", {
  expect_identical(as.cas(71432L), as.cas("71432"))
})

test_that("Hyphenation can be omitted in CAS", {
  expect_identical(as.cas("71-43-2"), as.cas("71432"))
})

test_that("Concatenating cas objects works correctly", {
  expect_equal(length(c(as.cas(71432L), "71432")), 2)
})

test_that("is.cas doesn't throw errors", {
  expect_true(is.cas(c(as.cas(71432L), "71432")))
})

test_that("Extract and replace methods don't throw errors for cas objects", {
   expect_true({
     cas_data <- cas(100)
     cas_data[4:10]
     cas_data[[50]]
     cas_data[3:4]  <- as.cas(rep("71432", 2))
     cas_data[[50]] <- as.cas(11)
     TRUE
   })
})

test_that("as.data.frame.cas doesn't throw errors", {
  expect_true({
    "data.frame" %in% class(as.data.frame(c(cas(10), as.cas("71432"))))
  })
})

test_that("as.integer.cas returns an integer", {
  expect_type(as.integer(as.cas("71432")), "integer")
})

test_that("as.double.cas returns a double", {
  expect_type(as.double(as.cas("71432")), "double")
})

test_that("as.character.cas returns a character", {
  expect_type(as.character(as.cas("71432")), "character")
})

test_that("as.list.cas returns a list", {
  expect_type(as.list(as.cas(rep("71432", 4))), "list")
})

test_that("as.cas returns a cas", {
  expect_s3_class(as.cas("71432"), "cas")
})

test_that("Huge CAS to integer returns NA", {
  expect_true({
    huge_cas <- as.cas("9999999-99-5")
    my_int   <- as.integer(huge_cas) |> suppressWarnings()
    is.na(my_int)
  })
})

test_that("Simple S3 functions work", {
  expect_no_error({
    my_cas <- as.cas("71432")
    print.cas(my_cas)
    show.cas(my_cas)
  })
})