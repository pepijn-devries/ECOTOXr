check_db <- function() {
  if (!check_ecotox_availability()) {
    skip("ECOTOX database not available")
  }
}

simple_search1 <- if (check_ecotox_availability()) {
  suppressMessages(suppressWarnings(search_ecotox(
    list(latin_name = list(terms = "Daphnia magna"), chemical_name = list(terms = "benzene")),
    c(list_ecotox_fields(), "results.result_id", "results.test_id", "tests.reference_number"),
    compute = TRUE)))
} else NULL

simple_search2 <- if (check_ecotox_availability()) {
  suppressMessages(suppressWarnings({search_ecotox(list(test_id = list(terms = "1", method = "exact")))}))
} else NULL

simple_search3 <- if (check_ecotox_availability()) {
  suppressMessages(suppressWarnings({search_ecotox(list(latin_name = list(terms = "perdix perdix"),
                                                       test_cas = list(terms = "1336363")),
  c(list_ecotox_fields(), "results.result_id", "results.test_id", "tests.reference_number"))}))
} else NULL

search_q <- if (check_ecotox_availability()) {
  suppressMessages(suppressWarnings({search_query_ecotox(list(test_id = list(terms = "1", method = "exact")))}))
} else NULL

throws_errors <- function(expression) {
  result <- FALSE
  tryCatch(expression, error = function(e) {result <<- T}, warning = function(w) {invisible(NULL)})
  result
}

#################################
#################################
####                         ####
####         TEST 01         ####
####                         ####
#################################
#################################


test_that("All tables in the database are specified", {
  check_db()
  expect_true({
    dbcon <- suppressWarnings(dbConnectEcotox())
    test <- all(dbListTables(dbcon) %in% ECOTOXr:::.db_specs$table)
    dbDisconnectEcotox(dbcon)
    test
  })
})

#################################
#################################
####                         ####
####         TEST 02         ####
####                         ####
#################################
#################################

test_that("All specified tables are in the database", {
  check_db()
  expect_true({
    dbcon            <- suppressWarnings(dbConnectEcotox())
    tables_not_in_db <- ECOTOXr:::.db_specs$table[!ECOTOXr:::.db_specs$table %in% dbListTables(dbcon)]
    e_file           <- suppressWarnings({get_ecotox_sqlite_file()})
    e_date           <- as.Date(substr(e_file, nchar(e_file) - 16, nchar(e_file) - 7), format = "%m_%d_%Y")
    test             <-
      length(tables_not_in_db) == 0 ||
      (all(tables_not_in_db %in% c("length_unit_codes", "length_unit_codes", "length_type_codes", "length_type_codes")) &&
                                  e_date <= as.Date("2023-06-15"))
    dbDisconnectEcotox(dbcon)
    test
  })
})

#################################
#################################
####                         ####
####         TEST 03         ####
####                         ####
#################################
#################################

test_that("All fields in the database are specified", {
  check_db()
  expect_true({
    dbcon <- suppressWarnings(dbConnectEcotox())
    tables <- dbListTables(dbcon)
    test <- all(unlist(lapply(tables, function(tab) {
      all(dbListFields(dbcon, tab) %in% subset(ECOTOXr:::.db_specs, table == tab)$field_name)
    })))
    dbDisconnectEcotox(dbcon)
    test
  })
})

#################################
#################################
####                         ####
####         TEST 04         ####
####                         ####
#################################
#################################

test_that("All specified fields are in the database", {
  check_db()
  expect_true({
    dbcon <- suppressWarnings(dbConnectEcotox())
    tables <- dbListTables(dbcon)
    test <- all(unlist(lapply(tables, function(tab) {
      all(subset(ECOTOXr:::.db_specs, table == tab)$field_name %in% dbListFields(dbcon, tab))
    })))
    dbDisconnectEcotox(dbcon)
    test
  })
})

#################################
#################################
####                         ####
####         TEST 05         ####
####                         ####
#################################
#################################

test_that("Getting the path to the ECOTOX database doesn't throw errors", {
  expect_false(throws_errors({get_ecotox_path()}))
})

#################################
#################################
####                         ####
####         TEST 06         ####
####                         ####
#################################
#################################

test_that("Getting SQLite file location doesn't throw errors", {
  check_db()
  expect_false(throws_errors({get_ecotox_sqlite_file()}))
})

#################################
#################################
####                         ####
####         TEST 07         ####
####                         ####
#################################
#################################

test_that("A simple search results in expected table", {
  check_db()
  expect_true({
    ## Compare result with anticipated ids:
    all(
      c("1020021", "1020022", "1020023", "1022155", "1031085", "1031086", "1031087", "1031088", "1031196", "1031197",
        "1064409", "1072942", "1072943", "1072944", "1083684", "1083685", "1083686", "1098939", "1098940", "1098941",
        "1098942", "1098943", "1098944", "1098945", "1098946", "1098947", "1098948", "1098949", "1098950", "1125798",
        "1136665", "1136666", "1142641", "1152541", "1187783", "1189253", "1237724", "2113979", "2114101", "2194929") %in%
        simple_search1$test_id
    )
  })
})

#################################
#################################
####                         ####
####         TEST 08         ####
####                         ####
#################################
#################################

test_that("A simple search results in unique result ids", {
  check_db()
  expect_true({
    ## Compare result with anticipated ids:
    all(!duplicated(simple_search1$result_id))
  })
})

#################################
#################################
####                         ####
####         TEST 09         ####
####                         ####
#################################
#################################

test_that("In a simple search test that there is a publication year when there is a reference number.", {
  check_db()
  expect_false({
    any(is.na(simple_search1$publication_year) & !is.na(simple_search1$reference_number))
  })
})

#################################
#################################
####                         ####
####         TEST 10         ####
####                         ####
#################################
#################################

test_that("A simple search does not necessarily result in unique result ids when chemical carriers are added to output", {
  check_db()
  expect_false({
    results <- suppressWarnings(
      search_ecotox(
        list(test_id = list(terms = "1000260")),
        c("tests.test_id", "results.result_id", "chemical_carriers.carrier_id"),
        group_by_results = FALSE)
    )
    ## Compare result with anticipated ids:
    all(!duplicated(results$result_id))
  })
})

#################################
#################################
####                         ####
####         TEST 11         ####
####                         ####
#################################
#################################

test_that("Default field names are fewer than all field names", {
  expect_true({
    length(list_ecotox_fields("default")) < length(list_ecotox_fields("all"))
  })
})

#################################
#################################
####                         ####
####         TEST 12         ####
####                         ####
#################################
#################################

test_that("A simple search query returns a single element of type character", {
  check_db()
  expect_true({
    length(search_q) == 1 && typeof(search_q) == "character"
  })
})

#################################
#################################
####                         ####
####         TEST 13         ####
####                         ####
#################################
#################################

test_that("A query doesn't mistakenly returns field name as value", {
  check_db()
  expect_false({
    (is.null(simple_search2$test_grade) || all(simple_search2$test_grade == "test_grade"))
  })
})

#################################
#################################
####                         ####
####         TEST 14         ####
####                         ####
#################################
#################################

test_that("No duplicated results are returned when searching for test id", {
  check_db()
  expect_false({
    any(duplicated(simple_search2))
  })
})

#################################
#################################
####                         ####
####         TEST 15         ####
####                         ####
#################################
#################################

test_that("When multiple doses are linked to a result, no duplicates are returned", {
  check_db()
  expect_false({
    any(duplicated(simple_search3))
  })
})

#################################
#################################
####                         ####
####         TEST 16         ####
####                         ####
#################################
#################################

test_that("get_ecotox_info doesn't throw an error.", {
  expect_false({ throws_errors(suppressMessages(get_ecotox_info())) })
})

### Additional tests
test_that("Listing fields works for different scopes", {
  expect_true({
    all(list_ecotox_fields("extended") %in% list_ecotox_fields("full"))
  })
})

test_that("Citation file is created correctly", {
  expect_no_error({
    dest_path <- file.path(tempdir(), "ecotox_ascii_12_12_2024.zip")
    dummy_file <- file.path(tempdir(), "ecotox_ascii_12_12_2024.sqlite")
    file.create(dummy_file)
    ECOTOXr:::.write_citation(dest_path, "foobar_12_12_2024.zip")
    cite_ecotox(tempdir(), "12_12_2024")
    file.remove(gsub(".zip", "_cit.txt", dest_path), dummy_file)
  })
})

test_that("Can request citation", {
  expect_no_error({
    cite_ecotox() |>
      suppressMessages() |>
      suppressWarnings()
  })
})
