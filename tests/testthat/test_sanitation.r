library(dplyr, quietly = TRUE)

check_db <- function() {
  if (!check_ecotox_availability()) {
    skip("ECOTOX database not available")
  }
}

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

test_that("Text is sanitised to units as expected", {
  expect_true({
    x <-
      as_unit_ecotox(c("C", "K"), type = "media") |>
      lapply(units::deparse_unit) |>
      unlist()
    all(x == c("°C", "K"))
  })
})

test_that("Converting numerics with units works as expected", {
  expect_equal({
    tibble(
      conc1_mean = c("1", "2", "5e-4", "0.2"),
      conc1_unit = c("mg/L", "M", "% w/v", "ppt w/v")
    ) |>
      process_ecotox_numerics(add_units = TRUE) |>
      mutate(
        conc1_mean_standard = mixed_to_single_unit(conc1_mean, "ug/L")) |>
      pull("conc1_mean_standard") |>
      as.numeric()
  }, c(1e+03, NA_real_, 5e+03, 2e+05))
})

test_that("Sanitising what is already is sanitised returns as is", {
  expect_true({
    as_numeric_ecotox(1) == 1 &&
      as_date_ecotox(as.Date("2025-03-20")) == as.Date("2025-03-20") &
      as_unit_ecotox(units::mixed_units(1, "mg/L")) ==
      units::mixed_units(1, "mg/L")
  })
})

test_that("Locally converted units correspond well with online standards", {
  check_db()
  skip_if_offline("cfpub.epa.gov/ecotox/")
  skip_on_cran()
  expect_true({

    data_local <- search_ecotox(
      list(
        test_cas = list(
          terms          = "1912249",
          method         = "exact"
        ),
        endpoint = list(
          terms          = "LC50",
          method         = "exact"
        ),
        media_type = list(
          terms          = c("FW", "SW"),
          method         = "contains"
        )
      ),
      output_fields = c("species.latin_name", "results.conc1_mean",
                        "results.conc1_unit", "results.result_id")) |>
      suppressWarnings()
  
    data_online <-
      websearch_ecotox(
        list_ecotox_web_fields(
          txAdvancedChemicalEntries = "1912249",
          RBCHEMSEARCHTYPE          = "EXACT",
          cbResult_number = "Result+Number",
          sortfield = "AQ.RESULT_NUMBER",
          cbResultsGroup12a = "LC50",
          cbemwFresh = "emwFresh",
          cbemwSalt = "emwSalt")
      )
    
    compare <-
      left_join(
        data_local,
        data_online$`Aquatic-Export` |>
          select(
            result_id = "Result+Number",
            conc_standardised = "Conc 1 Mean (Standardized)"),
        by = "result_id"
      ) |>
      process_ecotox_numerics(add_units = TRUE, warn = FALSE) |>
      mutate(conc_package = mixed_to_single_unit(conc1_mean, "mg/L") |> as.numeric(),
             ratio = conc_standardised / conc_package) |>
      filter(!is.na(ratio))
    
    all(abs(compare$ratio - 1) < 1e-6)
  })
})