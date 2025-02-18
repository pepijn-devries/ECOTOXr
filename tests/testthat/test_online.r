library(dplyr, quietly = TRUE) |> suppressWarnings() |> suppressMessages()

check_db <- function() {
  if (!check_ecotox_availability()) {
    skip("ECOTOX database not available")
  }
}

test_that("Online and local search yield the same results", {
  check_db()
  skip_if_offline()
  expect_true({
    load(file.path(testthat::test_path(), "test_data", "insecticides.rdata"))
    insecticides$cas <- format(as.cas(insecticides$cas), hyphenate = FALSE)
    unit_conversion <-
      data.frame(what       = c(rep("mass", 8), rep("volume", 2)),
                 unit       = c("pg", "ng", "ug", "mg", "g", "nmol", "umol", "mmol", "L", "m3"),
                 conversion = 10^c(-12, -9, -6, -3, 0, -9, -6, -3, 0, 3))
    
    insecticedes_search <- search_ecotox(
      list(
        test_cas   = list(terms = insecticides$cas,  method = "exact"),
        endpoint   = list(terms = c("EC50", "LC50"), method = "contains"),
        latin_name = list(terms = "Daphnia magna",   method = "exact"),
        effect     = list(terms = c("ITX", "MOR"),   method = "contains")
      ),
      c(list_ecotox_fields(), "results.obs_duration_mean", "results.obs_duration_unit",
        "results.result_id")) |>
      mutate(
        duration_corr = case_match(
          obs_duration_unit, "d" ~ 1, "h" ~ 1/24, "mi" ~ 1/(60*24), "NR" ~ NA, "wk" ~ 7),
        duration_corr = suppressWarnings(as.numeric(obs_duration_mean)*duration_corr),
        test_cas      = as.character(ECOTOXr::as.cas(test_cas)),
        conc1_mean    = suppressWarnings({as.numeric(gsub("[*]", "", conc1_mean))})
      ) |>
      filter(duration_corr == 2 & conc1_mean != "NR" & !grepl("org", conc1_unit)) |>
      left_join(insecticides |> distinct(), c(test_cas = "cas")) |>
      mutate(
        conc1_unit_fix = trimws(gsub("AI", "", conc1_unit)),
        conc1_unit_fix =
          case_match(
            conc1_unit_fix,
            "mM" ~ "mmol/L",
            "uM" ~ "umol/L",
            "nM" ~ "nmol/L",
            "mg/kg" ~ "mg/L",
            "ppm" ~ "mg/L",
            "ppb" ~ "ug/L",
            "ppt" ~ "ng/L",
            .default = conc1_unit_fix),
        conc1_conversion_factor = {
          do.call(rbind, strsplit(conc1_unit_fix, "/")) |>
            as.data.frame() |>
            rename_with(~c("mass", "volume")) |>
            left_join(unit_conversion, c(mass = "unit")) |>
            rename(mass_conversion   = "conversion") |>
            mutate(
              ## If mass is reported as volume (1 case) use specific gravity to convert to actual mass
              mass_conversion = ifelse(mass == "ul" & test_cas == "333-41-5", 1.117e-6, mass_conversion)
            ) |>
            left_join(unit_conversion, c(volume = "unit")) |>
            rename(volume_conversion = "conversion") |>
            mutate(molar_conversion  = ifelse(grepl("mol", mass), molweight, 1),
                   total_conversion  = molar_conversion*mass_conversion/volume_conversion) |>
            pull(total_conversion)
        },
        conc1_ug_l = 1e6*conc1_mean*conc1_conversion_factor
      ) |>
      suppressWarnings() |>
      suppressMessages()
    
    websearch <- list_ecotox_web_fields(
      txAdvancedChemicalEntries   = paste(insecticides$cas,
                                          collapse = "\r\n"),
      RBCHEMSEARCHTYPE            = "EXACT",
      txAdvancedSpecEntries       = "daphnia magna",
      RBSPECSEARCHTYPE            = "EXACT",
      cbResultsGroup12a           = "LC50",
      cbResultsGroup13a           = "EC50",
      cbResultsGroup6             = "MOR",
      cbResultsGroup7c            = "ITX",
      txExposureDurationStd       = "2",
      cbResult_number             = "Result Number")
    
    websearch <- suppressWarnings(websearch_ecotox(websearch))
    websearch <- websearch$`Aquatic-Export` |>
      dplyr::filter(!is.na(`Conc 1 Mean (Standardized)`) &
                      `Conc 1 Units (Standardized)` == "AI mg/L") |>
      select(result_id  = "Result Number",
             conc1_ug_l = "Conc 1 Mean (Standardized)",
             test_cas   = "CAS Number") |>
      mutate(test_cas   = as.character(as.cas(test_cas)), ## hyphenate the CAS numbers
             conc1_ug_l = 1e3*conc1_ug_l)
    conc_check <-
      full_join(
        websearch |>
          select(web_conc = "conc1_ug_l", "result_id"),
        insecticedes_search |>
          select(local_conc = "conc1_ug_l", "result_id"),
        by = "result_id"
      ) |>
      mutate(
        diff = 1 - web_conc/local_conc,
        check = diff < 1e-3
      )
    result <-
      ## Number of records differ no more than 2
      (abs(nrow(websearch) - nrow(insecticedes_search)) <= 2) &&
      ## Retrieved websearch cas numbers are also in local cas numbers
      all(websearch$test_cas %in% insecticedes_search$test_cas) &&
      ## Retrieved local cas numbers are also in websearch cas numbers
      all(insecticedes_search$test_cas %in% websearch$test_cas) &&
      ## concentrations are identical
      all(na.omit(conc_check$check))
    result
  })
})

test_that("Can get download URL", {
  skip_if_offline()
  expect_true({
    get_ecotox_url(verify_ssl = FALSE) |> endsWith(".zip")
  })
})

test_that("Download from EPA ECOTOX starts", {
  skip_if_offline()
  expect_true({
    tryCatch({
      ## 'maxfilesize' is set such that it can obtain the download link, but
      ## the database itself cannot be downloaded
      download_ecotox_data(tempdir(), verify_ssl = FALSE, ask = FALSE, maxfilesize = 10000) |>
        suppressMessages()
      TRUE
    }, error = function(e) {
      endsWith(e$request$url, ".zip")
    })
  })
})

# TODO test disabled for debugging purposes
# test_that("Websearch returns results", {
#   skip_if_offline()
#   expect_true({
#     search_fields <-
#       list_ecotox_web_fields(
#         txAdvancedSpecEntries     = "daphnia magna",
#         RBSPECSEARCHTYPE          = "EXACT",
#         txAdvancedChemicalEntries = "benzene",
#         RBCHEMSEARCHTYPE          = "EXACT")
#     search_results   <- websearch_ecotox(search_fields, verify_ssl = FALSE)
#     comptox_results  <- websearch_comptox(c("benzene", "108-88-3"), verify_ssl = FALSE)
#     comptox_results2 <- websearch_comptox("100", inputType = "MASS", massError = 5, verify_ssl = FALSE)
#     nrow(search_results$`Aquatic-Export`) > 10L && length(comptox_results) > 0L &&
#       length(comptox_results2) > 0L
#   })
# })
