#' Search and retrieve toxicity records from the on-line database
#'
#' `r lifecycle::badge('experimental')` Functions to search and retrieve records from the on-line database at
#' <https://www.epa.gov/ecotox/search.cfm>.
#'
#' The functions described here to search and retrieve records from the on-line database are experimental. This is because this feature is
#' not formally supported by the EPA, and it may break in future iterations of the on-line database. The functions form an interface between
#' R and the ECOTOX website and is therefore limited by its restrictions as described in the package documentation: [ECOTOXr::ECOTOXr]. The
#' functions should therefore be used with caution.
#' 
#' @note **IMPORTANT:** when you plan to perform multiple adjacent searches (for instance in a loop), please insert a call to [Sys.sleep()].
#' This to avoid overloading the server and getting you IP address banned from the server.
#'
#' @param fields A named `list` of `character`s, used to build a search for for the on-line search query of <https://www.epa.gov/ecotox/search.cfm>.
#' Use [list_ecotox_web_fields()] to construct a valid list.
#' @param habitat Use `aquire` (default) to retrieve aquatic data, `terrestrial` for, you've guessed it, terrestrial data.
#' @param ... In case of [list_ecotox_web_fields()] the dots can be used as search field values used to update the returned list of fields.
#' 
#' In case of [websearch_ecotox()] the dots can be used to pass custom options to the underlying [httr::POST()] call. For available
#' field names, use `names(list_ecotox_web_fields())`
#' @return Returns named `list` of [dplyr::tibble]s with search results. Results are unpolished and `as is' returned by EPA's web service.
#' 
#' [list_ecotox_web_fields()] returns a named list with fields that can be used in a web search of EPA's ECOTOX database, using
#' [websearch_ecotox()].
#' @rdname websearch
#' @name websearch_ecotox
#' @examples
#' \dontrun{
#' search_fields <-
#'   list_ecotox_web_fields(
#'     txAdvancedSpecEntries     = "daphnia magna",
#'     RBSPECSEARCHTYPE          = "EXACT",
#'     txAdvancedChemicalEntries = "benzene",
#'     RBCHEMSEARCHTYPE          = "EXACT")
#' search_results <- websearch_ecotox(search_fields)
#' }
#' @author Pepijn de Vries
#' @family online-search-functions
#' @family search-functions
#' @export
websearch_ecotox <- function(fields = list_ecotox_web_fields(), habitat = c("aquire", "terrestrial"), ...) {
  habitat <- match.arg(habitat)
  
  ## download preview
  search_post <-
    httr::POST(sprintf("https://cfpub.epa.gov/ecotox/data/search_handler.cfm?sub=%s&type=", habitat),
               body = fields, encode = "form", ...)
  .check_http_status(search_post, "Failed to send search query")
  search_response <-  rvest::read_html(search_post)
  warnings <- search_response %>% rvest::html_element(xpath = "//div[@class='callout alert']") %>% rvest::html_text2()
  if (!is.na(warnings)) stop(warnings)
  search_response <- jsonlite::parse_json(search_post$content %>% rawToChar())
  headers <- lapply(search_response$headers, `[[`, 1) %>% unlist()
  table_preview <- search_response$records %>% lapply(structure, names = headers) %>% lapply(dplyr::as_tibble) %>% dplyr::bind_rows()
  
  ## Download excel report
  httr_result <-
    httr::POST(sprintf("https://cfpub.epa.gov/ecotox/data/search_handler.cfm?sub=%s&type=excel", habitat),
               body = fields, encode = "form", ...)
  
  .check_http_status(httr_result, "Failed to send search query")
  
  ## Return preview when Excel download has failed
  if (!grepl("spreadsheet", httr_result$headers$`content-type`)) {
    warn_text <- httr_result %>% rvest::read_html() %>% rvest::html_text2() %>% stringr::str_replace("Warning", "") %>% trimws()
    warn_text <- paste(warn_text, "Returning on-line preview data only")
    warning(warn_text)
    return(list(`On-line preview` = table_preview))
  }
  
  ## otherwise return full data
  tab_file <- tempfile(fileext = ".xlsx")
  writeBin(httr_result$content, tab_file)
  sheet_names <- readxl::excel_sheets(tab_file)
  data_tables <-
    suppressMessages(
      structure(
        lapply(sheet_names, function(name) {
          readxl::read_excel(tab_file, name, col_names = if(name == "Cover Sheet") c("Search property", "Result") else TRUE)
        }),
        names = sheet_names
      )
    )
  data_tables[["On-line preview"]] <- table_preview
  unlink(tab_file)
  
  return(data_tables)
}

#' @rdname websearch
#' @name list_ecotox_web_fields
#' @export
list_ecotox_web_fields <- function(...){
  # search FORM data as obtained from www.epa.gov/ecotox/search.cfm :
  form_data <- "cbBCF=BCF+1+Value+Op+%7C+BCF+1+Value+%7C+BCF+1+Min+Op+%7C+BCF+1+Min+%7C+BCF+1+Max+Op+%7C+BCF+1+Max+%7C+BCF+1+Unit+%7C+BCF+2+Value+Op+%7C+BCF+2+Value+%7C+BCF+2+Min+Op+%7C+BCF+2+Min+%7C+BCF+2+Max+Op+%7C+BCF+2+Max+%7C+BCF+2+Unit+%7C+BCF+3+Value+Op+%7C+BCF+3+Value+%7C+BCF+3+Min+Op+%7C+BCF+3+Min+%7C+BCF+3+Max+Op+%7C+BCF+3+Max+%7C+BCF+3+Unit&cbCAS_number_name=CAS+Number+%7C+Chemical+Name&cbMethod=Chemical+Analysis&cbChemical_grade=Chemical+Grade&cbChemical_purity=Chemical+Purity+%7C+Chemical+Purity+Mean+Op+%7C+Chemical+Purity+Mean(%25)+%7C+Chemical+Purity+Min+Op+%7C+Chemical+Purity+Min(%25)+%7C+Chemical+Purity+Max+Op+%7C+Chemical+Purity+Max(%25)&cbConcentration_standard=Conc+1+Type+(Standardized)+%7C+Conc+1+Mean+Op+(Standardized)+%7C+Conc+1+Mean+(Standardized)+%7C+Conc+1+Min+Op+(Standardized)+%7C+Conc+Min+1+(Standardized)+%7C+Conc+1+Max+Op+(Standardized)+%7C+Conc+1+Max+(Standardized)+%7C+Conc+1+Units+(Standardized)+%7C+Conc+2+Type+(Standardized)+%7C+Conc+2+Mean+Op+(Standardized)+%7C+Conc+2+Mean+(Standardized)+%7C+Conc+2+Min+Op+(Standardized)+%7C+Conc+Min+2+(Standardized)+%7C+Conc+2+Max+Op+(Standardized)+%7C+Conc+2+Max+(Standardized)+%7C+Conc+2+Units+(Standardized)+%7C+Conc+3+Type+(Standardized)+%7C+Conc+3+Mean+Op+(Standardized)+%7C+Conc+3+Mean+(Standardized)+%7C+Conc+3+Min+Op+(Standardized)+%7C+Conc+Min+3+(Standardized)+%7C+Conc+3+Max+Op+(Standardized)+%7C+Conc+3+Max+(Standardized)+%7C+Conc+3+Units+(Standardized)&cbEffect=Effect&cbEffectMeas=Effect+Measurement&cbEndpoint=Endpoint&cbExposure_type=Exposure+Type&cbMediatype=Media+Type&cbNumDoses=Number+of+Doses&cbObserved_duration_std=Observed+Duration+(Days)+%7C+Observed+Duration+Mean+Op+(Days)+%7C+Observed+Duration+Mean+(Days)+%7C+Observed+Duration+Min+Op+(Days)+%7C+Observed+Duration+Min+(Days)+%7C+Observed+Duration+Max+Op+(Days)+%7C+Observed+Duration+Max+(Days)+%7C+Observed+Duration+Units+(Days)&cbOrganism_age=Organism+Age+%7C+Organism+Age+Mean+Op+%7C+Organism+Age+Mean+%7C+Organism+Age+Min+Op+%7C+Organism+Age+Min+%7C+Organism+Age+Max+Op+%7C+Organism+Age+Max+%7C+Age+Units&cbOrganism_lifestage=Organism+Lifestage&cbReference_Citation=Author+%7C+Reference+Number+%7C+Title+%7C+Source+%7C+Publication+Year&cbResponse_Site=Response+Site&cbSpecies_group=Species+Group&cbSpecies_latin_common_name=Species+Scientific+Name+%7C+Species+Common+Name&cbLocation_test=Test+Location&txAdvancedChemicalEntries=&RBCHEMSEARCHTYPE=CONTAINS&txAdvancedEffectEntries=&RBEFFECTSEARCHTYPE=CONTAINS&&txAdvancedSpecEntries=&RBSPECSEARCHTYPE=CONTAINS&rbSpecSearchKing=BOTH&rbSpecSearchGroup=SCIENTIFICNAME&specSelections=&txExposureDurationStd=&txExposureDurationMin=&txExposureDurationMax=&Starting_Publication_Year=1915&Ending_Publication_Year=2022&txAdvancedAuthorsEntries=&txAdvancedPublicationsEntries=&length=20&start=0"
  
  form_data <- strsplit(form_data, "&")[[1]]
  form_data <- form_data[form_data != ""]
  form_data <- do.call(c, lapply(form_data, function(x) {
    x <- strsplit(x[[1]], "=")[[1]]
    structure(ifelse(is.na(x[2]), "", gsub("[+]", " ", utils::URLdecode(x[2]))), names = x[1])
  })) %>% as.list()
  form_data[names(c(...))] <- c(...)
  return(form_data)
}

#' Search and retrieve substance information from <https://comptox.epa.gov/dashboard>
#'
#' `r lifecycle::badge('experimental')` Search <https://comptox.epa.gov/dashboard> for substances and their chemico-physical properties
#' and meta-information.
#'
#' The [CompTox Chemicals Dashboard](https://comptox.epa.gov/dashboard) is a freely accessible on-line U.S. EPA database.
#' It contains information on physico-chemical properties, environmental fate and transport, exposure, usage, *in vivo* toxicity,
#' and *in vitro* bioassay of a wide range of substances.
#' 
#' The function described here to search and retrieve records from the on-line database is experimental. This is because this feature is
#' not formally supported by the EPA, and it may break in future incarnations of the on-line database. The function forms an interface between
#' R and the [CompTox](https://comptox.epa.gov) website and is therefore limited by the restrictions documented there.
#' @param searchItems A `vector` of `character`s where each element is a substance descriptor (any of the selected `identifierType`s) you
#' wish to query.
#' @param identifierTypes Substance identifiers for searching CompTox. Only used when `inputType` is set to `"IDENTIFIER"`.
#' @param inputType Type of input used for searching CompTox. See usage section for valid entries.
#' @param downloadItems Output fields of CompTox data for requested substances
#' @param massError Error tolerance when searching for substances based on their monoisotopic mass. Only used for `inputType = "MASS"`.
#' @param timeout Time in seconds (default is 300 secs), that the routine will wait for the download link to get ready.
#' It will throw an error if it takes longer than the specified `timeout`.
#' @return Returns a named `list` of [dplyr::tibble]s containing the search results for the requested output tables and fields.
#' Results are unpolished and `as is' returned by EPA's web service.
#' @param ... Arguments passed on to [httr::GET] requests.
#' @rdname websearch_comptox
#' @name websearch_comptox
#' @examples
#' \dontrun{
#' ## search for substance name 'benzene' and CAS registration number 108-88-3
#' ## on https://comptox.epa.gov/:
#' comptox_results <- websearch_comptox(c("benzene", "108-88-3"))
#' 
#' ## search for substances with monoisotopic mass of 100+/-5:
#' comptox_results2 <- websearch_comptox("100", inputType = "MASS", massError = 5)
#' }
#' @author Pepijn de Vries
#' @family onlinesearch-functions
#' @family search-functions
#' @export
websearch_comptox <- function(
    searchItems,
    identifierTypes = c("chemical_name", "CASRN", "INCHIKEY", "dtxsid"),
    inputType       = c("IDENTIFIER", "DTXCID", "INCHIKEY_SKELETON", "MSREADY_FORMULA", "EXACT_FORMULA", "MASS"),
    downloadItems   = c("DTXCID", "CASRN", "INCHIKEY", "IUPAC_NAME", "SMILES", "INCHI_STRING", "MS_READY_SMILES",
                        "QSAR_READY_SMILES", "MOLECULAR_FORMULA", "AVERAGE_MASS", "MONOISOTOPIC_MASS",
                        "QC_LEVEL", "SAFETY_DATA", "EXPOCAST", "DATA_SOURCES", "TOXVAL_DATA",
                        "NUMBER_OF_PUBMED_ARTICLES", "PUBCHEM_DATA_SOURCES", "CPDAT_COUNT", "IRIS_LINK",
                        "PPRTV_LINK", "WIKIPEDIA_ARTICLE", "QC_NOTES", "ABSTRACT_SHIFTER", "TOXPRINT_FINGERPRINT",
                        "ACTOR_REPORT", "SYNONYM_IDENTIFIER", "RELATED_RELATIONSHIP", "ASSOCIATED_TOXCAST_ASSAYS",
                        "TOXVAL_DETAILS", "CHEMICAL_PROPERTIES_DETAILS", "BIOCONCENTRATION_FACTOR_TEST_PRED",
                        "BOILING_POINT_DEGC_TEST_PRED", "48HR_DAPHNIA_LC50_MOL/L_TEST_PRED", "DENSITY_G/CM^3_TEST_PRED",
                        "DEVTOX_TEST_PRED", "96HR_FATHEAD_MINNOW_MOL/L_TEST_PRED", "FLASH_POINT_DEGC_TEST_PRED",
                        "MELTING_POINT_DEGC_TEST_PRED", "AMES_MUTAGENICITY_TEST_PRED", "ORAL_RAT_LD50_MOL/KG_TEST_PRED",
                        "SURFACE_TENSION_DYN/CM_TEST_PRED", "THERMAL_CONDUCTIVITY_MW/(M*K)_TEST_PRED",
                        "TETRAHYMENA_PYRIFORMIS_IGC50_MOL/L_TEST_PRED", "VISCOSITY_CP_CP_TEST_PRED",
                        "VAPOR_PRESSURE_MMHG_TEST_PRED", "WATER_SOLUBILITY_MOL/L_TEST_PRED",
                        "ATMOSPHERIC_HYDROXYLATION_RATE_(AOH)_CM3/MOLECULE*SEC_OPERA_PRED", "BIOCONCENTRATION_FACTOR_OPERA_PRED",
                        "BIODEGRADATION_HALF_LIFE_DAYS_DAYS_OPERA_PRED", "BOILING_POINT_DEGC_OPERA_PRED",
                        "HENRYS_LAW_ATM-M3/MOLE_OPERA_PRED", "OPERA_KM_DAYS_OPERA_PRED",
                        "OCTANOL_AIR_PARTITION_COEFF_LOGKOA_OPERA_PRED", "SOIL_ADSORPTION_COEFFICIENT_KOC_L/KG_OPERA_PRED",
                        "OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED", "MELTING_POINT_DEGC_OPERA_PRED",
                        "OPERA_PKAA_OPERA_PRED", "OPERA_PKAB_OPERA_PRED", "VAPOR_PRESSURE_MMHG_OPERA_PRED",
                        "WATER_SOLUBILITY_MOL/L_OPERA_PRED", "EXPOCAST_MEDIAN_EXPOSURE_PREDICTION_MG/KG-BW/DAY",
                        "NHANES", "TOXCAST_NUMBER_OF_ASSAYS/TOTAL", "TOXCAST_PERCENT_ACTIVE"),
    massError = 0,
    timeout   = 300,
    ...) {
  search_form <-
    list(
      identifierTypes = match.arg(identifierTypes, several.ok = T),
      massError       = massError,
      downloadItems   = match.arg(downloadItems, several.ok = T),
      searchItems     = paste0(searchItems, collapse = "\n"),
      inputType       = match.arg(inputType),
      downloadType    = "EXCEL"
    )
  post_result <- httr::POST(
    "https://comptox.epa.gov/dashboard-api/batchsearch/export/?lb2ljny4",
    body   = search_form,
    encode = "json",
    httr::content_type("application/json")
  )
  .check_http_status(post_result, "Failed to post search query")
  
  ## Wait for download to get ready, by checking its status every second
  i <- 0
  repeat {
    search_status <- httr::GET(
      paste0("https://comptox.epa.gov/dashboard-api/batchsearch/export/status/", post_result$content %>% rawToChar()), ...)
    .check_http_status(search_status, "Failed to check download status")
    if ((search_status$content %>% rawToChar()) == "true") break
    i <- i + 1
    if (i == 30) warning("It is taking exceptionally long for preparing the download, you may wish to abort...")
    if (i == timeout) stop("Did not succeed before timeout, try again or increase the timeout...")
    Sys.sleep(1)
  }

  ## Download is ready, so let's go get it
  search_result <- httr::GET(
    paste0("https://comptox.epa.gov/dashboard-api/batchsearch/export/content/", post_result$content %>% rawToChar()),
    httr::content_type("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"), ...)
  .check_http_status(search_result, "Failed to obtain search result")
  tab_file <- tempfile(fileext = ".xlsx")
  writeBin(search_result$content, tab_file)
  sheet_names <- readxl::excel_sheets(tab_file)
  data_tables <- structure(
    lapply(sheet_names, function(name) {
      readxl::read_excel(tab_file, name, col_names = if(name == "Cover Sheet") c("Search property", "Result") else TRUE)
      }),
    names = sheet_names
  )
  unlink(tab_file)
  return(data_tables)
}

.check_http_status <- function(httr_object, message = "") {
  ## http status between 200 and 299 indicates success
  if (!dplyr::between(as.numeric(httr_object$status), 200, 299)) {
    stop(sprintf("%s. Http response %s status code %s\n\n%s",
                 message,
                 httr::content(httr_object)$title,
                 httr::content(httr_object)$status,
                 httr::content(httr_object)$detail))
  }
}