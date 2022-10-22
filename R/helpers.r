.add_tags <- function(x, sqlite) {
  if (missing(sqlite)) sqlite <- attributes(x)$database_file
  attributes(x)$date_created  <- Sys.Date()
  attributes(x)$created_with  <- sprintf("Package ECOTOXr v%s", utils::packageVersion("ECOTOXr"))
  attributes(x)$database_file <- sqlite
  return(x)
}

.fail_on_missing <- function(path = get_ecotox_path()) {
  test <- check_ecotox_availability(path)
  if (!test) {
    stop("No local database located. Download data first by calling 'download_ecotox_data()'")
  } else return(test)
}

.search_ecotox_lazy_get_result_ids <- function(search, dbcon) {
  method <- field <- wildcard <- collapse <- prefix <- terms <- term <- term_t <- NULL
  search_result <-
    lapply(search, as.data.frame, strings.as.factors = F) %>%
    dplyr::bind_rows(.id = "field") %>%
    dplyr::mutate(
      table = lapply(field, function(x) .db_specs$table[.db_specs$field_name %in% x])
    ) %>%
    tidyr::unnest("table") %>%
    dplyr::mutate(method   = if(!"method" %in% names(dplyr::cur_data())) NA else dplyr::cur_data()[["method"]],
                  method   = ifelse(is.na(method), "contains", method),
                  wildcard = ifelse(method == "contains", "%", ""),
                  collapse = ifelse(method == "contains", " OR ", ", "),
                  prefix   = ifelse(method == "contains",
                                    sprintf("\`%s\` LIKE ", field),
                                    sprintf("\`%s\` COLLATE NOCASE IN ", field)),
                  term_t   = paste(sprintf("'%s%s%s'", wildcard, terms, wildcard))
                  ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("field"))) %>%
    dplyr::group_modify(~{
      tab <- .db_specs$table[.db_specs$field_name == .y$field & .db_specs$primary_key != ""]
      if (length(tab) == 0 || nrow(.x) == 1) return(.x %>% dplyr::select(-"field"))
      if (length(tab) > 0 && "results" %in% tab) return(.x %>% dplyr::select(-"field") %>% dplyr::filter(table == "results"))
      return(.x %>% dplyr::select(-"field") %>% dplyr::filter(table %in% tab))
    }, .keep = TRUE) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("table", "method", "field")))) %>%
    dplyr::summarise(
      term = {
        sprintf(ifelse(method == "contains", "%s%s", "%s(%s)"), prefix, paste(term_t, collapse = collapse))
        },
      .groups = "keep") %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("table", "field")))) %>%
    dplyr::summarise(term = paste(term, collapse = " OR "), .groups = "keep") %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("table")))) %>%
    dplyr::summarise(term = paste(sprintf("(%s)", term), collapse = " OR "), .groups = "keep") %>%
    dplyr::rowwise() %>%
    dplyr::summarise(
      tbl   = list({
        get_fields <-
          .db_specs$field_name[.db_specs$table %in% table & (.db_specs$primary_key != "" | .db_specs$foreign_key != "")] %>%
          unique() %>%
          {function(x) {sprintf("`%s`", x)}}() %>%
          paste(collapse = ", ")
        dplyr::tbl(dbcon, dplyr::sql(sprintf("SELECT %s FROM '%s' WHERE %s", get_fields, table, term)))
      }), .groups = "keep") %>%
    dplyr::mutate(table_group = ifelse(any(c("species", "species_synonyms") %in% table), "specsyn", table)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("table_group"))) %>%
    dplyr::summarise(
      tbl   = if (length(table) > 1) list(do.call(dplyr::union, tbl)) else tbl,
      table = if (length(table) > 1) "species" else table[[1]]
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("table"))) %>%
    dplyr::group_map(~{
      #keep linking tables until result_ids are obtained
      count <- 0
      repeat {
        count <- count + 1
        if (count > 5) stop("Sorry could not build a query for the search you have specified.")
        cur_tab    <- .y$table
        cur_fields <- colnames(.x$tbl[[1]])
        if (any(c("result_id", "test_id") %in% cur_fields)) break
        referring <- .db_specs[.db_specs$table %in% c("results", "tests") &
                                 .db_specs$foreign_key == sprintf("%s(%s)", cur_tab, cur_fields),,drop = F]
        if (nrow(referring) > 0) {
          .y$table <- referring$table
          .x$tbl[[1]] <-
            dplyr::left_join(.x$tbl[[1]],
                             dplyr::tbl(dbcon, referring$table), structure(referring$field_name, names = cur_fields))
        }
      }
      return(.x$tbl[[1]] %>% select(dplyr::matches("^result_id$|^test_id$")))
    })

  test_results   <- search_result %>% purrr::map(~{!"result_id" %in% colnames(.)}) %>% unlist()
  result_results <- search_result[!test_results]
  if (length(result_results) > 0) result_results <- result_results %>%
    purrr::reduce(dplyr::inner_join, by = c("test_id", "result_id"))
  test_results   <- search_result[test_results]
  if (length(test_results) > 0)
      test_results <- test_results %>%
        purrr::reduce(dplyr::inner_join, by = c("test_id")) %>%
        dplyr::left_join(dplyr::tbl(dbcon, "results") %>% dplyr::select("result_id", "test_id"), "test_id")
  if (length(test_results) == 0) {
    search_result <- result_results
  } else if (length(result_results) == 0) {
    search_result <- test_results
  } else {
    search_result <- dplyr::intersect(test_results, result_results)
  }
  return(search_result)
}

.search_ecotox_lazy_append_fields <- function(dbcon, search_result, output_fields, compute) {
  con                  <- search_result[["src"]]$con
  output_fields        <- as.data.frame(do.call(rbind, strsplit(output_fields, ".", fixed = T)), stringsAsFactors = F)
  names(output_fields) <- c("table", "field")
  ## Duplicated field names need to be renamed to avoid ambiguous results, keep track of those in 'new_field'
  output_fields$new_field <- output_fields$field
  output_fields_tables <- unique(output_fields$table)
  ## Find out if there are any field names duplicated which are not used as joining keys:
  duplicated_fields    <- unique(sort(.db_specs$field_name[duplicated(.db_specs$field_name)]))
  duplicated_fields    <- duplicated_fields[
    duplicated_fields %in%
      .db_specs$field_name[.db_specs$primary_key == "" & .db_specs$foreign_key == ""]
  ]

  .process_lookups <- function(parent_tab, prefix = "") {
    REPLACE <- temp_code <- NULL # These are used in dplyr routines below but are not globally bound
    
    foreigns <- gsub("\\(code\\)", "",
                     unique(.db_specs$foreign_key[.db_specs$table == parent_tab & endsWith(.db_specs$foreign_key, "(code)")]))
    tabs <- .db_specs[
      (.db_specs$table == parent_tab & .db_specs$field_name %in% output_fields$field &
         endsWith(.db_specs$foreign_key, "(code)")) |
        .db_specs$table == parent_tab &
        grepl(paste0(foreigns[foreigns %in% output_fields$table], "\\(",collapse="|"), .db_specs$foreign_key),,
      drop = F]
    tabs$foreign_tab <- gsub("\\(code\\)", "", tabs$foreign_key)
    
    .db_specs[
      .db_specs$table == parent_tab &
        startsWith(.db_specs$foreign_key, paste0(foreigns[foreigns %in% output_fields$table], "(")),,drop=F]
    if (nrow(tabs) > 0) {
      for (tab in unique(tabs$foreign_tab)) {
        flds <- tabs$field_name[tabs$foreign_tab == tab]
        for (fld in flds) {
          get_idx <- function(f) {output_fields$table == tab & output_fields$field == f}
          if (any(gsub(prefix, "", output_fields$new_field[get_idx("code")]) != output_fields$field[get_idx("code")])) {
            output_fields <<- dplyr::bind_rows(
              output_fields,
              data.frame(field = "code", table = tab, new_field = paste0(prefix, fld))
            )
          } else {
            output_fields$new_field[get_idx("code")] <<- paste0(prefix, fld)
          }
          
          descr <- paste0(fld, "_description_")
          if (any(gsub(prefix, "", output_fields$new_field[get_idx("description")]) !=
                  output_fields$field[get_idx("description")])) {
            output_fields <<- dplyr::bind_rows(
              output_fields,
              data.frame(field = "description", table = tab, new_field = paste0(prefix, descr))
            )
          } else {
            output_fields$new_field[get_idx("description")] <<- paste0(prefix, descr)
          }
          
          if (nrow(output_fields[output_fields$table == tab,]) > 0) {
            result <<- result %>%
              ## Forward slash should be ignored when joining lookup values:
              dplyr::mutate(temp_code = REPLACE(!! rlang::sym(paste0(prefix, fld)), "/", "")) %>%
              dplyr::left_join(
                tbl(con, tab) %>%
                  dplyr::rename(temp_code = "code", !!paste0(prefix, descr) := "description"),
                "temp_code") %>%
              dplyr::select(-temp_code)
            if (compute) result <<- result %>% dplyr::compute()
          }
        }
      }
    }
    return(NULL)
  }
  
  result <- search_result
  if (compute) result <- result %>% dplyr::compute()
  ## start with results table as this is needed for obtaining dose link ids

  results_output <- union(output_fields$field[output_fields$table == "results"], "result_id")
  dose_tables    <- c("dose_responses", "dose_response_details", "doses", "dose_response_links", "dose_stat_method_codes")
  if (any(dose_tables %in% output_fields_tables)) {

    out_rename <- output_fields$table %in% dose_tables & !output_fields$field %in% c("test_id", "result_id")
    output_fields$new_field[out_rename] <- paste0("dose_link_", output_fields$field[out_rename])
    result <-
      result %>%
      dplyr::left_join(dplyr::tbl(con, "dose_response_links"),   "result_id") %>%
      dplyr::left_join(dplyr::tbl(con, "dose_responses"),      c("dose_resp_id", "test_id")) %>%
      dplyr::left_join(dplyr::tbl(con, "dose_response_details"), "dose_resp_id") %>%
      dplyr::left_join(dplyr::tbl(con, "doses"),               c("dose_id", "test_id")) %>%
      dplyr::rename_with(~paste0("dose_link_", .), !dplyr::any_of(c("test_id", "result_id")))
    .process_lookups("dose_response_links")
    .process_lookups("doses", "dose_link_")
  }
  if (any(c("media_characteristics", "organic_matter_type_codes") %in% output_fields_tables)) {
    foreigns <- .db_specs$field_name[.db_specs$table == "media_characteristics" & .db_specs$foreign_key != ""]
    result <-
      result %>%
      dplyr::left_join(dplyr::tbl(con, "media_characteristics"), "result_id") %>%
      dplyr::select(dplyr::any_of(union(
        colnames(result),
        union(
          foreigns,
          output_fields$field[output_fields$table == "media_characteristics"]
        )
      )))
    .process_lookups("media_characteristics")
  }

  result_foreigns <- data.frame(
    table  = gsub("\\(.*?\\)", "",
                 .db_specs$foreign_key[.db_specs$foreign_key != "" & .db_specs$table == "results"]),
    field  = .db_specs$field_name[.db_specs$foreign_key != "" & .db_specs$table == "results"]
  ) %>% dplyr::filter(!table %in% c("tests", "results"))

  results_output <- union(results_output, c("result_id", result_foreigns$field))
  result <- result %>%
    dplyr::left_join(dplyr::tbl(con, "results") %>%
                       dplyr::select(dplyr::any_of(union(c("result_id", "test_id"), results_output))) %>%
                       dplyr::rename_with(function(x) paste0("result_", x),
                                          dplyr::any_of(c("created_date", "modified_date", "additional_comments"))),
                     c("result_id", "test_id"))
  output_fields$new_field[output_fields$table == "results" & output_fields$field == "created_date"] <-
    "result_created_date"
  output_fields$new_field[output_fields$table == "results" & output_fields$field == "modified_date"] <-
    "result_modified_date"
  output_fields$new_field[output_fields$table == "results" & output_fields$field == "additional_comments"] <-
    "result_additional_comments"
  
  if (compute) result <- result %>% dplyr::compute()
  
  .process_lookups("results")

  ## continue with linking all requested tables to the test data

  if ("chemical_carriers" %in% output_fields_tables) {
    car_sel <- output_fields$table == "chemical_carriers" & !output_fields$field %in% c("carrier_id", "test_id")
    output_fields$new_field[car_sel] <- paste0("carrier_", output_fields$new_field[car_sel])
    result <-
      result %>%
      dplyr::left_join(dplyr::tbl(con, "chemical_carriers") %>%
                         dplyr::rename_with(~paste0("carrier_", .), !dplyr::any_of(c("carrier_id", "test_id"))),
                       "test_id") %>%
      dplyr::select(dplyr::any_of(union(
        colnames(result),
        output_fields$new_field[output_fields$table == "chemical_carriers"]
      )))
  }

  test_foreigns <- data.frame(
    table  = gsub("\\(.*?\\)", "",
                  .db_specs$foreign_key[.db_specs$foreign_key != "" & .db_specs$table == "tests"]),
    field  = .db_specs$field_name[.db_specs$foreign_key != "" & .db_specs$table == "tests"]
  ) %>% dplyr::filter(!table %in% c("tests", "results"))

  test_foreigns <- test_foreigns[test_foreigns$table %in% output_fields$table,,drop = F]
  test_foreigns$foreign <- .db_specs$field_name[.db_specs$primary_key != ""][
    match(test_foreigns$table, .db_specs$table[.db_specs$primary_key != ""])]
  
  required_test_fields <-
    union(output_fields$field[output_fields$table == "tests"], test_foreigns$field)
  
  result <- result %>%
    dplyr::left_join(dplyr::tbl(con, "tests") %>%
                       dplyr::select(dplyr::any_of(union("test_id", required_test_fields))) %>%
                       dplyr::rename_with(function(x) paste0("test_", x),
                                          dplyr::any_of(c("created_date", "modified_date", "additional_comments"))),
                     "test_id")
  output_fields$new_field[output_fields$table == "tests" & output_fields$field == "created_date"] <-
    "test_created_date"
  output_fields$new_field[output_fields$table == "tests" & output_fields$field == "modified_date"] <-
    "test_modified_date"
  output_fields$new_field[output_fields$table == "tests" & output_fields$field == "additional_comments"] <-
    "test_additional_comments"
  
  if (any(c("species", "species_synonyms") %in% output_fields_tables)) {
    species_fields <- output_fields$field[output_fields$table == "species"]
    species_syn_fields <- output_fields$field[output_fields$table == "species_synonyms"]
    result <- result %>%
      dplyr::left_join(
        dplyr::tbl(con, "species") %>% dplyr::select(union(species_fields, "species_number")),
        "species_number")
    if (length(species_syn_fields) > 0) {
      output_fields$new_field[output_fields$table == "species_synonyms" & output_fields$field == "latin_name"] <-
        "species_synonyms"
      result <- result %>%
        dplyr::left_join(
          dplyr::tbl(con, "species_synonyms") %>%
            dplyr::select(union(species_syn_fields, "species_number")) %>%
            dplyr::rename(species_synonyms = "latin_name"), ## TODO only if requested as output
          "species_number")
    }
  }
  
  # TODO perform line below only when group by results is set to true
  if ("species_synonyms" %in% colnames(result)) {
    result <- result %>%
      dplyr::group_by(dplyr::across(!dplyr::any_of("species_synonyms"))) %>%
      dplyr::summarise(dplyr::across(dplyr::any_of("species_synonyms"), ~GROUP_CONCAT(., "|")), .groups = "keep") %>%
      dplyr::ungroup()
  }
  
  if ("chemicals" %in% output_fields$table) {
    output_fields$new_field[output_fields$table == "chemicals" & output_fields$field == "cas_number"] <-
      "test_cas"
    output_fields$new_field[output_fields$table == "chemicals" & output_fields$field == "chemical_name"] <-
      "test_chemical"
    output_fields$new_field[output_fields$table == "chemicals" & output_fields$field == "ecotox_group"] <-
      "test_chemical_group"
    result <- result %>%
      dplyr::left_join(
        dplyr::tbl(con, "chemicals") %>%
          dplyr::select(union("cas_number", output_fields$field[output_fields$table == "chemicals"])) %>%
          dplyr::rename_with(
            function(x){
              c("test_cas", "test_chemical", "test_chemical_group")[match(x, c("cas_number", "chemical_name", "ecotox_group"))]
            }, dplyr::any_of(c("cas_number", "chemical_name", "ecotox_group"))),
        "test_cas")
  }
  
  if ("references" %in% output_fields$table) {
    result <- result %>%
      dplyr::left_join(
        dplyr::tbl(con, "references") %>%
          dplyr::select(union("reference_number", output_fields$field[output_fields$table == "references"])),
        "reference_number"
      )
  }
  
  ## join remaining lookup values
  .process_lookups("tests")
  
  renamed <- output_fields$field != output_fields$new_field
  message(
    crayon::white(
      paste(
        sprintf("'%s.%s' was renamed '%s'",
                output_fields$table[renamed],
                output_fields$field[renamed],
                output_fields$new_field[renamed]),
        collapse = "\n")
    )
  )
  
  result <- result %>% dplyr::select(dplyr::any_of(union(unique(output_fields$new_field), "result_id")))
  return(result)
}

.group_nest_results <- function(search_result) {
  if (nrow(search_result) > 0) {
    group_by <-
      search_result %>%
      dplyr::group_by(dplyr::across(dplyr::any_of("result_id"))) %>%
      dplyr::summarise_all(~all(is.na(.)) || all(.[[1]] == .)) %>%
      dplyr::ungroup() %>%
      select( tidyselect::vars_select_helpers$where(~ all(.))) %>%
      colnames()
    if (all(colnames(search_result) %in% group_by)) return (search_result) else{
      search_result <-
        search_result %>%
        dplyr::group_nest(dplyr::across(dplyr::any_of(group_by)), .key = "nested_data")
    }
  } else search_result
}
