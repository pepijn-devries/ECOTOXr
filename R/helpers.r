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
  # TODO when searching multiple fields in the same table, combine those searches in 1 with 'and'
  # TODO when searching for test_id only, result_id is not correctly joined...
  search_results <- lapply(
    names(search),
    function(search_field) {
      srch <- search[[search_field]]
      search_tables <- .db_specs$table[.db_specs$field_name == search_field & .db_specs$primary_key != ""]

      if (length(search_tables) == 0) search_tables <- .db_specs$table[.db_specs$field_name == search_field]
      if (length(search_tables) == 0) stop(sprintf("Unknown search field: %s", search_field))
      species <- paste(search_tables[search_tables %in% c("species", "species_synonyms")], collapse = ";")
      search_tables <- search_tables[!search_tables %in% c("species", "species_synonyms")]
      if (species != "") search_tables <- c(search_tables, species)
      search_results <-
        lapply(search_tables, function(my_tbl) {
          if (!all(names(srch) %in% c("terms", "method")))
            stop("Each search field can only contain two elements: 'terms' and 'method'.")
          if (typeof(srch$terms) != "character" || length(srch$terms) == 0)
            stop("Provide at least 1 search term (type 'character')")
          method   <- match.arg(srch[["method"]], c("exact", "contains"))
          wildcard <- ifelse(method == "contains", "%", "")
          collapse <- ifelse(method == "contains", " OR ", ", ")
          prefix   <- ifelse(method == "contains", sprintf("\`%s\` LIKE ", search_field), "")
          terms    <- paste(sprintf("%s'%s%s%s'",
                                    prefix,
                                    wildcard,
                                    srch$terms,
                                    wildcard),
                            collapse = collapse)
          if (method == "exact")
            terms <- sprintf("`%s` COLLATE NOCASE IN (%s)", search_field, terms)
          get_fields    <- ""
          search_result <- NULL
          repeat {
            my_tbl <- strsplit(my_tbl, ";")[[1]]
            get_fields <-
              unique(.db_specs$field_name[.db_specs$table %in% my_tbl &
                                            (.db_specs$primary_key != "" | .db_specs$foreign_key != "")])
            if (is.null(search_result)) {
              search_result <- lapply(
                my_tbl, function(my_t) {
                  tbl(dbcon, sql(sprintf("SELECT DISTINCT %s FROM '%s' WHERE %s",
                                         paste(sprintf("`%s`", get_fields), collapse = ", "), my_t, terms)))
                }
              )
              if (length(search_result) == 1) {
                search_result <- search_result[[1]]
              } else {
                search_result <- purrr::reduce(search_result, dplyr::union_all)
              }
            } else {
              join_key2 <-
                .db_specs$field_name[.db_specs$table == my_tbl &
                                       grepl(sprintf("(%s)", join_key), .db_specs$foreign_key, fixed = T)]
              search_result <-
                search_result %>%
                inner_join({
                  temp_search <- lapply(
                    my_tbl, function(my_t) {
                      tbl(dbcon, sql(sprintf("SELECT %s FROM '%s'",
                                             paste(sprintf("`%s`", get_fields), collapse = ", "), my_t)))
                    })
                  if (length(temp_search) == 1) {
                    temp_search[[1]]
                  } else {
                    purrr::reduce(temp_search, dplyr::union_all)
                  }
                }, stats::setNames(join_key2, join_key))
            }

            if (any(c("test_id", "result_id") %in% get_fields)) break
            join_key <- unique(.db_specs$field_name[.db_specs$table %in% my_tbl & .db_specs$primary_key != ""])
            if (length(join_key) == 0) {
              join_key <- .db_specs$field_name[.db_specs$table == my_tbl & .db_specs$foreign_key != ""]
              next_tbl <- .db_specs$table[.db_specs$field_name %in% join_key]
            } else {
              next_tbl <- .db_specs$table[.db_specs$foreign_key %in% sprintf("%s(%s)", my_tbl, join_key)]
            }
            if (length(next_tbl) > 1 && !any(next_tbl %in% c("results", "tests")))
              stop("Sorry can't build a query for your search. Try searching in a different table.")
            if (length(next_tbl) > 0) my_tbl <- next_tbl[next_tbl %in% c("results", "tests")][[1]]
            
          }
          return(search_result %>% select(dplyr::matches("^result_id$|^test_id$")))
        })
      
      search_results <-
        if (length(search_results) == 1) search_results[[1]] else purrr::reduce(search_results, dplyr::union_all)
    })
  
  all_tests <- unlist(purrr::map(search_results, ~all(colnames(.) == "test_id")))
  search_test_results <- if (length(search_results) == 1) search_results[[1]] else
    if (any(all_tests)) {
      purrr::reduce(
        search_results[all_tests],
        dplyr::intersect
      )
    } else NULL
  if (!is.null(search_test_results))
    search_test_results <- search_test_results %>%
    dplyr::inner_join(dplyr::tbl(dbcon, "results") %>% dplyr::select(dplyr::any_of(c("result_id", "test_id"))), "test_id")
  search_results_results <- search_results[unlist(purrr::map(search_results, ~!all(colnames(.) == "test_id")))]
  if(length(search_results_results) == 0) {
    return(search_test_results)
  } else {
    search_results_results <- purrr::reduce(search_results_results, dplyr::intersect)
    if (is.null(search_test_results)) return(search_results_results) else
      return(inner_join(search_test_results, search_results_results,
                        by = c("test_id", "result_id")))
  }
}

.search_ecotox_lazy_append_fields <- function(dbcon, search_result, output_fields, compute) {
  ## TODO let's start with a clean slate.
  ## TODO steps: distinguish between simple lookup table and complex nested tables (i.e. dose-response)
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
      #TODO narrowing the selection will clash when joining lookup tables
      # dplyr::select(any_of(union(
      #   colnames(result),
      #   output_fields$field[output_fields$table %in% dose_tables]
      # ))) %>%
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
