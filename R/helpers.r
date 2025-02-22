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
  # Declare variables to pass CRAN checks
  .data <- field <- terms <- table_mod <- NULL

  search_result <-
    lapply(search, as.data.frame, strings.as.factors = F) |>
    dplyr::bind_rows(.id = "field") |>
    dplyr::mutate(
      table = lapply(field, function(x) {
        switch(
          x,
          result_id = "results",
          test_id   = "tests",
          .db_specs$table[.db_specs$field_name %in% x])
      })
    ) |>
    tidyr::unnest("table") |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c("table", "field", "method")))) |>
    dplyr::summarise(
      where = {
        method <- tryCatch({.data$method[[1]]}, error = function(e) "contains")
        if (method == "exact") {
          sprintf("`%s` COLLATE NOCASE IN ('%s')", field[[1]], paste(.data$terms, collapse = "', '"))
        } else if(method == "contains"){
          paste(sprintf("`%s` LIKE '%%%s%%'", field, terms), collapse = " OR ")
        } else {
          stop("Sorry, specified search method is not implemented.")
        }
      }) |>
    dplyr::group_by(table) |>
    dplyr::group_map(~{
      dplyr::tibble(
        tbl   = list(
          tbl(dbcon, .x$table[[1]]) |>
            dplyr::filter(!!sql(paste(sprintf("(%s)", .x$where), collapse = " AND ")))),
        table = .x$table[[1]]
      )}, .keep = T) |>
    dplyr::bind_rows() |>
    dplyr::group_by(table_mod = gsub("_synonyms", "", table)) |>
    dplyr::summarise(
      table = table_mod[[1]],
      tbl   =
        if (length(tbl) > 1) {
          tbl |>
            purrr::map(~{select(., dplyr::any_of("species_number"))}) |>
            purrr::reduce(~{dplyr::union(.x, .y)}) |>
            list()
        } else {
          tbl
        }) |>
    dplyr::group_by(dplyr::across(dplyr::any_of("table"))) |>
    dplyr::group_map(~{
      #keep linking tables until result_ids are obtained
      my_tab <- ..1$tbl[[1]]
      count <- 0
      repeat {
        count <- count + 1
        if (count > 5) stop("Sorry could not build a query for the search you have specified.")
        cur_fields <- colnames(my_tab)
        if (any(c("result_id", "test_id") %in% cur_fields)) break
        referring <- .db_specs[.db_specs$table %in% c("results", "tests") &
                                 .db_specs$foreign_key %in% sprintf("%s(%s)", ..2$table, cur_fields),,drop = F]
        if (nrow(referring) == 1) {
          foreign <- unlist(regmatches(referring$foreign_key, gregexpr("(?<=\\().+(?=\\))", referring$foreign_key, perl = T)))
          my_tab <-
            my_tab |>
            left_join(tbl(dbcon, referring$table), by = structure(referring$field_name, names = foreign))
        } else {
          stop("Sorry could not build a query for the search you have specified.")
        }
      }
      dplyr::tibble(tbl = list(my_tab |> select(dplyr::any_of(c("result_id", "test_id")))))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(result_available = tbl |>
                      purrr::map(~ paste(sort(match(colnames(.), c("test_id", "result_id"))), collapse = "_")) |>
                      unlist()) |>
    dplyr::group_map(~{
      switch(
        .y$result_available,
        `1` = {
          purrr::reduce(.x$tbl, ~{inner_join(.x, .y, by = "test_id")}) |>
            left_join(tbl(dbcon, "results"), by = "test_id") |>
            select(dplyr::all_of(c("test_id", "result_id")))
        },
        `2` = {
          purrr::reduce(.x$tbl, ~{inner_join(.x, .y, by = "result_id")}) |>
            left_join(tbl(dbcon, "results"), by = "result_id") |>
            select(dplyr::all_of(c("test_id", "result_id")))
        },
        `1_2` = {
          purrr::reduce(.x$tbl, ~{inner_join(.x, .y, by = c("test_id", "result_id"))})
        })
    }) |>
    purrr::reduce(~{inner_join(.x, .y, by = c("test_id", "result_id"))})
  
  return(search_result)
}

.search_ecotox_lazy_append_fields <- function(dbcon, search_result, output_fields, compute, ...) {
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
  
  .process_lookups <- function(result, parent_tab, prefix = "") {
    REPLACE <- NULL # This is used in dplyr routines below but are not globally bound
    join_params <- list()
    
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
            join_params[[length(join_params) + 1]] <-
              list(
                left_by      = paste0(prefix, fld, "_temp"),
                right_table  = tab,
                right_by     = "code",
                right_select = structure("description", names = paste0(prefix, descr))
              )
          }
        }
      }
    }
    if (length(join_params) > 0) {
      ## Create temp fields where forward slash is removed for joining with lookup values
      lefties      <- join_params |> purrr::map_chr(~.$left_by)
      lefties_repl <- lefties |> purrr::map(~rlang::expr(REPLACE(!!rlang::sym(gsub("_temp$", "", .)), "/", "")))
      names(lefties_repl) <- lefties
      result <- result |> dplyr::mutate(!!!lefties_repl)
      
      for (i in seq_along(join_params)) {
        jp <- join_params[[i]]
        result <-
          result |>
          left_join(tbl(dbcon, jp$right_table) |> dplyr::rename(!!!jp$right_select),
                           by = structure(jp$right_by, names = jp$left_by))
      }
      result <- result |> select(-dplyr::any_of(lefties))
      if (compute) result <- result |> dplyr::compute()
    }
    return(result)
  }
  
  result <- search_result
  if (compute) result <- result |> dplyr::compute()
  ## start with results table as this is needed for obtaining dose link ids
  
  results_output <- union(output_fields$field[output_fields$table == "results"], "result_id")
  dose_tables    <- c("dose_responses", "dose_response_details", "doses", "dose_response_links", "dose_stat_method_codes")
  if (any(dose_tables %in% output_fields_tables)) {
    
    out_rename <- output_fields$table %in% dose_tables & !output_fields$field %in% c("test_id", "result_id")
    output_fields$new_field[out_rename] <- paste0("dose_link_", output_fields$field[out_rename])
    result <-
      result |>
      left_join(tbl(con, "dose_response_links"),   "result_id") |>
      left_join(tbl(con, "dose_responses"),      c("dose_resp_id", "test_id")) |>
      left_join(tbl(con, "dose_response_details"), "dose_resp_id") |>
      left_join(tbl(con, "doses"),               c("dose_id", "test_id")) |>
      dplyr::rename_with(~paste0("dose_link_", .), !dplyr::any_of(c("test_id", "result_id"))) |>
      .process_lookups("dose_response_links") |>
      .process_lookups("doses", "dose_link_")
  }
  
  if (any(c("media_characteristics", "organic_matter_type_codes") %in% output_fields_tables)) {
    foreigns <- .db_specs$field_name[.db_specs$table == "media_characteristics" & .db_specs$foreign_key != ""]
    result <-
      result |>
      left_join(tbl(con, "media_characteristics"), "result_id") |>
      select(dplyr::any_of(union(
        colnames(result),
        union(
          foreigns,
          output_fields$field[output_fields$table == "media_characteristics"]
        )
      ))) |>
      .process_lookups("media_characteristics")
  }
  
  result_foreigns <- data.frame(
    table  = gsub("\\(.*?\\)", "",
                  .db_specs$foreign_key[.db_specs$foreign_key != "" & .db_specs$table == "results"]),
    field  = .db_specs$field_name[.db_specs$foreign_key != "" & .db_specs$table == "results"]
  ) |> dplyr::filter(!table %in% c("tests", "results"))
  
  results_output <- union(results_output, c("result_id", result_foreigns$field))
  result <- result |>
    left_join(tbl(con, "results") |>
                       select(dplyr::any_of(union(c("result_id", "test_id"), results_output))) |>
                       dplyr::rename_with(function(x) paste0("result_", x),
                                          dplyr::any_of(c("created_date", "modified_date", "additional_comments"))),
                     c("result_id", "test_id"))
  output_fields$new_field[output_fields$table == "results" & output_fields$field == "created_date"] <-
    "result_created_date"
  output_fields$new_field[output_fields$table == "results" & output_fields$field == "modified_date"] <-
    "result_modified_date"
  output_fields$new_field[output_fields$table == "results" & output_fields$field == "additional_comments"] <-
    "result_additional_comments"
  
  if (compute) result <- result |> dplyr::compute()
  
  result <- result |> .process_lookups("results")
  
  ## continue with linking all requested tables to the test data
  
  if ("chemical_carriers" %in% output_fields_tables) {
    car_sel <- output_fields$table == "chemical_carriers" & !output_fields$field %in% c("carrier_id", "test_id")
    output_fields$new_field[car_sel] <- paste0("carrier_", output_fields$new_field[car_sel])
    result <-
      result |>
      left_join(tbl(con, "chemical_carriers") |>
                         dplyr::rename_with(~paste0("carrier_", .), !dplyr::any_of(c("carrier_id", "test_id"))),
                       "test_id") |>
      select(dplyr::any_of(union(
        colnames(result),
        output_fields$new_field[output_fields$table == "chemical_carriers"]
      )))
  }
  
  test_foreigns <- data.frame(
    table  = gsub("\\(.*?\\)", "",
                  .db_specs$foreign_key[.db_specs$foreign_key != "" & .db_specs$table == "tests"]),
    field  = .db_specs$field_name[.db_specs$foreign_key != "" & .db_specs$table == "tests"]
  ) |> dplyr::filter(!table %in% c("tests", "results"))
  
  test_foreigns <- test_foreigns[test_foreigns$table %in% output_fields$table,,drop = F]
  test_foreigns$foreign <- .db_specs$field_name[.db_specs$primary_key != ""][
    match(test_foreigns$table, .db_specs$table[.db_specs$primary_key != ""])]
  
  required_test_fields <-
    union(output_fields$field[output_fields$table == "tests"], test_foreigns$field)
  
  result <- result |>
    left_join(tbl(con, "tests") |>
                       select(dplyr::any_of(union("test_id", required_test_fields))) |>
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
    result <- result |>
      left_join(
        tbl(con, "species") |> select(union(species_fields, "species_number")),
        "species_number")
    if (length(species_syn_fields) > 0) {
      output_fields$new_field[output_fields$table == "species_synonyms" & output_fields$field == "latin_name"] <-
        "species_synonyms"
      spec_syns <-
        tbl(con, "species_synonyms") |>
        select(union(species_syn_fields, "species_number"))
      if ("species_synonyms" %in% output_fields$table) {
        spec_syns <- spec_syns |> dplyr::rename(species_synonyms = "latin_name")
      }
      result <- result |> left_join(spec_syns, "species_number")
    }
  }
  
  if (!is.null(list(...)$group_by_results) && list(...)$group_by_results && "species_synonyms" %in% colnames(result)) {
    result <- result |>
      dplyr::group_by(dplyr::across(!dplyr::any_of("species_synonyms"))) |>
      dplyr::summarise(dplyr::across(dplyr::any_of("species_synonyms"), ~GROUP_CONCAT(., "|")), .groups = "keep") |>
      dplyr::ungroup()
  }
  
  if ("chemicals" %in% output_fields$table) {
    output_fields$new_field[output_fields$table == "chemicals" & output_fields$field == "cas_number"] <-
      "test_cas"
    output_fields$new_field[output_fields$table == "chemicals" & output_fields$field == "chemical_name"] <-
      "test_chemical"
    output_fields$new_field[output_fields$table == "chemicals" & output_fields$field == "ecotox_group"] <-
      "test_chemical_group"
    result <- result |>
      left_join(
        tbl(con, "chemicals") |>
          select(union("cas_number", output_fields$field[output_fields$table == "chemicals"])) |>
          dplyr::rename_with(
            function(x){
              c("test_cas", "test_chemical", "test_chemical_group")[match(x, c("cas_number", "chemical_name", "ecotox_group"))]
            }, dplyr::any_of(c("cas_number", "chemical_name", "ecotox_group"))),
        "test_cas")
  }
  
  if ("references" %in% output_fields$table) {
    result <- result |>
      left_join(
        tbl(con, "references") |>
          select(union("reference_number", output_fields$field[output_fields$table == "references"])),
        "reference_number"
      )
  }
  
  ## join remaining lookup values
  result <- result |> .process_lookups("tests")
  
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
  
  result <- result |> select(dplyr::any_of(union(unique(output_fields$new_field), "result_id")))
  return(result)
}

.group_nest_results <- function(search_result) {
  if (nrow(search_result) > 0) {
    group_by <-
      search_result |>
      dplyr::group_by(dplyr::across(dplyr::any_of("result_id"))) |>
      dplyr::summarise_all(~all(is.na(.)) || all(.[[1]] == .)) |>
      dplyr::ungroup() |>
      select(tidyselect::vars_select_helpers$where(~ all(.))) |>
      colnames()
    if (all(colnames(search_result) %in% group_by)) return (search_result) else{
      search_result <-
        search_result |>
        dplyr::group_nest(dplyr::across(dplyr::any_of(group_by)), .key = "nested_data")
    }
  } else search_result
}
