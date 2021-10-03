#' Search and retrieve toxicity records from the database
#'
#' Create (and execute) an SQL search query based on basic search terms and options. This allows you to search
#' the database, without having to understand SQL.
#'
#' The ECOTOX database is stored locally as an SQLite file, which can be queried with SQL. These functions
#' allow you to automatically generate an SQL query and send it to the database, without having to understand
#' SQL. The function \code{search_query_ecotox} generates and returns the SQL query (which can be edited by
#' hand if desired). You can also directly call \code{search_ecotox}, this will first generate the query,
#' send it to the database and retrieve the result.
#'
#'
#' Although the generated query is not optimized for speed, it should be able to process most common searches
#' within an acceptable time. The time required for retrieving data from a search query depends on the complexity
#' of the query, the size of the query and the speed of your machine. Most queries should be completed within
#' seconds (or several minutes at most) on modern machines. If your search require optimisation for speed,
#' you could try reordering the search fields. You can also edit the query generated with \code{search_query_ecotox}
#' by hand and retrieve it with \code{\link[DBI]{dbGetQuery}}.
#'
#' Note that this package is actively maintained and this function may be revised in future versions.
#' In order to create reproducible results the user must: always work with an official release from
#' CRAN and document the package and database version that are used to generate specific results (see also
#' \code{\link{cite_ecotox}()}).
#' @param search A named \code{list} containing the search terms. The names of the elements should refer to
#' the field (i.e. table header) in which the terms are searched. Use \code{\link{list_ecotox_fields}()} to
#' obtain a list of available field names.
#'
#' Each element in that list should contain another list with at least one element named 'terms'. This should
#' contain a \code{vector} of \code{character} strings with search terms. Optionally, a second element
#' named 'method' can be provided which should be set to either '\code{contain}' (default, when missing) or
#' '\code{exact}'. In the first case the query will match any record in the indicated field that contains
#' the search term. In case of '\code{exact}' it will only return exact matches. Note that searches are
#' not case sensitive, but are picky with special (accented) characters. While building the local database
#' (see \link{build_ecotox_sqlite}) such special characters may be treated differently on different
#' operating systems. For the sake of reproducibility, the user is advised to stick with non-accented
#' alpha-numeric characters.
#'
#' Search terms for a specific field (table header) will be combined with 'or'. Meaning that any record that
#' matches any of the terms are returned. For instance when 'latin_name' 'Daphnia magna' and 'Skeletonema costatum'
#' are searched, results for both species are returned. Search terms across fields (table headers) are combined with
#' 'and', which will narrow the search. For instance if 'chemical_name' 'benzene' is searched in combination
#' with 'latin_name' 'Daphnia magna', only tests where Daphnia magna are exposed to benzene are returned.
#'
#' When this search behaviour described above is not desirable, the user can either adjust the query manually,
#' or use this function to perform several separate searches and combine the results afterwards.
#'
#' Beware that some field names are ambiguous and occur in multiple tables (like `cas_number' and `code').
#' When searching such fields, the search result may not be as expected.
#' @param output_fields A \code{vector} of \code{character} strings indicating which field names (table headers)
#' should be included in the output. By default \code{\link{list_ecotox_fields}("default")} is used. Use
#' \code{\link{list_ecotox_fields}("all")} to list all available fields.
#' @param group_by_results Ecological test results are generally the most informative element in the ECOTOX
#' database. Therefore, this search function returns a table with unique results in each row.
#'
#' However, some tables in the database (such as 'chemical_carriers' and 'dose_responses') have a one to many
#' relationship with test results. This means that multiple chemical carriers can be linked to a single test result,
#' similarly, multiple doses can also be linked to a single test result.
#'
#' By default the search results are grouped by test results. As a result not all doses or chemical carriers may
#' be displayed in the output. Set the \code{group_by_results} parameter to \code{FALSE} in order to force SQLite
#' to output all data (all carriers and doses). But beware that test results may be duplicated in those cases.
#' @param ... Arguments passed to \code{\link{dbConnectEcotox}}. You can use this when the database
#' is not located at the default path (\code{\link{get_ecotox_path}()}).
#' @return In case of \code{search_query_ecotox}, a \code{character} string containing an SQL
#' query is returned. This query is built based on the provided search terms and options.
#'
#' In case of \code{search_ecotox} a \code{data.frame} is returned based on the search query built with
#' \code{search_query_ecotox}. The \code{data.frame} is unmodified as returned by SQLite, meaning that all
#' fields are returned as \code{character}s (even where the field types are 'date' or 'numeric').
#'
#' The results are tagged with: a time stamp; the package version used; and the
#' file path of the SQLite database used in the search (when applicable). These tags are added as attributes
#' to the output table or query.
#' @rdname search_ecotox
#' @name search_ecotox
#' @examples
#' \dontrun{
#' ## let's find the ids of all ecotox tests on species
#' ## where latin names contain either of 2 specific genus names and
#' ## where they were exposed to the chemical benzene
#' if (check_ecotox_availability()) {
#'   search <-
#'     list(
#'       latin_name    = list(
#'         terms          = c("Skeletonema", "Daphnia"),
#'         method         = "contains"
#'       ),
#'       chemical_name = list(
#'         terms          = "benzene",
#'         method         = "exact"
#'       )
#'     )
#'   ## numbers in result each represent a unique test id from the database
#'   result <- search_ecotox(search)
#'   query  <- search_query_ecotox(search)
#'   cat(query)
#' } else {
#'   print("Sorry, you need to use 'download_ecotox_data()' first in order for this to work.")
#' }
#' }
#' @author Pepijn de Vries
#' @export
search_ecotox <- function(search, output_fields = list_ecotox_fields("default"), group_by_results = TRUE, ...) {
  search <- search_query_ecotox(search, output_fields, group_by_results)
  dbcon  <- dbConnectEcotox(...)
  query  <- RSQLite::dbGetQuery(dbcon, search)
  dbDisconnectEcotox(dbcon)
  return(.add_tags(query, attributes(dbcon)$database_file))
}

#' @rdname search_ecotox
#' @name search_query_ecotox
#' @export
search_query_ecotox <- function(search, output_fields = list_ecotox_fields("default"), group_by_results = TRUE) {
  ignored_fields <- !(output_fields %in% list_ecotox_fields("all"))
  if (any(ignored_fields)) warning(sprintf("The following fields are unknown and ignored: %s.",
                                           paste(output_fields[ignored_fields], collapse =", ")))
  output_fields <- output_fields[!ignored_fields]
  if (!any(grepl("^results.", output_fields))) {
    warning("Output fields should contain at least 1 field from table 'results'. Adding 'test_id'.")
    output_fields <- c("results.test_id")
  }

  ## identify key fields that are required for joining tables
  db_links <- cbind(.db_specs,
                    do.call(rbind, lapply(strsplit(.db_specs$foreign_key, "\\(|\\)"), function(x) {
                      if (length(x) < 2) return(data.frame(foreign_table = "", foreign_field = "")) else
                        return(data.frame(foreign_table = x[1], foreign_field = x[2]))
                    })))
  db_links$is_key <- db_links$primary_key == "PRIMARY KEY" | db_links$foreign_key != "" | db_links$foreign_table != ""
  key_output_fields <- .db_specs[db_links$is_key | paste(.db_specs$table, .db_specs$field_name, sep = ".") %in% output_fields,,drop = F]
  output_fields     <- .db_specs[paste(.db_specs$table, .db_specs$field_name, sep = ".") %in% output_fields,,drop = F]

  if (!is.list(search)) stop("Parameter 'search' needs to be a list!")
  if (!all(unlist(lapply(search, is.list)))) stop("Each element of parameter 'search' should contain a list")
  if (any(duplicated(names(search)))) stop("You have used duplicated search fields. Use each field only once in your search!")
  search.tables <- do.call(rbind, lapply(names(search), function(fn) {
    tables   <- unique(.db_specs$table[.db_specs$field_name %in% fn])
    if (length(tables) == 0) stop(sprintf("Unknown search field: %s", fn))
    if (fn == "test_id") tables <- "tests"
    x        <- search[[fn]]
    if (!all(names(x) %in% c("terms", "method"))) stop("Each search field can only contain two elements: 'terms' and 'method'.")
    method   <- match.arg(x[["method"]], c("exact", "contains"))
    wildcard <- ifelse(method == "contains", "%", "")
    collapse <- ifelse(method == "contains", " OR ", ", ")
    prefix   <- ifelse(method == "contains", sprintf("\"%s\" LIKE ", fn), "")
    if (typeof(x$terms) != "character" || length(x$terms) == 0) stop("Provide at least 1 search term (type 'character')")

    terms    <- paste(sprintf("%s\"%s%s%s\"",
                              prefix,
                              wildcard,
                              x$terms,
                              wildcard),
                      collapse = collapse)
    if (method == "exact") {
      terms <- sprintf("\"%s\" COLLATE NOCASE IN (%s)", fn, terms)
    }
    return (data.frame(table  = tables,
                       terms  = terms,
                       method = method))
  }))
  search.tables <- rbind(search.tables,
                         data.frame(table = unique(c("results", "tests", with(output_fields, table[!(table %in% search.tables$table)]))),
                                    terms = "", method = ""))
  search.tables$id <- sprintf("search%03i", seq_len(nrow(search.tables)))
  search.tables$select <- unlist(lapply(seq_len(nrow(search.tables)), function(i) {
    out <- key_output_fields[key_output_fields$table == search.tables$table[i],,drop = F]
    paste(paste(search.tables$id[i], sprintf("\"%s\"", out$field_name), sep = "."), collapse = ", ")
  }))
  search.tables$query <-
    with(search.tables,
         sprintf("SELECT %s FROM \"%s\" AS %s%s",
                 select,
                 table,
                 id,
                 ifelse(terms != "", sprintf(" WHERE %s", terms), "")
         )
    )
  ## species and species_synonyms need to be combined before we can continue
  if (any(search.tables$table == "species_synonyms")) {
    sp_id                       <- search.tables$table == "species"
    ss_id                       <- search.tables$table == "species_synonyms"
    sp                          <- search.tables[sp_id,]
    select                      <- gsub(sp$id, "syns", sp$select)
    q                           <- search.tables$query[ss_id]
    q                           <- sprintf("SELECT %s FROM species AS syns INNER JOIN (%s) USING(species_number)",
                                           select,
                                           q)
    search.tables$query[sp_id]  <- sprintf("SELECT * FROM (%s UNION ALL %s) AS spec", search.tables$query[sp_id], q)
    search.tables$id[sp_id]     <- "syns"
    search.tables$select[sp_id] <- select
    search.tables               <- search.tables[!ss_id,]
  }
  search.tables$linked_to   <- ""
  search.tables$linked_by   <- ""
  search.tables$linked_from <- ""
  j <- 1
  for (tab in search.tables$table[!(search.tables$table %in% c("results", "tests"))]) {
    repeat {
      i <- which(search.tables$table == tab)
      links <- subset(db_links, (db_links$table == tab & db_links$foreign_table != "") | db_links$foreign_table == tab)
      exclude <- c("chemical_carriers", "doses", "dose_responses", "dose_response_details")
      exclude <- exclude[!(exclude %in% output_fields$table)]
      links <- subset(links, !links$table %in% exclude)
      inverselink <- subset(links, links$table == tab & links$field_name %in% c("test_id", "result_id"))
      if (nrow(inverselink) > 0) {
        search.tables$linked_to[i]   <- inverselink$foreign_table
        search.tables$linked_by[i]   <- inverselink$foreign_field
        search.tables$linked_from[i] <- inverselink$field_name
        break
      } else {
        links <- links[1,]
        if (links$table %in% c("results", "tests")) {
          search.tables$linked_to[i]   <- links$table
          search.tables$linked_by[i]   <- links$field_name
          search.tables$linked_from[i] <- links$foreign_field
          break
        }
        temp_sel <- db_links$field_name[db_links$table == links$table]
        search.tables$select[i] <- gsub(search.tables$id[i], sprintf("target%03i", j), search.tables$select[i])
        search.tables$id[i]     <- sprintf("target%03i", j)
        search.tables$select[i] <- paste(c(search.tables$select[i],
                                           sprintf("%s.\"%s\"", sprintf("source%03i", j), temp_sel)), collapse = ", ")
        search.tables$query[i] <-
          sprintf("SELECT %s FROM \"%s\" AS source%03i\nLEFT JOIN (%s) target%03i ON source%03i.%s = target%03i.\"%s\"",
                  search.tables$select[i],
                  links$table,
                  j,
                  search.tables$q[i],
                  j, j,
                  links$field_name,
                  j,
                  links$foreign_field)
        if (tab == links$table) stop("Can't build an SQL query using these parameters.")
        tab <- links$table
      }
    }
    j <- j + 1
  }
  tests.query.tabs   <- subset(search.tables, search.tables$linked_to == "tests")
  tests.query.withs  <- paste0("WITH ", paste(sprintf("%s AS (\n%s\n)", tests.query.tabs$id, tests.query.tabs$query), collapse = ",\n"))
  tests.query.select <- unique(sprintf("\"%s\"", key_output_fields$field_name[key_output_fields$table == "tests"]))

  results.query.tabs  <- subset(search.tables, search.tables$table == "results")
  results.query.where <- results.query.tabs$terms[results.query.tabs$terms != ""]
  tests_from_results  <- sprintf("tests.test_id IN (SELECT DISTINCT test_id FROM results WHERE %s)", results.query.where)

  tests.query.where  <- subset(search.tables, search.tables$table == "tests" & search.tables$terms != "")
  tests.query.where  <- paste(
    sprintf("(%s)",
            c(if (length(tests_from_results) > 0) tests_from_results else NULL,
              if (length(tests.query.where$terms) > 0) tests.query.where$terms else NULL,
              with(subset(tests.query.tabs, tests.query.tabs$terms != ""),
                   sprintf("tests.\"%s\" IN (SELECT \"%s\" FROM \"%s\")",
                           linked_by, linked_from, id)))),
    collapse = " AND ")
  tests.query.tabs$extrawhere <- rep("", nrow(tests.query.tabs))
  tests.query.tabs$extrawhere <- sprintf(" WHERE \"%s\".\"%s\" IN (SELECT DISTINCT tests_agg.\"%s\" FROM tests_agg)",
                                         tests.query.tabs$table,
                                         tests.query.tabs$linked_from,
                                         tests.query.tabs$linked_by)
  tests.query        <- sprintf("%s\nSELECT %s FROM tests%s%s\n",
                                tests.query.withs,
                                paste(tests.query.select, collapse = ", "),
                                ifelse(tests.query.where == "", "", " WHERE "),
                                tests.query.where)

  tests.query        <- paste0("WITH tests_agg AS (", tests.query,
                               ")\nSELECT * FROM tests_agg\n",
                               paste(sprintf("LEFT JOIN (SELECT * FROM \"%s\"%s) AS %s ON tests_agg.\"%s\" = %s.\"%s\"",
                                             tests.query.tabs$table,
                                             tests.query.tabs$extrawhere,
                                             tests.query.tabs$id,
                                             tests.query.tabs$linked_by,
                                             tests.query.tabs$id,
                                             tests.query.tabs$linked_from), collapse = "\n"))
  results.query.where <- paste(
    c(sprintf("(%s)", results.query.where),
      "results.test_id IN (SELECT DISTINCT test_id FROM tests_agg)"),
    collapse = " AND ")
  results.query <- sprintf(paste0("WITH tests_agg AS (%s)\n",
                                  "SELECT * FROM (SELECT DISTINCT * FROM results WHERE %s)\n",
                                  "INNER JOIN (SELECT * FROM tests_agg) USING(test_id)%s"),
                           tests.query,
                           results.query.where,
                           ifelse(group_by_results, "GROUP BY result_id", "")
  )

  results.query <- sprintf("SELECT %s FROM\n(%s)",
                           paste(sprintf("\"%s\"", unique(output_fields$field_name)), collapse = ", "),
                           results.query
  )
  return(.add_tags(results.query))
}
