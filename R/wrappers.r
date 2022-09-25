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
#' @param group_by_results TODO behaviour has changed results will be nested by `results_id'
#' Ecological test results are generally the most informative element in the ECOTOX
#' database. Therefore, this search function returns a table with unique results in each row.
#'
#' However, some tables in the database (such as 'chemical_carriers' and 'dose_responses') have a one to many
#' relationship with test results. This means that multiple chemical carriers can be linked to a single test result,
#' similarly, multiple doses can also be linked to a single test result.
#'
#' By default the search results are grouped by test results. As a result not all doses or chemical carriers may
#' be displayed in the output. Set the \code{group_by_results} parameter to \code{FALSE} in order to force SQLite
#' to output all data (all carriers and doses). But beware that test results may be duplicated in those cases.
#' @param compute The ECOTOXr package tries to construct database queries as lazy as possible. Meaning that R
#' moves as much of the heavy lifting as possible to the database. When your search becomes complicated (e.g., when
#' including many output fields), you may run into trouble and hit the SQL parser limits. In those cases you can set
#' this parameter to \code{TRUE}. Database queries are then computed in the process of joining tables. This is generally
#' slower. Alternatively, you could try to include less output fields in order to simplify the query.
#' @param as_data_frame \code{logical} value indicating whether the result should be converted into a \code{data.frame}
#' (default is \code{TRUE}). When set to \code{FALSE} the data will be returned as a \code{\link[dplyr:tibble]{tbl_df}}.
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
#' ## TODO check all examples after heavy modifications of code
#' ## let's find the ids of all ecotox tests on species
#' ## where Latin names contain either of 2 specific genus names and
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
search_ecotox <- function(search, output_fields = list_ecotox_fields("default"),
                          group_by_results = TRUE, compute = FALSE, as_data_frame = TRUE, ...) {
  temp_field <- if (!"results.result_id" %in% output_fields) "results.result_id" else NULL
  if (any(startsWith(output_fields, "dose_responses.")) && !"dose_responses.dose_resp_id" %in% output_fields)
    temp_field <- c(temp_field, "dose_responses.dose_resp_id")
  search_result <- search_ecotox_lazy(search, c(output_fields, temp_field), compute, ...)
  database_file <- attributes(search_result)$database_file
  dbcon         <- search_result[["src"]]$con
  search_result <- search_result %>% collect()
  dbDisconnect(dbcon)
  ## group by result_id if requested
  
  if (group_by_results) search_result <- .group_nest_results(search_result)

  ## remove temporary fields
  if (!is.null(temp_field))
    search_result <-
    search_result %>%
    select(!dplyr::any_of(gsub("^.*?[.]", "", temp_field)))
  if (as_data_frame) search_result <- search_result %>% as.data.frame()
  return(.add_tags(search_result, database_file))
}

#' @rdname search_ecotox
#' @name search_ecotox_lazy
#' @export
search_ecotox_lazy <- function(search, output_fields = list_ecotox_fields("default"),
                               compute = FALSE, ...) {
  ignored_fields <- !(output_fields %in% list_ecotox_fields("all"))
  if (any(ignored_fields)) warning(sprintf("The following fields are unknown and ignored: %s.",
                                           paste(output_fields[ignored_fields], collapse =", ")))
  output_fields <- output_fields[!ignored_fields]
  ## Note that the database connection is opened here, but not closed. It's the end-users responsibility
  ## to close the connection when no longer required.
  dbcon         <- dbConnectEcotox(...)
  search_result <- .search_ecotox_lazy_get_result_ids(search, dbcon)
  search_result <- .search_ecotox_lazy_append_fields(dbcon, search_result, output_fields, compute)
  return(.add_tags(search_result, attributes(dbcon)$database_file))
}

#' @rdname search_ecotox
#' @name search_query_ecotox
#' @export
search_query_ecotox <- function(search, output_fields = list_ecotox_fields("default"), ...) {
  search_result <- search_ecotox_lazy(search, output_fields, ...)
  dbcon         <- search_result[["src"]]$con
  database_file <- attributes(search_result)$database_file
  search_result <- utils::capture.output(
    search_result %>%
      dplyr::show_query()
  ) %>%
    utils::tail(-1) %>%
    paste(collapse = "\n")
  dbDisconnect(dbcon)
  return(.add_tags(search_result, database_file))
}