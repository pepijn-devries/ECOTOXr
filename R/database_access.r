#' @rdname get_path
#' @name get_ecotox_sqlite_file
#' @export
get_ecotox_sqlite_file <- function(path = get_ecotox_path(), version) {
  if (missing(version)) {
    version <- NULL
  } else {
    if (length(version) != 1) stop("Argument 'version' should hold a single element!")
    version <- as.Date(version, format = "%m_%d_%Y")
  }
  files <- attributes(.fail_on_missing(path))$files
  results <- nrow(files)
  files <- files[which(files$date == ifelse(is.null(version), max(files$date)[[1]], version)),]
  if (results > 1 && is.null(version)) {
      warning(sprintf("Multiple versions of the database found and not one specified. Using the most recent version (%s)",
                      format(files$date, "%Y-%m-%d")))
  }
  return(file.path(files$path, files$database))
}

#' Open or close a connection to the local ECOTOX database
#'
#' Wrappers for \code{\link[RSQLite:SQLite]{dbConnect}} and \code{\link[RSQLite:SQLite]{dbDisconnect}} methods.
#'
#' Open or close a connection to the local ECOTOX database. These functions are only required when you want
#' to send custom queries to the database. For most searches the \code{\link{search_ecotox}} function
#' will be adequate.
#'
#' @param path A \code{character} string with the path to the location of the local database (default is
#' \code{\link{get_ecotox_path}()}).
#' @param version A \code{character} string referring to the release version of the database you wish to locate.
#' It should have the same format as the date in the EPA download link, which is month, day, year, separated by
#' underscores ("\%m_\%d_\%Y"). When missing, the most recent available copy is selected automatically.
#' @param conn An open connection to the ECOTOX database that needs to be closed.
#' @param ... Arguments that are passed to \code{\link[RSQLite:SQLite]{dbConnect}} method
#' or \code{\link[RSQLite:SQLite]{dbDisconnect}} method.
#' @return A database connection in the form of a \code{\link[DBI]{DBIConnection-class}} object.
#' The object is tagged with: a time stamp; the package version used; and the
#' file path of the SQLite database used in the connection. These tags are added as attributes
#' to the object.
#' @rdname dbConnectEcotox
#' @name dbConnectEcotox
#' @examples
#' \dontrun{
#' ## This will only work when a copy of the database exists:
#' con <- dbConnectEcotox()
#'
#' ## check if the connection works by listing the tables in the database:
#' dbListTables(con)
#'
#' ## Let's be a good boy/girl and close the connection to the database when we're done:
#' dbDisconnectEcotox(con)
#' }
#' @author Pepijn de Vries
#' @export
dbConnectEcotox <- function(path = get_ecotox_path(), version, ...) {
  f <- get_ecotox_sqlite_file(path, version)
  return(.add_tags(RSQLite::dbConnect(RSQLite::SQLite(), f, ...), f))
}

#' @rdname dbConnectEcotox
#' @name dbDisconnectEcotox
#' @export
dbDisconnectEcotox <- function(conn, ...) {
  RSQLite::dbDisconnect(conn, ...)
}

#' Cite the downloaded copy of the ECOTOX database
#'
#' Cite the downloaded copy of the ECOTOX database and this package for reproducible results.
#'
#' When you download a copy of the EPA ECOTOX database using \code{\link{download_ecotox_data}()}, a BibTex file
#' is stored that registers the database release version and the access (= download) date. Use this function
#' to obtain a citation to that specific download.
#'
#' In order for others to reproduce your results, it is key to cite the data source as accurately as possible.
#' @param path A \code{character} string with the path to the location of the local database (default is
#' \code{\link{get_ecotox_path}()}).
#' @param version A \code{character} string referring to the release version of the database you wish to locate.
#' It should have the same format as the date in the EPA download link, which is month, day, year, separated by
#' underscores ("\%m_\%d_\%Y"). When missing, the most recent available copy is selected automatically.
#' @return Returns a \code{vector} of \code{\link{bibentry}}'s, containing a reference to the downloaded database
#' and this package.
#' @rdname cite_ecotox
#' @name cite_ecotox
#' @examples
#' \dontrun{
#' ## In order to cite downloaded database and this package:
#' cite_ecotox()
#' }
#' @author Pepijn de Vries
#' @export
cite_ecotox <- function(path = get_ecotox_path(), version) {
  db  <- get_ecotox_sqlite_file(path, version)
  bib <- gsub(".sqlite", "_cit.txt", db, fixed = T)
  if (!file.exists(bib)) stop("No bibentry reference to database download found!")
  result <- utils::readCitationFile(bib)
  return(c(result, utils::citation("ECOTOXr")))
}

#' Get information on the local ECOTOX database when available
#'
#' Get information on how and when the local ECOTOX database was build.
#'
#' Get information on how and when the local ECOTOX database was build. This information is retrieved
#' from the log-file that is (optionally) stored with the local database when calling \code{\link{download_ecotox_data}}
#' or \code{\link{build_ecotox_sqlite}}.
#' @param path A \code{character} string with the path to the location of the local database (default is
#' \code{\link{get_ecotox_path}()}).
#' @param version A \code{character} string referring to the release version of the database you wish to locate.
#' It should have the same format as the date in the EPA download link, which is month, day, year, separated by
#' underscores ("\%m_\%d_\%Y"). When missing, the most recent available copy is selected automatically.
#' @return Returns a \code{vector} of \code{character}s, containing a information on the selected local ECOTOX database.
#' @rdname get_ecotox_info
#' @name get_ecotox_info
#' @examples
#' \dontrun{
#' ## Show info on the current database (only works when one is downloaded and build):
#' get_ecotox_info()
#' }
#' @author Pepijn de Vries
#' @export
get_ecotox_info <- function(path = get_ecotox_path(), version) {
  default <- "No information available\n"
  inf <- tryCatch({
    db  <- get_ecotox_sqlite_file(path, version)
    gsub(".sqlite", ".log", db, fixed = T)
  }, error = function(e) return(default))
  if (file.exists(inf)) {
    inf <- readLines(inf)
  } else {
    inf <- default
  }
  message(crayon::white(paste(inf, collapse = "\n")))
  return(invisible(inf))
}

#' List the field names that are available from the ECOTOX database
#'
#' List the field names (table headers) that are available from the ECOTOX database
#'
#' This can be useful when specifying a \code{\link{search_ecotox}}, to identify which fields
#' are available from the database, for searching and output.
#' @param which A \code{character} string that specifies which fields to return. Can be any of:
#' '\code{default}': returns default output field names; '\code{all}': returns all fields; or
#' '\code{full}': returns all except fields from table 'dose_response_details'.
#' @param include_table A \code{logical} value indicating whether the table name should be included
#' as prefix. Default is \code{TRUE}.
#' @return Returns a \code{vector} of type \code{character} containing the field names from the ECOTOX database.
#' @rdname list_ecotox_fields
#' @name list_ecotox_fields
#' @examples
#' ## Fields that are included in search results by default:
#' list_ecotox_fields("default")
#'
#' ## All fields that are available from the ECOTOX database:
#' list_ecotox_fields("all")
#'
#' ## All except fields from the table 'dose_response_details'
#' ## that are available from the ECOTOX database:
#' list_ecotox_fields("full")
#' @author Pepijn de Vries
#' @export
list_ecotox_fields <- function(which = c("default", "full", "all"), include_table = TRUE) {
  which <- match.arg(which)
  result <- .db_specs$field_name
  if (include_table)      result <- paste(.db_specs$table, result, sep = ".")
  if (which == "default") result <- result[.db_specs$default_output]
  if (which == "full")    result <- result[.db_specs$table != "dose_response_details"]
  return(result)
}
