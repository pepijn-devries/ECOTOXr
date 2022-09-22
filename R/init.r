#' Get ECOTOX download URL from EPA website
#'
#' This function downloads the webpage at \url{https://cfpub.epa.gov/ecotox/index.cfm}. It then searches for the
#' download link for the complete ECOTOX database and extract its URL.
#'
#' This function is called by \code{\link{download_ecotox_data}} which tries to download the file from the resulting
#' URL. On some machines this fails due to issues with the SSL certificate. The user can try to download the file
#' by using this URL in a different browser (or on a different machine). Alternatively, the user could try to use
#' \code{\link{download_ecotox_data(ssl_verifypeer = 0L)}} when the download URL is trusted.
#' @param ... arguments passed on to \code{\link[httr]{GET}}
#' @return Returns a \code{character} string containing the download URL of the latest version of the EPA ECOTOX
#' database.
#' @rdname get_ecotox_url
#' @name get_ecotox_url
#' @examples
#' \dontrun{
#' get_ecotox_url()
#' }
#' @author Pepijn de Vries
#' @export
get_ecotox_url <- function(...) {
  link <- httr::GET("https://cfpub.epa.gov/ecotox/index.cfm", ...)
  link <- rvest::read_html(link)
  link <- rvest::html_elements(link, "a.ascii-link")
  link <- rvest::html_attr(link, "href")
  link <- link[!is.na(link) & endsWith(link, ".zip")]
  if (length(link) == 0) stop("Could not find ASCII download link...")
  return(link)
}

#' Check whether a ECOTOX database exists locally
#'
#' Tests whether a local copy of the US EPA ECOTOX database exists in \code{\link{get_ecotox_path}}.
#'
#' When arguments are omitted, this function will look in the default directory (\code{\link{get_ecotox_path}}).
#' However, it is possible to build a database file elsewhere if necessary.
#' @param target A \code{character} string specifying the path where to look for the database file.
#' @return Returns a \code{logical} value indicating whether a copy of the database exists. It also returns
#' a \code{files} attribute that lists which copies of the database are found.
#' @rdname check_ecotox_availability
#' @name check_ecotox_availability
#' @examples
#' check_ecotox_availability()
#' @author Pepijn de Vries
#' @export
check_ecotox_availability <- function(target = get_ecotox_path()) {
  files    <- list.files(target)
  file_reg <- gregexpr("(?<=^ecotox_ascii_)(.*?)(?=\\.sqlite$)", files, perl = T)
  file_reg <- regmatches(files, file_reg)

  files    <- files[unlist(lapply(file_reg, length)) > 0]
  file_reg <- unlist(file_reg[unlist(lapply(file_reg, length)) > 0])
  if (any(nchar(file_reg) > 0)) {
    file_reg <- as.Date(file_reg, format = "%m_%d_%Y")
    files    <- files[!is.na(file_reg)]
    file_reg <- file_reg[!is.na(file_reg)]
  } else {
    file_reg <- as.Date(NA)[-1]
    target   <- character(0)
  }
  result <- length(files) > 0
  attributes(result)$files <- data.frame(path = target, database = files, date = file_reg,
                                         stringsAsFactors = F)
  return(result)
}

#' The local path to the ECOTOX database (directory or sqlite file)
#'
#' Obtain the local path to where the ECOTOX database is (or will be) placed.
#'
#' It can be useful to know where the database is located on your disk. This function
#' returns the location as provided by \code{\link[rappdirs]{app_dir}}.
#'
#' @param path When you have a copy of the database somewhere other than the default
#' directory (\code{\link{get_ecotox_path}()}), you can provide the path here.
#' @param version A \code{character} string referring to the release version of the database you wish to locate.
#' It should have the same format as the date in the EPA download link, which is month, day, year, separated by
#' underscores ("\%m_\%d_\%Y"). When missing, the most recent available copy is selected automatically.
#' @return Returns a \code{character} string of the path.
#' \code{get_ecotox_path} will return the default directory of the database.
#' \code{get_ecotox_sqlite_file} will return the path to the sqlite file when it exists.
#' @rdname get_path
#' @name get_ecotox_path
#' @examples
#' get_ecotox_path()
#'
#' \dontrun{
#' ## This will only work if a local database exists:
#' get_ecotox_sqlite_file()
#' }
#' @author Pepijn de Vries
#' @export
get_ecotox_path <- function() {
  rappdirs::app_dir("ECOTOXr")$cache()
}

#' Download and extract ECOTOX database files and compose database
#'
#' In order for this package to fully function, a local copy of the ECOTOX database needs to be build.
#' This function will download the required data and build the database.
#'
#' This function will attempt to find the latest download url for the ECOTOX database from the 
#' \href{https://cfpub.epa.gov/ecotox/index.cfm}{EPA website} (see \code{\link{get_ecotox_url}()}).
#' When found it will attempt to download the zipped archive containing all required data. This data is than
#' extracted and a local copy of the database is build.
#'
#' Use '\code{\link{suppressMessages}}' to suppress the progress report.
#' @section Known issues:
#' On some machines this function fails to connect to the database download URL from the
#' \href{https://cfpub.epa.gov/ecotox/index.cfm}{EPA website} due to missing
#' SSL certificates. Unfortunately, there is no easy fix for this in this package. A work around is to download and
#' unzip the file manually using a different machine or browser that is less strict with SSL certificates. You can
#' then call \code{\link{build_ecotox_sqlite}()} and point the \code{source} location to the manually extracted zip
#' archive. For this purpose \code{\link{get_ecotox_url}()} can be used.
#'
#' @param target Target directory where the files will be downloaded and the database compiled. Default is
#' \code{\link{get_ecotox_path}()}.
#' @param write_log A \code{logical} value indicating whether a log file should be written to the target path
#  after building the SQLite database. See \code{\link{build_ecotox_sqlite}()} for more details. Default is
#' \code{TRUE}.
#' @param ask There are several steps in which files are (potentially) overwritten or deleted. In those cases
#' the user is asked on the command line what to do in those cases. Set this parameter to \code{FALSE} in order
#' to continue without warning and asking.
#' @param ... Arguments passed on to \code{\link[httr]{GET}}. When this function fails with the error: "Peer
#' certificate cannot be authenticated with given CA certificates", you could try to rerun the function with
#' the option \code{ssl_verifypeer = 0L}. Only do so when you trust the indicated URL.
#' @return Returns \code{NULL} invisibly.
#' @rdname download_ecotox_data
#' @name download_ecotox_data
#' @examples
#' \dontrun{
#' ## This will download and build the database in your temp dir:
#' download_ecotox_data(tempdir())
#' }
#' @author Pepijn de Vries
#' @export
download_ecotox_data <- function(target = get_ecotox_path(), write_log = TRUE, ask = TRUE, ...) {
  avail <- check_ecotox_availability()
  if (avail && ask) {
    cat(sprintf("A local database already exists (%s).", paste(attributes(avail)$file$database, collapse = ", ")))
    prompt <- readline(prompt = "Do you wish to continue and potentially overwrite the existing database? (y/n) ")
    if (!startsWith("Y", toupper(prompt))) {
      message("Download aborted...\n")
      return(invisible(NULL))
    }
  }
  if (!dir.exists(target)) dir.create(target, recursive = T)
  ## Obtain download link from EPA website:
  message(crayon::white("Obtaining download link from EPA website... "))
  link <- get_ecotox_url()
  dest_path <- file.path(target, utils::tail(unlist(strsplit(link, "/")), 1))
  message(crayon::green("Done\n"))
  proceed.download <- T
  if (file.exists(dest_path) && ask) {
    prompt <- readline(prompt = sprintf("ECOTOX data is already present (%s), overwrite (y/n)? ", dest_path))
    proceed.download <- startsWith("Y", toupper(prompt))
  }
  if (proceed.download) {
    message(crayon::white(sprintf("Start downloading ECOTOX data from %s...\n", link)))
    httr::GET(link, httr::config(
      noprogress = 0L,
      progressfunction = function(down, up) {
        message(crayon::white(sprintf("\r%0.1f MB downloaded...", down[2]/(1024*1024))), appendLF = F)
        TRUE
      }), httr::write_disk(dest_path, overwrite = TRUE))
    message(crayon::green(" Done\n"))
  }

  ## create bib-file for later reference
  con <- file(gsub(".zip", "_cit.txt", dest_path), "w+")
  release <- as.Date(stringr::str_sub(link, -15, -1), format = "_%m_%d_%Y.zip")
  writeLines(format(utils::bibentry(
    "misc",
    title        = format(release, "US EPA ECOTOXicology Database System Version 5.0 release %Y-%m-%d"),
    author       = utils::person(family = "US EPA", role = "aut"),
    year         = format(release, "%Y"),
    url          = link,
    howpublished = link,
    note         = format(Sys.Date(), "Accessed: %Y-%m-%d")), "R"), con)
  close(con)
  extr.path <- gsub(".zip", "", dest_path)
  proceed.unzip <- T
  if (dir.exists(extr.path)) {
    test.files <- list.files(extr.path)
    if (length(test.files) >= 12 && any(test.files == "chemical_carriers.txt") && ask) {
      cat("Extracted zip files already appear to exist.\n")
      prompt <- readline(prompt = "Continue unzipping and overwriting these files (y/n)? ")
      proceed.unzip <- startsWith("Y", toupper(prompt))
    }
  }
  if (proceed.unzip) {
    message(crayon::white("Extracting downloaded zip file... "))
    utils::unzip(file.path(target, utils::tail(unlist(strsplit(link, "/")), 1)), exdir = target)
    message(crayon::green("Done\n"))
    if (ask &&
        startsWith("Y", toupper(readline(prompt = "Done extracting zip file, remove it to save disk space (y/n)? ")))) {
      message(crayon::white("Trying to delete zip file... "))
      tryCatch({
        file.remove(file.path(target, utils::tail(unlist(strsplit(link, "/")), 1)))
        message(crayon::green("Done\n"))
      }, error = function(e) {
        message(crayon::red("Failed to delete the file, continuing with next step"))
      })
    }
  }
  message(crayon::white("Start constructing SQLite database from downloaded tables...\n"))
  message(crayon::white("Note that this may take some time...\n"))
  build_ecotox_sqlite(extr.path, target, write_log)
  return(invisible(NULL))
}

#' Build an SQLite database from zip archived tables downloaded from EPA website
#'
#' This function is called automatically after \code{\link{download_ecotox_data}}. The database files can
#' also be downloaded manually from the \href{https://cfpub.epa.gov/ecotox/}{EPA website} from which a local
#' database can be build using this function.
#'
#' Raw data downloaded from the EPA website is in itself not very efficient to work with in R. The files are large
#' and would put a large strain on R when loading completely into the system's memory. Instead use this function
#' to build an SQLite database from the tables. That way, the data can be queried without having to load it all into
#' memory.
#'
#' EPA provides the raw table from the \href{https://cfpub.epa.gov/ecotox/}{ECOTOX database} as text files with
#' pipe-characters ('|') as table column separators. Although not documented, the tables appear not to contain comment
#' or quotation characters. There are records containing the reserved pipe-character that will confuse the table parser.
#' For these records, the pipe-character is replaced with a dash character ('-').
#'
#' In addition, while reading the tables as text files, this package attempts to decode the text as UTF8. Unfortunately,
#' this process appears to be platform-dependent, and may therefore result in different end-results on different platforms.
#' This problem only seems to occur for characters that are listed as 'control characters' under UTF8. This will have
#' consequences for reproducibility, but only if you build search queries that look for such special characters. It is
#' therefore advised to stick to common (non-accented) alpha-numerical characters in your searches, for the sake of
#' reproducibility.
#' 
#' Use '\code{\link{suppressMessages}}' to suppress the progress report.
#'
#' @param source A \code{character} string pointing to the directory path where the text files with the raw
#' tables are located. These can be obtained by extracting the zip archive from \url{https://cfpub.epa.gov/ecotox/}
#' and look for 'Download ASCII Data'.
#' @param destination A \code{character} string representing the destination path for the SQLite file. By default
#' this is \code{\link{get_ecotox_path}()}.
#' @param write_log A \code{logical} value indicating whether a log file should be written in the destination path
#  after building the SQLite database. See \code{\link{build_ecotox_sqlite}()} for more details. Default is
#' \code{TRUE}. The log contains information on the source and destination path, the version of this package,
#' the creation date, and the operating system on which the database was created.
#' @return Returns \code{NULL} invisibly.
#' @rdname build_ecotox_sqlite
#' @name build_ecotox_sqlite
#' @examples
#' \dontrun{
#' ## This example will only work properly if 'dir' points to an existing directory
#' ## with the raw tables from the ECOTOX database. This function will be called
#' ## automatically after a call to 'download_ecotox_data()'.
#' test <- check_ecotox_availability()
#' if (test) {
#'   files   <- attributes(test)$files[1,]
#'   dir     <- gsub(".sqlite", "", files$database, fixed = T)
#'   path    <- files$path
#'   if (dir.exists(file.path(path, dir))) {
#'     ## This will build the database in your temp directory:
#'     build_ecotox_sqlite(source = file.path(path, dir), destination = tempdir())
#'   }
#' }
#' }
#' @author Pepijn de Vries
#' @export
build_ecotox_sqlite <- function(source, destination = get_ecotox_path(), write_log = TRUE) {
  dbname <- paste0(basename(source), ".sqlite")
  dbcon  <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(destination, dbname))

  ## Loop the text file tables and add them to the sqlite database 1 by 1
  i <- 0
  by(.db_specs, .db_specs$table, function(tab) {
    i <<- i + 1
    message(crayon::white(sprintf("Adding '%s' table (%i/%i) to database:\n",
                                  tab$table[[1]], i, length(unique(.db_specs$table)))))
    filename <- file.path(source, paste0(tab$table[[1]], ".txt"))
    if (!file.exists(filename)) filename <- file.path(source, "validation", paste0(tab$table[[1]], ".txt"))

    ## Remove table from database if it already exists
    RSQLite::dbExecute(dbcon, sprintf("DROP TABLE IF EXISTS [%s];", tab$table[[1]]))

    ## specify query to create the table in the sqlite database
    foreign_keys <- tab[tab$foreign_key != "",, drop = F]
    if (nrow(foreign_keys) > 0) {
      foreign_keys <- apply(foreign_keys, 1, function(x) {
        sprintf("\tFOREIGN KEY(%s) REFERENCES [%s]", x[["field_name"]], x[["foreign_key"]])
      })
      foreign_keys <- paste(foreign_keys, collapse = ",\n")
    } else foreign_keys <- ""
    query <- tab[,names(tab) %in% c("field_name", "data_type", "primary_key", "not_null")]
    query[is.na(query)] <- ""
    query <- apply(query, 1, paste, collapse = " ")
    query <- paste(paste0("\t", trimws(query)), collapse = ",\n")
    if (foreign_keys != "") query <- paste0(query, ",\n", foreign_keys)
    query <- sprintf("CREATE TABLE [%s](\n%s\n);", tab$table[[1]], query)
    RSQLite::dbExecute(dbcon, query)

    head  <- NULL
    lines.read <- 1
    ## Copy tables in 50000 line fragments to database, to avoid memory issues
    frag.size  <- 50000
    message(crayon::white(sprintf("\r  0 lines (incl. header) of '%s' added to database", tab$table[[1]])),
            appendLF = F)
    repeat {
      if (is.null(head)) {
        head <- iconv(readr::read_lines(filename, skip = 0, n_max = 1, progress = F), to = "UTF8", sub = "*")
      } else {
        testsize   <- ifelse(lines.read == 1, frag.size - 1, frag.size)
        body       <- readr::read_lines(filename, skip = lines.read, n_max = testsize, progress = F)
        body       <- suppressWarnings({iconv(body, to = "UTF8", sub = "*")})
        ## Replace pipe-characters with dashes when they are between brackets "("and ")",
        ## These should not be interpreted as table separators and will mess up the table.read call
        body       <- stringr::str_replace_all(body, "(?<=\\().+?(?=\\))", function(x){
          if (grepl("[\\(/]", x)) return(x) ## there should not be another opening bracket or forward slash! in that case leave as is
          gsub("[|]", "-", x)
        })

        lines.read <- lines.read + length(body)

        table.frag <- utils::read.table(text = c(head, body[1:1]),
                                        sep = "|", header = T, quote = "", comment.char = "",
                                        stringsAsFactors = F, strip.white = F)

        ## strip.white is set to F, as they occur in primary keys!
        table.frag <- utils::read.table(text = c(head, body),
                                        sep = "|", header = T, quote = "", comment.char = "",
                                        stringsAsFactors = F, strip.white = F)

        missing_cols    <- tab$field_name[!tab$field_name %in% colnames(table.frag)]
        unexpected_cols <- colnames(table.frag)[!colnames(table.frag) %in% tab$field_name]
        if (length(unexpected_cols) > 0)
          message(sprintf("\r Ignoring unexpected column(s) '%s' in '%s'", paste(unexpected_cols, collapse = "', '"),
                          tab$table[[1]]))
        if (length(missing_cols) > 0)
          message(sprintf("\r Missing column(s) '%s' in '%s'", paste(missing_cols, collapse = "', '"), tab$table[[1]]))
        RSQLite::dbWriteTable(dbcon, tab$table[[1]],
                              table.frag[,setdiff(tab$field_name, missing_cols), drop = F], append = T)
        
        message(crayon::white(sprintf("\r %i lines (incl. header) of '%s' added to database", lines.read, tab$table[[1]])),
                appendLF = F)
        if (length(body) < testsize) break
      }
    }
    message(crayon::green(" Done\n"))
  })
  RSQLite::dbDisconnect(dbcon)
  if (write_log) {
    logfile      <- file.path(destination, paste0(basename(source), ".log"))
    downloadinfo <- file.path(destination, paste0(basename(source), "_cit.txt"))
    writeLines(text = sprintf(
      "ECOTOXr SQLite log\n\nSource:        %s\nDestination:   %s\nDownload info: %s\nBuild with:    %s\nBuild on:      %s\nBuild date:    %s",
      source,
      destination,
      ifelse(file.exists(downloadinfo), downloadinfo, "Not available"),
      paste0("ECOTOXr V", utils::packageVersion("ECOTOXr")),
      paste(Sys.info()[c("sysname", "release")], collapse = " "),
      format(Sys.Date(), "%Y-%m-%d")
    ),
    con = logfile)
  }
  return(invisible(NULL))
}
