#' Get ECOTOX download URL from EPA website
#'
#' `r lifecycle::badge('stable')` This function downloads the webpage at <https://cfpub.epa.gov/ecotox/index.cfm>. It then searches for the
#' download link for the complete ECOTOX database and extract its URL.
#'
#' This function is called by [download_ecotox_data()] which tries to download the file from the resulting
#' URL. On some machines this fails due to issues with the SSL certificate. The user can try to download the file
#' by using this URL in a different browser (or on a different machine). Alternatively, the user could try to use
#' `[download_ecotox_data](verify_ssl = FALE)` when the download URL is trusted.
#' @param verify_ssl When set to `FALSE` the SSL certificate of the host (EPA)
#' is not verified. Can also be set as option:
#' `options(ECOTOXr_verify_ssl = TRUE)`. Default is `TRUE`.
#' @param ... arguments passed on to [httr2::req_options()]
#' @returns Returns a `character` string containing the download URL of the latest version of the EPA ECOTOX
#' database.
#' @rdname get_ecotox_url
#' @name get_ecotox_url
#' @examples
#' if (interactive()) {
#'   get_ecotox_url()
#' }
#' @author Pepijn de Vries
#' @family database-build-functions
#' @family online-functions
#' @export
get_ecotox_url <- function(verify_ssl = getOption("ECOTOXr_verify_ssl"), ...) {
  if (is.null(verify_ssl)) verify_ssl <- TRUE
  args <- list(...)
  if (!verify_ssl) {
    Sys.setenv(OPENSSL_CONF = system.file("openssl.cnf", package = "ECOTOXr"))
    args[["ssl_verifyhost"]] <- 0
    args[["ssl_verifypeer"]] <- 0
  }
  link <- tryCatch({
    .get_ecotox_url("https://cfpub.epa.gov/ecotox/index.cfm", !!!args)
  }, error = function(e) {
    .get_ecotox_url("https://gaftp.epa.gov/ecotox/", !!!args)
  })
  return(link)
}

.get_ecotox_url <- function(url, ...) {
  link <-
    httr2::request(url) |>
    httr2::req_options(...) |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  link <- link[!is.na(link) & endsWith(link, ".zip")]
  if (length(link) == 0)
    rlang::abort(c(
      x = "Could not find ASCII download link...",
      i = "Please file a reproducible issue report at https://www.github.com/pepijn-devries/ECOTOXr/issues"))
  link[!startsWith(link, "http")] <- paste0(url, link[!startsWith(link, "http")])
  link_dates <-
    stringr::str_sub(link, -14, -5) |>
    as.Date(format = "%m_%d_%Y")
  link[which(link_dates == max(link_dates))]
}

#' Check whether a ECOTOX database exists locally
#'
#' `r lifecycle::badge('stable')` Tests whether a local copy of the US EPA ECOTOX database exists in
#'  [get_ecotox_path()].
#'
#' When arguments are omitted, this function will look in the default directory ([get_ecotox_path()]).
#' However, it is possible to build a database file elsewhere if necessary.
#' @param target A `character` string specifying the path where to look for the database file.
#' @returns Returns a `logical` value indicating whether a copy of the database exists. It also returns
#' a `files` attribute that lists which copies of the database are found.
#' @rdname check_ecotox_availability
#' @name check_ecotox_availability
#' @examples
#' check_ecotox_availability()
#' @author Pepijn de Vries
#' @family database-access-functions
#' @export
check_ecotox_availability <- function(target = get_ecotox_path()) {
  files    <- list.files(target)
  file_reg <- stringr::str_extract(files, "(?<=^ecotox_ascii_)(.*?)(?=\\.sqlite$)")
  
  sel      <- lengths(file_reg) > 0 & !is.na(file_reg)
  files    <- files[sel]
  file_reg <- unlist(file_reg[sel])
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
                                         stringsAsFactors = FALSE)
  return(result)
}

#' The local path to the ECOTOX database (directory or sqlite file)
#'
#' `r lifecycle::badge('stable')` Obtain the local path to where the ECOTOX database is
#' (or will be) placed.
#'
#' It can be useful to know where the database is located on your disk. This function
#' returns the location as provided by [tools::R_user_dir()], or as
#' specified by you using `options(ECOTOXr_path = "mypath")`.
#'
#' @param path When you have a copy of the database somewhere other than the default
#' directory ([get_ecotox_path()]), you can provide the path here.
#' @param version A `character` string referring to the release version of the database you wish to locate.
#' It should have the same format as the date in the EPA download link, which is month, day, year, separated by
#' underscores ("%m_%d_%Y"). When missing, the most recent available copy is selected automatically.
#' @returns Returns a `character` string of the path.
#' `get_ecotox_path` will return the default directory of the database.
#' `get_ecotox_sqlite_file` will return the path to the sqlite file when it exists.
#' @rdname get_path
#' @name get_ecotox_path
#' @examples
#' get_ecotox_path()
#'
#' if (check_ecotox_availability()) {
#'   ## This will only work if a local database exists:
#'   get_ecotox_sqlite_file()
#' }
#' @author Pepijn de Vries
#' @family database-access-functions
#' @export
get_ecotox_path <- function() {
  getOption("ECOTOXr_path", {
    adir <- rappdirs::app_dir("ECOTOXr")$cache()
    udir <- tools::R_user_dir("ECOTOXr", "data")
    if (normalizePath(adir) |> suppressWarnings() !=
        normalizePath(udir) |> suppressWarnings()) {
      if (dir.exists(adir) && length(list.files(adir)) > 0) {
        ## This is a temporary check. It allows users to transition
        ## from `rappdirs` to `tools`
        cli::cli_warn(c(
          w = paste("You are using using an outdated `rappdirs` path for your ECOTOX data.",
                    "It may not work in future releases"),
          i = "Please call {.run ECOTOXr::migrate_ecotox_path()} to migrate data"
        ))
      }
    }
    udir
  })
}

#' Temporary function to switch to new ECOTOX path
#' 
#' Since ECOTOXr v1.2.5, the path is located with the base package `tools`
#' instead of the previously used `rappdirs`. Use this function to migrate
#' your old files to this new location.
#' @param overwrite `logical` value. Should files that already exist in the
#' new location be overwritten?
#' @returns Returns `NULL` invisibly.
#' @family database-access-functions
#' @export
migrate_ecotox_path <- function(overwrite = FALSE) {
  ## This is a temporary function. It allows users to transition
  ## from `rappdirs` to `tools`
  adir <- rappdirs::app_dir("ECOTOXr")$cache()
  udir <- tools::R_user_dir("ECOTOXr", "data")
  if (!dir.exists(udir)) dir.create(udir, recursive = TRUE)
  file.copy(from = list.files(adir, include.dirs = TRUE, full.names = TRUE),
            to = udir, recursive = TRUE, overwrite = overwrite)
  unlink(adir, recursive = TRUE)
  invisible()
}

#' Download and extract ECOTOX database files and compose database
#'
#' `r lifecycle::badge('stable')` In order for this package to fully function, a local copy of the ECOTOX database
#'  needs to be build. This function will download the required data and build the database.
#'
#' This function will attempt to find the latest download url for the ECOTOX database from the 
#' [EPA website](https://cfpub.epa.gov/ecotox/index.cfm) (see [get_ecotox_url()]).
#' When found it will attempt to download the zipped archive containing all required data. This data is then
#' extracted and a local copy of the database is build.
#'
#' Use '[suppressMessages()]' to suppress the progress report.
#' @section Known issues:
#' On some machines this function fails to connect to the database download URL from the
#' [EPA website](https://cfpub.epa.gov/ecotox/index.cfm) due to missing
#' SSL certificates. Unfortunately, there is no easy fix for this in this package. A work around is to download and
#' unzip the file manually using a different machine or browser that is less strict with SSL certificates. You can
#' then call [build_ecotox_sqlite()] and point the `source` location to the manually extracted zip
#' archive. For this purpose [get_ecotox_url()] can be used. Alternatively, one could try to call [download_ecotox_data()]
#' by setting `verify_ssl = FALSE`; but only do so when you trust the download URL from [get_ecotox_URL()].
#'
#' @param target Target directory where the files will be downloaded and the database compiled. Default is
#' [get_ecotox_path()].
#' @param write_log A `logical` value indicating whether a log file should be written to the target path
#  after building the SQLite database. See \code{\link{build_ecotox_sqlite}()} for more details. Default is
#' `TRUE`.
#' @param ask There are several steps in which files are (potentially) overwritten or deleted. In those cases
#' the user is asked on the command line what to do in those cases. Set this parameter to `FALSE` in order
#' to continue without warning and asking.
#' @inheritParams get_ecotox_url
#' @param ... Arguments passed on to [httr2::req_options()].
#' @returns Returns `NULL` invisibly.
#' @rdname download_ecotox_data
#' @name download_ecotox_data
#' @examples
#' \dontrun{
#' ## This will download and build the database in your temp dir:
#' if (interactive()) {
#'   download_ecotox_data(tempdir())
#' }
#' }
#' @author Pepijn de Vries
#' @family database-build-functions
#' @family online-functions
#' @export
download_ecotox_data <- function(
    target = get_ecotox_path(), write_log = TRUE, ask = interactive(),
    verify_ssl = getOption("ECOTOXr_verify_ssl"), ...) {
  if (!interactive() && ask)
    rlang::abort(c(
      x = "I cannot 'ask' if the session is not interactive",
      i = "Try setting 'ask' to `FALSE`"))
  if (is.null(verify_ssl)) verify_ssl <- TRUE
  avail <- check_ecotox_availability(target)
  if (avail && ask) {
    dbs <- paste(attributes(avail)$file$database, collapse = ", ")
    .unsuppressable_message(paste(
      "A local database already exists ({dbs}).",
      "Do you wish to continue and potentially overwrite",
      "the existing database? (y/n) "
      ), rlang::current_env())
    utils::flush.console()
    prompt <- readline(prompt = "")
    if (!startsWith("Y", toupper(prompt))) {
      cli::cli_alert_warning("Download aborted...")
      return(invisible(NULL))
    }
  }
  if (!dir.exists(target)) dir.create(target, recursive = TRUE)
  ## Obtain download link from EPA website:
  prgs_bar <-
    cli::cli_progress_step("Obtaining download link from EPA website...")
  link <- get_ecotox_url(verify_ssl, ...)
  dest_path <- file.path(target, utils::tail(unlist(strsplit(link, "/")), 1))
  cli::cli_progress_done(prgs_bar)
  proceed.download <- TRUE
  if (file.exists(dest_path) && ask) {
    .unsuppressable_message(
      "ECOTOX data is already present ({dest_path}), overwrite (y/n)? ",
      rlang::current_env())
    utils::flush.console()
    prompt <- readline(prompt = "")
    proceed.download <- startsWith("Y", toupper(prompt))
  }
  if (proceed.download) {
    cli::cli_alert_info("Start downloading ECOTOX data from {.href [EPA]({link})}")
    cfg <- list(...)
    if (!verify_ssl) {
      Sys.setenv(OPENSSL_CONF = system.file("openssl.cnf", package = "ECOTOXr"))
      cfg[["ssl_verifyhost"]] <- 0
      cfg[["ssl_verifypeer"]] <- 0
    }
    cfg[["noprogress"]] <- FALSE
    cfg[["xferinfofunction"]] <- function(down, up) {
      if (down[[1]] == 0) return(TRUE)
      cli::cli_progress_update(
        .envir = rlang::env_parent(),
        set = as.integer(down[[2]]),
        total = as.integer(down[[1]]))
      return(TRUE)
    }
    
    prgs_bar <- cli::cli_progress_bar(
      status = "Downloading",
      type = "download", total = 0)
    
    httr2::request(link) |>
      httr2::req_options(!!!cfg) |>
      httr2::req_perform(path = dest_path)
    
    cli::cli_progress_update(id = prgs_bar, force = TRUE)
    cli::cli_progress_done(prgs_bar)
    cli::cli_progress_cleanup()
    cli::cli_alert_success("Downloading done")
  }
  
  ## create bib-file for later reference
  .write_citation(dest_path, link)
  
  extr.path <- gsub(".zip", "", dest_path)
  proceed.unzip <- TRUE
  
  if (dir.exists(extr.path)) {
    test.files <- list.files(extr.path)
    if (length(test.files) >= 12 && any(test.files == "chemical_carriers.txt") && ask) {
      .unsuppressable_message(paste(
        "Extracted zip files already appear to exist.",
        "Continue unzipping and overwriting these files (y/n)? "),
        rlang::current_env())
      utils::flush.console()
      prompt <- readline(prompt = "")
      proceed.unzip <- startsWith("Y", toupper(prompt))
    }
  }
  
  if (proceed.unzip) .unzip_ecotox(ask, link, target)
  
  cli::cli_alert_info(paste(
    "Start constructing SQLite database from downloaded tables...",
    "Note that this may take some time...", sep = "\n"
  ))
  build_ecotox_sqlite(extr.path, target, write_log)
  return(invisible(NULL))
}

.write_citation <- function(dest_path, link) {
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
}

.unzip_ecotox <- function(ask, link, target, remove = TRUE) {
  prgs_bar <- cli::cli_progress_step("Extracting downloaded zip file...")
  exdir     <- gsub(".zip", "", basename(link))
  file_list <- utils::unzip(file.path(target, utils::tail(unlist(strsplit(link, "/")), 1)),
                            list = TRUE)$Name
  if (all(startsWith(file_list, exdir))) exdir <- ""
  utils::unzip(file.path(target, utils::tail(unlist(strsplit(link, "/")), 1)),
               exdir = file.path(target, exdir))
  cli::cli_progress_done(prgs_bar)
  if (ask) {
    .unsuppressable_message(
      "Done extracting zip file, remove it to save disk space (y/n)? ",
      rlang::current_env()
    )
    utils::flush.console()
    if (startsWith("Y", toupper(readline(prompt = ""))) || remove) {
      
      prgs_bar <- cli::cli_progress_step("Trying to delete zip file...")
      tryCatch({
        file.remove(file.path(target, utils::tail(unlist(strsplit(link, "/")), 1)))
        cli::cli_process_done(prgs_bar)
      }, error = function(e) {
        cli::cli_progress_done(result = "failed")
      })
    }
  }
}

#' Build an SQLite database from zip archived tables downloaded from EPA website
#'
#' `r lifecycle::badge('stable')` This function is called automatically after [download_ecotox_data()]. The database
#' files can also be downloaded manually from the [EPA website](https://cfpub.epa.gov/ecotox/) from which a local
#' database can be build using this function.
#'
#' Raw data downloaded from the EPA website is in itself not very efficient to work with in R. The files are large
#' and would put a large strain on R when loading completely into the system's memory. Instead use this function
#' to build an SQLite database from the tables. That way, the data can be queried without having to load it all into
#' memory.
#'
#' EPA provides the raw table from the [ECOTOX database](https://cfpub.epa.gov/ecotox/) as text files with
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
#' Use '[suppressMessages()]' to suppress the progress report.
#'
#' @param source A `character` string pointing to the directory path where the text files with the raw
#' tables are located. These can be obtained by extracting the zip archive from <https://cfpub.epa.gov/ecotox/>
#' and look for 'Download ASCII Data'.
#' @param destination A `character` string representing the destination path for the SQLite file. By default
#' this is [get_ecotox_path()].
#' @param write_log A `logical` value indicating whether a log file should be written in the destination path
#  after building the SQLite database. See \code{\link{build_ecotox_sqlite}()} for more details. Default is
#' `TRUE`. The log contains information on the source and destination path, the version of this package,
#' the creation date, and the operating system on which the database was created.
#' @returns Returns `NULL` invisibly.
#' @rdname build_ecotox_sqlite
#' @name build_ecotox_sqlite
#' @examples
#' source_path <- tempfile()
#' dir.create(source_path)
#' 
#' ## This is a small mockup file resembling the larger zip
#' ## files that can be downloaded with `download_ecotox_data()`:
#' 
#' source_file <- system.file("ecotox-test.zip", package = "ECOTOXr")
#' 
#' unzip(source_file, exdir = source_path)
#' 
#' build_ecotox_sqlite(source_path, tempdir())
#' @author Pepijn de Vries
#' @family database-build-functions
#' @export
build_ecotox_sqlite <- function(source, destination = get_ecotox_path(), write_log = TRUE) {
  Sys.setenv("VROOM_CONNECTION_SIZE" = "41950304") # Increase the size for read_table
  dbname <- paste0(basename(source), ".sqlite")
  dbcon  <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(destination, dbname))
  on.exit({
    RSQLite::dbDisconnect(dbcon)
  })
  unexpected_fields <- character(0)
  missing_fields    <- character(0)
  missing_tables    <- character(0)
  incomplete_check  <- character(0)
  
  ## Loop the text file tables and add them to the sqlite database 1 by 1
  i <- 0

  n_tables <- length(unique(.db_specs$table))
  lines <- 0
  tab <- .db_specs[1,]
  progress_msg <- paste(
    "{cli::pb_spin} Table ({.val {i}}/{n_tables}), added {.strong {lines}} lines",
    "to '{tab$table[[1]]}'"
  )
  prgs_bar <- cli::cli_progress_message(
    progress_msg, format_done = "Done",
    format_failed = "Failed", .auto_close = FALSE)
  by(.db_specs, .db_specs$table, function(tab) {
    i <<- i + 1
    cli::cli_progress_update(id = prgs_bar)
    filename <- file.path(source, paste0(tab$table[[1]], ".txt"))
    if (!file.exists(filename)) filename <- file.path(source, "validation", paste0(tab$table[[1]], ".txt"))
    if (!file.exists(filename)) {
      missing_tables <<- c(missing_tables, tab$table[[1]])
      cli::cli_alert_warning(paste(
        "File for table '{tab$table[[1]]}' does not exist.",
        "This may occur for older ECOTOX releases.",
        "Will try to continue without this table."))
      return(NULL)
    }
    
    ## Remove table from database if it already exists
    RSQLite::dbExecute(dbcon, sprintf("DROP TABLE IF EXISTS [%s];", tab$table[[1]]))
    
    ## specify query to create the table in the sqlite database
    foreign_keys <- tab[tab$foreign_key != "",, drop = FALSE]
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
    lines <- "1"
    ## Copy tables in 50000 line fragments to database, to avoid memory issues
    frag.size  <- 50000
    cli::cli_progress_update(id = prgs_bar)
    repeat {
      if (is.null(head)) {
        head <- iconv(readr::read_lines(filename, skip = 0, n_max = 1, progress = FALSE), to = "UTF8", sub = "*")
      } else {
        testsize   <- ifelse(lines.read == 1, frag.size - 1, frag.size)
        ## readr sometimes generates warnings for possible parsing problems (inherited from vroom)
        ## however, running 'readr::problems' does not show any issues, muffle this warning
        ## if this is the case:
        body       <- withCallingHandlers({
          chunk <- readr::read_lines(filename, skip = lines.read, n_max = testsize, progress = FALSE)
        }, warning = function(w) if (nrow(readr::problems(chunk)) == 0) rlang::cnd_muffle(w))
        body       <- suppressWarnings({iconv(body, to = "UTF8", sub = "*")})
        ## Some records incorrectly contain line feed characters. Replace with space:
        body       <- suppressWarnings({gsub("\U000D", " ", body)})
        ## Replace pipe-characters with dashes when they are between brackets "("and ")",
        ## These should not be interpreted as table separators and will mess up the table.read call
        body       <- stringr::str_replace_all(body, "(?<=\\().+?(?=\\))", function(x){
          ## there should not be another opening bracket, double pipe or forward slash! in that case leave as is
          ifelse (grepl("[\\(/]", x) | grepl("||", x, fixed = TRUE), x, gsub("[|]", "-", x))
        })
        
        lines.read <- lines.read + length(body)
        
        ## Join lines when number of pipes is to small (probably caused by unintended linefeed)
        repeat{
          count_pipes <- unlist(lapply(regmatches(body, gregexpr("[|]", body)), length))
          join_lines  <- which(count_pipes < length(regmatches(head, gregexpr("[|]", head))[[1]]))[1:2]
          if (length(join_lines) > 0 & !any(is.na(join_lines))) {
            body        <- c(body[-join_lines], paste(body[join_lines], collapse = " "))
          } else break
        }
        
        ## strip.white is set to FALSE, as they occur in primary keys!
        table.frag <- utils::read.table(text = c(head, body),
                                        sep = "|", header = TRUE, quote = "", comment.char = "",
                                        stringsAsFactors = FALSE, strip.white = FALSE)
        
        missing_cols    <- tab$field_name[!tab$field_name %in% colnames(table.frag)]
        unexpected_cols <- colnames(table.frag)[!colnames(table.frag) %in% tab$field_name]
        if (length(unexpected_cols) > 0)
          unexpected_fields <<- union(unexpected_fields, paste(tab$table[[1]], unexpected_cols, sep = "."))
        if (length(missing_cols) > 0)
          missing_fields    <<- union(missing_fields, paste(tab$table[[1]], missing_cols, sep = "."))
        if (RSQLite::dbExistsTable(dbcon, tab$table[[1]]) && ("PRIMARY KEY" %in% tab$primary_key)) {
          prim_key <- which(tab$primary_key == "PRIMARY KEY")
          RSQLite::dbWriteTable(dbcon, "temp",
                                table.frag[,setdiff(tab$field_name, missing_cols), drop = FALSE], overwrite = TRUE)
          
          ## When the primary key is not unique, update the table using the last occurrence of the primary key.
          updt <- dbplyr::sql_query_upsert(dbcon, tab$table[[1]], "temp",
                                           by = names(table.frag)[prim_key],
                                           update_cols = names(table.frag)[-prim_key])
          written_len <- RSQLite::dbExecute(dbcon, updt)
          if (written_len < nrow(table.frag)) {
            cli::cli_alert_warning(paste(
              "Table '{tab$table[[1]]}' contains less records than read from source.",
              "Likely cause: duplicate records in source."
            ))
            incomplete_check <- union(incomplete_check, tab$table[[1]])
          }
          invisible(RSQLite::dbExecute(dbcon, "DROP TABLE IF EXISTS temp;"))
        } else {
          RSQLite::dbWriteTable(dbcon, tab$table[[1]],
                                table.frag[,setdiff(tab$field_name, missing_cols), drop = FALSE], append = TRUE)
        }
        
        lines <- format(lines.read, scientific = FALSE)
        cli::cli_progress_update(id = prgs_bar)
        if (length(body) < testsize) break
      }
    }
    if (any(startsWith(unexpected_fields, paste0(tab$table[[1]], ".")))) {
      unexpects <-
        paste(unexpected_fields[startsWith(unexpected_fields, paste0(tab$table[[1]], "."))], collapse = "', '")
      cli::cli_alert_warning("Ignored unexpected column(s) '{unexpects}'")
    }
    if (any(startsWith(missing_fields, paste0(tab$table[[1]], ".")))) {
      missing_cols <- paste(missing_fields[startsWith(missing_fields, paste0(tab$table[[1]], "."))], collapse = "', '")
      cli::cli_alert_warning("Missing column(s) '{missing_cols}'.")
    }
  })
  cli::cli_progress_done(prgs_bar)
  
  if (write_log) {
    logfile      <- file.path(destination, paste0(basename(source), ".log"))
    downloadinfo <- file.path(destination, paste0(basename(source), "_cit.txt"))
    if (file.exists(logfile)) invisible(file.remove(logfile))
    writeLines(text = sprintf(
      paste(c(
        "ECOTOXr SQLite log\n",
        "Source:        %s", "Destination:   %s", "Download info: %s", "Build with:    %s",
        "Build on:      %s", "Build date:    %s", "Missing tbls:  %s", "Missing flds:  %s",
        "Unexp. fields: %s", "Incomplete?:   %s"), collapse = "\n"),
      source,
      destination,
      ifelse(file.exists(downloadinfo), downloadinfo, "Not available"),
      paste0("ECOTOXr V", utils::packageVersion("ECOTOXr")),
      paste(Sys.info()[c("sysname", "release")], collapse = " "),
      format(Sys.Date(), "%Y-%m-%d"),
      paste(missing_tables, collapse = ", "),
      paste(missing_fields, collapse = ", "),
      paste(unexpected_fields, collapse = ", "),
      paste(incomplete_check, collapse = ", ")
    ),
    con = logfile)
    cli::cli_alert_success("Completed building the database")
  }
  return(invisible(NULL))
}
