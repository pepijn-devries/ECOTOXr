#' Process ECOTOX search results by converting `character` to dates where relevant
#' 
#' `r lifecycle::badge('experimental')` The function `search_ecotox()` returns fields
#' from the ECOTOX database as is. Fields that represent dates are usually formatted as
#' `"%m\%d\%Y"`. Unfortunately, this format is not consistently used throughout the
#' database. `process_ecotox_dates()` takes a `data.frame` returned by
#' `search_ecotox()`, locates date columns, represented by text, sanitises the text
#' and converts them to `Date` objects. It will sanitise the date fields as much as possible.
#' It will correct most dates. Dates without a specified calender year, a date range,
#' illegal date format (even after sanitation) are returned as `NA`.
#' @param x A `data.frame` obtained with `search_ecotox()`, for which the dates need
#' to be processed.
#' @param .fns Function to convert `character` to `Date`. By default `as_date_ecotox()`
#' is used which also sanitises the input. You can also use `as.Date()` if you don't
#' want the sanitation step. You can also write a custom function.
#' @param ... Arguments passed to `.fns`.
#' @param .names A 'glue' specification used to rename the date columns. By default
#' it is `"{.col}"`, which will overwrite existing text columns with date columns.
#' You can for instance add a suffix with `"{.col}_date"` if you want to
#' rename the resulting date columns.
#' @returns Returns a `data.frame` in which the columns containing date information
#' is converted from the character format from the database to actual date objects (
#' `"POSIXlt"` and `"POSIXct"`).
#' @author Pepijn de Vries
#' @examples
#' if (check_ecotox_availability()) {
#'   df <- search_ecotox(
#'     list(
#'       latin_name    = list(
#'         terms          = c("Skeletonema", "Daphnia"),
#'         method         = "contains"
#'       ),
#'       chemical_name = list(
#'         terms          = "benzene",
#'         method         = "exact"
#'       )
#'     ), list_ecotox_fields("full"))
#'
#'   df_dat <-
#'     process_ecotox_dates(df, warn = FALSE)
#' }
#' @family ecotox-sanitisers
#' @export
process_ecotox_dates <- function(x, .fns = as_date_ecotox, ..., .names = NULL) {
  ## identify date columns
  date_columns <- .db_specs$field_name[grepl("_date$", .db_specs$field_name)]
  patt <- sprintf("^.*?(%s)$",
                  paste(c(date_columns, "_date"), collapse = "|"))
  
  fun <- function(x) { .fns(x, ...) }
  x |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches(patt, perl = TRUE),
        .fns  = fun,
        .names = .names
      )
    )
}

#' Values represented by ECOTOX `character` to dates
#' 
#' `r lifecycle::badge('experimental')` Similar to `as.Date()`, but it also
#' performs some text sanitising before coercing text to dates.
#' 
#' The following steps are performed (in the order as listed)
#' to sanitise text before coercing it to numerics:
#' 
#'  * Trim whitespaces
#'  * Replace hyphens with forward slashes
#'  * Replace double forward slashes, forward slashes followed by a zero and spaces,
#'    with a single forward slash
#'  * Replace `"mm"` or `"dd"` (case insensitive) with the value specified as argument.
#'    Add a forward slash to it when missing.
#'  * Treat `"na"`, `"nr"`, `"xx"` and `"00"` (case insensitive) as unreported
#'    values when followed by a forward slash. Replace it with the `nr` argument
#'  * Remove alphabetical characters when directly followed by a numerical character.
#'  * Replace literal month names with its numerical calendar value (1-12).
#'  * When the date consists of one value, assume it is a calender year and add
#'    `dd` and `mm` as day and month value.
#'  * If a date consists of two numbers, assume it is month, followed by year. In that
#'    case insert the `dd` value for the day.
#' 
#' It is your own responsibility to check if the sanitising steps are appropriate for
#' your analyses.
#' @param x A vector of `character` strings. It expects fields as commonly returned
#' from the ECOTOX database.
#' @param dd Replacement values for unspecified days in a date. Defaults to `1L`. If
#' you want dates with unspecified days to result in `NA`, use `dd = -1L`.
#' @param mm Replacement values for unspecified months in a date. Defaults to `1L`. If
#' you want dates with unspecified months to result in `NA`, use `mm = -1L`.
#' @param nr Replacement values for generically unspecified values in a date.
#' Defaults to `1L`. If you want dates with unspecified values to result in `NA`,
#' use `nr = -1L`.
#' @param ... Passed to `as.Date()`.
#' @param warn If set to `FALSE` warnings while converting text to dates are suppressed.
#' @returns A vector of `Date` class objects with the same length as `x`.
#' @author Pepijn de Vries
#' @examples
#' ## a vector of commonly used notations in the database to represent
#' ## dates. Most frequent format is %m/%d/%Y
#' char_date <- c("5-19-1987   ", "5/dd/2021", "3/19/yyyy", "1985", "mm/19/1999",
#'                "October 2004", "nr/nr/2015")
#' 
#' as_date_ecotox(char_date)
#' 
#' ## Set unspecified days to 15:
#' as_date_ecotox(char_date, dd = 15L)
#' 
#' ## Unspecified days should result in NA:
#' as_date_ecotox(char_date, dd = -1L)
#' 
#' ## Set unspecified months to 6:
#' as_date_ecotox(char_date, mm = 6L)
#' 
#' ## Set generically unspecified value to 6:
#' as_date_ecotox(char_date, nr = 6L)
#' @family ecotox-sanitisers
#' @export
as_date_ecotox <- function(x, dd = 1L, mm = 1L, nr = 1L, ..., warn = TRUE) {
  if (typeof(x) != "character") stop(
    paste("`as_date_ecotox` should only convert `characters`.",
          "I got", typeof(x), "instead."))
  
  # Declare variables to pass CRAN checks
  .data <- NULL
  
  args <- list(...)
  if (is.null(args$format)) args$format <- "%m/%d/%Y"
  fun <- \(x) do.call(as.Date, c(list(x = x), args))
  
  dplyr::tibble(date = x) |>
    dplyr::mutate(
      date_sane = trimws(.data$date),
      date_sane = gsub("-", "/", .data$date_sane),
      date_sane = gsub("//|/0|[ ]", "/", .data$date_sane),
      date_sane = gsub("(MM|mm)(?!/)", paste0(mm, "/"), .data$date_sane, perl = TRUE),
      date_sane = gsub("(DD|dd)(?!/)", paste0(dd, "/"), .data$date_sane, perl = TRUE),
      date_sane = gsub("dd", dd, .data$date_sane, ignore.case = TRUE),
      date_sane = gsub("mm", mm, .data$date_sane, ignore.case = TRUE),
      date_sane = gsub("na/|nr/|xx/00/", paste0(nr, "/"), .data$date_sane, ignore.case = TRUE),
      date_sane = gsub("/0/", paste0("/", nr, "/"), .data$date_sane, ignore.case = TRUE),
      date_sane = gsub("[a-z|A-Z](?=[0-9])", "", .data$date_sane, perl = TRUE),
      date_sane = {
        ds <- .data$date_sane
        for (m in 1:12) {
          ds <- gsub(month.name[m], m, ds, ignore.case = TRUE)
        }
        ds
      },
      date_split = strsplit(.data$date_sane, "/"),
      date_sane = ifelse(lengths(.data$date_split) == 1, {
        result <- .data$date_sane
        result[nchar(result) == 4] <- sprintf("%i/%i/%s", mm, dd,
                                              result[nchar(result) == 4])
        result
      }, ifelse(lengths(.data$date_split) == 2, {
        ## Assume that the two parts are 'month' and 'year'
        lapply(.data$date_split, \(x) {
          if (length(x) == 2) sprintf("%s/%s/%s", x[[1]], dd, x[[2]]) else paste(x, collapse = "/")
        }) |> unlist()
      }, .data$date_sane)),
      date_proc = fun(.data$date_sane)
    ) |>
    dplyr::pull("date_proc")
}
