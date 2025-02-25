#' Process ECOTOX search results by converting `character` to `numeric` where relevant
#' 
#' `r lifecycle::badge('experimental')` The function `search_ecotox()` returns fields
#' from the ECOTOX database as is. Many numeric values are stored in the database as
#' text. It is not uncommon that these text fields cannot be converted directly and need
#' some sanitising first. `process_ecotox_numerics()` takes a `data.frame` returned by
#' `search_ecotox()`, locates numeric columns, represented by text, sanitises the text
#' and converts them to numerics.
#' @param x A `data.frame` obtained with `search_ecotox()`, for which the numerics need
#' to be processed.
#' @param .fns Function to convert `character` to `numeric`. By default `as_numeric_ecotox()`
#' is used which also sanitises the input. You can also use `as.numeric()` if you don't
#' want the sanitation step. You can also write a custom function.
#' @param ... Arguments passed to `.fns`.
#' @param .names A 'glue' specification used to rename the numeric columns. By default
#' it is `"{.col}"`, which will overwrite existing text columns with numeric columns.
#' You can for instance add a suffix with `"{.col}_num"` if you want to
#' rename the resulting numeric columns.
#' @returns Returns a `data.frame` in which the columns containing numeric information
#' is converted from the character format from the database to actual numerics.
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
#'   df_num <-
#'     process_ecotox_numerics(df, warn = FALSE)
#' }
#' @family ecotox-sanitisers
#' @export
process_ecotox_numerics <- function(x, .fns = as_numeric_ecotox, ..., .names = NULL) {

  ## identify numeric columns
  ## numeric columns are columns that have a similarly named unit column
  ## and does not contain with 'unit', 'code' '_type', '_op' or '_comments'
  numeric_columns <- .db_specs$field_name[grepl("_unit$", .db_specs$field_name)]
  numeric_columns <- gsub("_unit$", "", numeric_columns) |> unique()
  patt <- sprintf("^(%s)((?!description|unit|code|_type|_op|_comments).)*$",
                  paste(numeric_columns, collapse = "|"))

  ## coerce identified columns to numeric using .fns
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

#' Values represented by ECOTOX `character` to `numeric`
#' 
#' `r lifecycle::badge('experimental')` Similar to `as.numeric()`, but it also
#' performs some text sanitising before coercing text to numerics.
#' 
#' The following steps are performed to sanitise text before coercing it to numerics:
#' 
#'  * Notes labelled with `"x"` or `"\*"` are removed.
#'  * Operators (`">"`, `">="`, `"<"`, `"<="`, `"~"`, `"="`, `"ca"`, `"er"`)
#'    are removed.
#'  * Text between brackets (`"()"`) is removed (including the brackets)
#'  * Comma's are considered to be a thousand separator when they are located
#'    at any fourth character (from the right) and removed. Comma's at any other location
#'    is assumed to be a decimal separator and is replaced by a period.
#'  * If there is a hyphen present (not preceded by an "`"e"` or `"E"`) it is probably
#'    representing a range of values. When `range_fun` is `NULL` it will result in a `NA`.
#'    Otherwise, the numbers are split at the hyphen and aggregated with `range_fun`
#' 
#' It is your own responsibility to check if the sanitising steps are appropriate for
#' your analyses.
#' @param x A vector of `character` strings. It expects fields as commonly returned
#' from the ECOTOX database.
#' @param range_fun Function to summarise range values. If `NULL` range values are
#' returned as `NA`
#' @param ... Arguments passed to `range_fun`.
#' @param warn If set to `FALSE` warnings while converting text to numerics are suppressed.
#' @returns A vector of `numeric` values with the same length as `x`.
#' @author Pepijn de Vries
#' @examples
#' ## a vector of commonly used notations in the database to represent
#' ## numeric values 
#' char_num <- c("10", " 2", "3 ", "~5", "9.2*", "2,33",
#'               "2,333", "2.1(1.0 - 3.2)", "1-5", "1e-3")
#' 
#' ## Text fields reported as ranges are returned as `NA`:
#' as_numeric_ecotox(char_num, warn = FALSE)
#' 
#' ## Text fields reported as ranges are processed with `range_fun`
#' as_numeric_ecotox(char_num, range_fun = median)
#' @family ecotox-sanitisers
#' @export
as_numeric_ecotox <- function(x, range_fun = NULL, ..., warn = TRUE) {
  if (typeof(x) != "character") stop(
    paste("`as_numeric_ecotox` should only convert `characters`.",
          "I got", typeof(x), "instead."))
  regex_note   <- "[*x/]"
  regex_oper   <- "^(er)|(ca)|(>)|(>=)|(<)|(<=)|(~)|(=)"
  regex_brac   <- "\\((.*?)\\)"
  regex_rnge   <- "[0-9](?<!e|E)-"
  has_notation <- grepl(regex_note, x)
  has_operator <- grepl(regex_oper, x)
  has_brackets <- grepl(regex_brac, x)
  
  ## Assume that commas are thousand separators when occurring every fourth character,
  ## else assume it is a decimal separator

  commas <- gregexpr(",", x)
  idx    <- which(lapply(commas, function(x) x[[1L]] != -1L) |> unlist())

  x[idx] <-
    lapply(idx, function(i) {
      pos  <- nchar(x[i]) - commas[[i]] + 1L
      repl <- ifelse(all(pos %% 4L) == 0, "", ".")
      gsub(",", repl, x[i])
    }) |>
    unlist()

  ## strip whitespaces, note characters, operators and text between brackets
  x <- gsub(regex_note, "", x)
  x <- gsub(regex_oper, "", x)
  x <- gsub(regex_brac, "", x)

  has_ranges   <- grepl(regex_rnge, x, perl = TRUE)
  if (any(has_ranges) && !is.null(range_fun)) {
    x[has_ranges] <-
      x[has_ranges] |>
      strsplit("(?<!e|E)-", perl = TRUE) |>
      lapply(as.numeric) |>
      lapply(range_fun, ...) |>
      unlist()
  }
  
  x <- if (warn) as.numeric(x) else suppressWarnings(as.numeric(x))
  if (any(has_notation)) attributes(x)$has_notation <- has_notation
  if (any(has_brackets)) attributes(x)$has_brackets <- has_brackets
  if (any(has_operator)) attributes(x)$has_brackets <- has_operator
  if (any(has_ranges))   attributes(x)$has_brackets <- has_ranges
  x
}

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
                  paste(date_columns, collapse = "|"))
  
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

#' Process ECOTOX search results by converting `character` to units where relevant
#' 
#' `r lifecycle::badge('experimental')` The function `search_ecotox()` returns fields
#' from the ECOTOX database as is. Fields that represent units are not standardised in
#'  the database. Therefore, this format is not consistently used throughout the
#' database. `process_ecotox_units()` takes a `data.frame` returned by
#' `search_ecotox()`, locates unit columns, represented by text, sanitises the text
#' and converts them to `?units::units` objects. It will sanitise the unit fields as
#' much as possible. *TODO* `NA`
#' @param x A `data.frame` obtained with `search_ecotox()`, for which the units need
#' to be processed.
#' @param .fns Function to convert `character` to unit. By default `as_unit_ecotox()`
#' is used which also sanitises the input. You can also write a custom function.
#' @param ... Arguments passed to `.fns`.
#' @param .names A 'glue' specification used to rename the unit columns. By default
#' it is `"{.col}"`, which will overwrite existing text columns with unit columns.
#' You can for instance add a suffix with `"{.col}_unit"` if you want to
#' rename the resulting unit columns.
#' @returns Returns a `data.frame` in which the columns containing unit information
#' is converted from the character format from the database to actual unit objects (
#' `?units::units`).
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
#'   df_unit <-
#'     process_ecotox_units(df, warn = FALSE)
#' }
#' @family ecotox-sanitisers
#' @export
process_ecotox_units <- function(x, .fns = as_unit_ecotox, ..., .names = NULL) {
  ## identify unit columns
  unit_columns <- .db_specs$field_name[grepl("_unit$", .db_specs$field_name)]
  patt <- sprintf("^.*?(%s)$",
                  paste(unit_columns, collapse = "|"))
  
  x |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches(patt, perl = TRUE),
        .fns  = ~ {
          ## TODO only pass 'type' when .fns is 'as_unit_ecotox'
          .fns(.x, type = .field_to_unit_type(dplyr::cur_column()), ...)
        },
        .names = .names
      )
    )
}

.field_to_unit_type <- function(field) {
  type <- .db_specs$foreign_key[endsWith(field, .db_specs$field_name)]
  type <- gsub("[_].*", "", type) |> unique()
  if (type == "sample") type <- "size"
  type
}

#' Values represented by ECOTOX `character` to units
#' 
#' `r lifecycle::badge('experimental')` Convert text to units after
#' sanitising.
#' 
#' The following steps are performed (in the order as listed)
#' to sanitise text before coercing it to units:
#' 
#'  * TODO
#' 
#' It is your own responsibility to check if the sanitising steps are appropriate for
#' your analyses.
#' @param x A vector of `character` strings. It expects fields as commonly returned
#' from the ECOTOX database.
#' @param type The type of unit that can help the sanitation process. See the 'usage'
#' section for available options. These options are linked to the different unit tables
#' in the database (see `vignette("ecotox-schema")`). It can help to interpret ambiguous
#' units correctly. For instance, 'dpm' can both mean 'disintegrations per minute'
#' (`type = "concentration"`) and 'days post-moult' (`type = "duration"`).
#' @param ... TODO
#' @param warn If set to `FALSE` warnings while converting text to units are suppressed.
#' @returns A vector of `?units::unit` class objects with the same length as `x`.
#' @author Pepijn de Vries
#' @examples
#' ## TODO
#' @family ecotox-sanitisers
#' @export
as_unit_ecotox <- function(
    x,
    type = c("concentration", "duration", "length", "media", "application", "size", "weight"),
    ..., warn = TRUE) {
  if (typeof(x) != "character") stop(
    paste("`as_unit_ecotox` should only convert `characters`.",
          "I got", typeof(x), "instead."))
  
  type <- rlang::arg_match(type)
  # Declare variables to pass CRAN checks
  .data <- NULL
  
  fun <- if (warn) \(x) x else suppressWarnings
  
  x <-
    lapply(
      x,
      \(y) {
        tryCatch({
          units::mixed_units(1, y)
        }, error = function(e) units::mixed_units(1, "1"))
      }) |>
    fun()
  do.call(c, x)
}
