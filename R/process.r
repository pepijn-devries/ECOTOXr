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