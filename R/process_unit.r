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
    tibble(code = x) |>
    dplyr::mutate(
      code          = trimws(.data$code),
      ## remove square brackets and commas
      code          = gsub("\\[|\\]|,", "", .data$code),
      ## remove space after numeric followed by character
      code          = gsub("(?<=[0-9]) (?=[a-z|A-Z])", "", .data$code, perl = TRUE),
      ## remove AI and AE prefixes
      code          = gsub("^(ai|AI|ae|AE) ", "", .data$code),
      ## remove Carbon and organic matter annotation
      code          = gsub(" [C|OM](?=$|[ :/*^])", "", .data$code, perl = TRUE),
      ## standardise scientific notation in numeric component
      code          = gsub("10[xX]", "1e", .data$code),
      ## ensure 'wk' is parsed as week
      code          = gsub("wk", "week", .data$code, perl = TRUE),
      ## 'lbs' is not recognised as plural of 'lb' ## TODO not all lbs values are captured
      code          = gsub("(?<=^|[ :/*^])lbs(?=$|[ :/*^])", "lb", .data$code, perl = TRUE),
      ##TODO regex to match "Nm" between space, asterisk or forward slash, also at start or end
      code          = gsub("(?<=^|[ :/*^])Nm(?=$|[ :/*^])", "nm", .data$code, perl = TRUE),
      code          = gsub("(?<=^|[ :/*^])BW(?=$|[ :/*^])", "bodyweight", .data$code, perl = TRUE),
      ## TODO look for more unit annotations
      code          = gsub("([ ]?)soil", "", .data$code, ignore.case = TRUE, perl = TRUE),
      
      ## 'type' specific sanitation steps
      code          = if (type == "concentration") {
        
        result <- .data$code
        ## in case of type 'concentration' 'K' stands for Karmen units
        result <- gsub("(?<=^|[ :/*^])K(?=$|[ :/*^])", "Karmen", result, perl = TRUE)
        ## in case of concentration dpm is disintegrations (counts) per minute
        result <- gsub("(?<=^|[ :/*^])dpm(?=$|[ :/*^])", "counts/min", result, perl = TRUE)
        result
        
      } else          if (type == "media") {
        
        result <- .data$code
        ## in case of type 'media' 'C' stands for Celcius  
        result <- gsub("(?<=^|[ :/*^])C(?=$|[ :/*^])", "Celsius", result, perl = TRUE)
        ## in case of type 'media' 'K' stands for Kelvin  
        result <- gsub("(?<=^|[ :/*^])K(?=$|[ :/*^])", "Kelvin", result, perl = TRUE)
        result
        
      } else .data$code,
      
      ## rename all equivalents of 'day' to 'day'
      code        = gsub("(?<=^|[ :/*^])BDAY|dayg|dayef|dayl|dayl|daym|dayv|hd|dapu|dbh|dph|dge|dla|dpe|dpel|dpes|dpf|dpfg|dpfl|dpgm|dph|dphv|dpm|dpmm|dpn|dpo|dpp|dpr|dpw|dpref|dps|dpu|dws|dpys(?=$|[ :/*^])", "day", .data$code, perl = TRUE),
      ## Use 'unit' by default as arbitrary unit
      code        = ifelse(.data$code == "", "unit", .data$code)
    )
  
  x <-
    x |>
    pull("code") |>
    lapply(
      \(y) {
        tryCatch({
          units::mixed_units(1, y) |> fun()
        }, error = function(e) units::mixed_units(1, "unit"))
      }) |>
    fun()
  
  do.call(c, x)
}
