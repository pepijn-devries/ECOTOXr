#' Process ECOTOX search results by converting `character` to units where relevant
#' 
#' `r lifecycle::badge('experimental')` The function `search_ecotox()` returns fields
#' from the ECOTOX database as is. Fields that represent units are not standardised in
#' the database. Therefore, this format is not consistently used throughout the
#' database. `process_ecotox_units()` takes a `data.frame` returned by
#' `search_ecotox()`, locates unit columns, represented by text, sanitises the text
#' and converts them to `units::mixed_units()` objects. It will sanitise the unit fields as
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
      ## remove known prefixes
      code          = gsub("^(ae|AE|ai|AI|fl|litter) ", "", .data$code),
      ## double spaces to single spaces
      code          = gsub("  ", " ", .data$code),
      ## remove spaces around multiply symbol
      code          = gsub(" \\* ", "*", .data$code),
      ## standardise scientific notation in numeric component
      code          = gsub("10[xX]", "1e", .data$code),
      ## interpret '0/00' as parts per thousand.
      code          = gsub("0/00", "ppt", .data$code, fixed = TRUE),
      ## ensure 'wk' is parsed as week
      code          = gsub("wk", "week", .data$code, perl = TRUE),
      ## 'lbs' is not recognised as plural of 'lb' ## TODO not all lbs values are captured
      code          = gsub("(?<=^|[ :/*^])lbs(?=$|[ :/*^])", "lb", .data$code, perl = TRUE),
      ##TODO regex to match "Nm" between space, asterisk or forward slash, also at start or end
      code          = gsub("(?<=^|[ :/*^])Nm(?=$|[ :/*^])", "nm", .data$code, perl = TRUE),
      ## 'ac' is 'acre'
      code          = gsub("(?<=^|[ :/*^])ac(?=$|[ :/*^])", "acre", .data$code, perl = TRUE),
      ## replace common count names with 'count'
      code          = gsub("(?<=^|[ :/*^])(beat|(cell(s?))|no|org)(?=$|[ :/*^])", "count", .data$code, perl = TRUE),
      ## remove various annotations (space followed by one of 'known' annotations)
      code          = gsub(" (act|AE|arb|ash|bdwt|BO|bt|BW|C|C2H4|canopy|cap|CCT|CEC|change|clay|clitellate|CNTL|CO2|com|corn|cortex|CREA|dev|diet|DNA|dose|dry|DT|earliness|egg|eu|EU|FATL|fertile|fl|fluid|GAIN|H20|Hb|Hg|in|ingested|INHIB|initial|intake|lit|litter|max|media|min|MIT|N|NH3|node|O2|OC|of|oil|OM|org|P|PC|PLIPD|plt|pod|pro|prod|PRTL|ret|RI|RNA|S|sat|sd|sed|seed|soil|soln|solvent|SP|sperm|TI|TIME|tolerance|total|ureau|WDTH|wet|wght|WGHT|WSF|wt|yld|Zn)(?=$|[ :/*^])",
                           "", .data$code, perl = TRUE),
      ## percentage preceded by decade of grams is in the mass unit per decilitre
      code          = gsub("g([ ]?)%( w/v)?", "g/dL", .data$code),
      ## percentage concentration mass over volume is more explicitly
      ## expressed as hectogram per millilitre
      code          = gsub("^%([ ]?)(m|w)(:|/)v$", "hg/L", .data$code),
      ## percentage concentration volume over volume is more explicitly
      ## expressed as litre per hectolitre
      code          = gsub("^%([ ]?)v(:|/)v$", "L/hL", .data$code),
      ## percentage concentration mass over mass is more explicitly
      ## expressed as grams per hectograms
      code          = gsub("^%([ ]?)(m|w)(:|/)(m|w)$", "g/hg", .data$code),
      ## percentage concentration volume over mass is more explicitly
      ## expressed as grams per decalitre
      code          = gsub("^%([ ]?)v(:|/)(m|w)$", "g/dL", .data$code),

      ## 'type' specific sanitation steps
      code          = if (type == "concentration") {
        
        result <- .data$code
        ## in case of type 'concentration' 'K' stands for Karmen units
        result <- gsub("(?<=^|[ :/*^])K(?=$|[ :/*^])", "Karmen", result, perl = TRUE)
        ## in case of concentration dpm is disintegrations (counts) per minute
        result <- gsub("(?<=^|[ :/*^])(c|d)pm(?=$|[ :/*^])", "counts/min", result, perl = TRUE)
        ## mM, uM, M is mmol/L, mmol/L and mol/L respectively
        result <- gsub("((?<=^|[ :/*^])|(?<=^|[ :/*^][u|m]))M(?=$|[ :/*^])", "mol/L", result, perl = TRUE)
        result
        
      } else          if (type == "media") {
        
        result <- .data$code
        ## in case of type 'media' 'C' stands for Celsius  
        result <- gsub("(?<=^|[ :/*^])C(?=$|[ :/*^])", "Celsius", result, perl = TRUE)
        ## in case of type 'media' 'K' stands for Kelvin  
        result <- gsub("(?<=^|[ :/*^])K(?=$|[ :/*^])", "Kelvin", result, perl = TRUE)
        result
        
      } else .data$code,
      
      code          = {
        ## scientific notation is not always parsed correctly by units package.
        ## Therefore, the scientific notation is replaced by decimal notation
        num <-
          stringr::str_extract_all(.data$code, "\\d+[.]?\\d*e[-|+]?\\d+") |>
          lapply(as.numeric) |>
          lapply(\(y) {
            if (length(y) == 0) return("")
            format(y, scientific = abs(log10(y)) >= 9)
          }) |>
          unlist()
        stringr::str_replace_all(.data$code, "\\d+[.]?\\d*e[-|+]?\\d+", num)
      },
      
      ## remove space if preceded by numeric and followed by alphabetical character
      code          = gsub("(?<=[0-9]) (?=[a-z|A-Z])", "", .data$code, perl = TRUE),
      ## rename all equivalents of 'day' to 'day'
      code          = gsub("(?<=^|[ :/*^])BDAY|dapu|dayef|dayg|dayl|dayl|daym|dayv|dbh|dge|dla|dpe|dpel|dpes|dpf|dpfg|dpfl|dpgm|dph|dph|dphv|dpm|dpmm|dpn|dpo|dpp|dpr|dpref|dps|dpu|dpw|dpys|dws|hd(?=$|[ :/*^])", "day", .data$code, perl = TRUE),
      ## Use 'unit' by default as arbitrary unit
      code          = gsub("^(|--|NR|NC)$", "unit", .data$code)
    )
  
  result <-
    x |>
    distinct() |>
    mutate(
      unit = {
        y <-
          lapply(
            .data$code,
            \(y) {
              tryCatch({
                units::mixed_units(1, y) |> fun()
              }, error = function(e) units::mixed_units(1, "unit"))
            }) |>
          fun()
        do.call(c, y)
      }
    )
  
  dplyr::left_join(x, result, "code") |>
    pull("unit")
}
