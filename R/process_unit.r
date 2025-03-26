.ut_sep0 <- "[ :/*^]"                      ## separator characters of unit fragment;
                                           ## essentially maths operators and spaces
.ut_sep1 <- sprintf("(?<=^|%s)", .ut_sep0) ## separator indicating start of unit fragment
.ut_sep2 <- sprintf("(?=$|%s)",  .ut_sep0) ## separator indicating end of unit fragment

## Commonly occurring annotations in units:

.ut_suffix <- 
  c("act", "AE", "ae", "arb", "ash", "ATP", "B-nap", "B-naph", "BAPNA", "bdwt", "blood",
    "BO", "bt", "BTEE", "BW", "C", "C2H4", "Ca", "CaO", "CaCO3", "caliper", "canopy", "cap",
    "CCT", "CEC", "change", "clay", "clitellate", "CNTL", "CO2", "co2", "CO3", "com", "corn",
    "cortex", "CREA", "d soil", "dev", "diet", "DNA", "DOPA", "dose", "dry", "DOPA", "DT",
    "earliness", "EDTA", "egg", "eu", "EU", "evolved", "FA", "FATL", "fertile", "fl", "fluid",
    "FM", "folium", "GAIN", "GH", "H2O", "H2O2", "HA", "Hb", "HCl", "HCO3", "Hg", "ht", "humus",
    "in", "ingested", "INHIB", "initial", "intake", "K[+]", "lipid", "LIT", "lit", "litter",
    "lf", "mat", "max", "MDA", "mdhyde", "media", "Mg", "min", "MIT", "MO", "N", "NaCl", "NH3",
    "NO2", "node", "O2", "OC", "of", "OH", "oil", "OM", "org", "P", "P[+]", "p450", "PBG", "PC",
    "phenolphth", "PLIPD", "plt", "pod", "pro", "prod", "PRTL", "RBC", "RBCE", "ret", "RI",
    "RNA", "ro", "root", "S", "sat", "sd", "sed", "seed", "soil", "soln", "solvent", "srtl",
    "SP", "sperm", "TE", "TEQ", "TI", "TIME", "tolerance", "total", "UA", "urea", "WBC", "WDTH",
    "wet", "wght", "WGHT", "WSF", "wt", "yld", "Zn")

## Equivalents for 'durations':

.ut_second   <-
  c("s", "sec", "spf", "spref")

.ut_minute   <-
  c("mi", "mpf", "mph")

.ut_hour   <-
  c("h", "hbe", "hbf", "hbh", "hpe", "hpel", "hpf", "hph", "hpp", "hpr", "hps")

.ut_day   <-
  c("BDAY", "dapu", "dayef", "dayg", "dayl", "daym", "dayv", "dbh", "dge", "dla", "dpe",
    "dpel", "dpes", "dpf", "dpfg", "dpfl", "dpgm", "dph", "dph", "dphv", "dpm", "dpmm",
    "dpn", "dpo", "dpp", "dpr", "dpref", "dps", "dpu", "dpw", "dpys", "dws", "hd")

.ut_week   <-
  c("wk", "wks", "wkpm", "wkprf", "wpe", "wph", "wphv", "wpp", "wps", "wpv3", "wpv5")

.ut_month   <-
  c("mo", "mope", "mopf", "moph", "mopm", "mopres", "mopswm", "mpgm", "mpp")

.ut_year   <-
  c("yr", "yc", "yph")

## Unit fragments representing counts
## Note that cc only stands for 'cubic centimetres' in case of 'cc/L'.
## In all other cases it is 'cocoons'.
.ut_counts <- c("ad", "(beat(s?))", "(bee(s?))", "(branch((es)?))", "bt", "bud",
                "(burrow(s?))", "cc", "(cel(l?)(s?))", "(cast((ing)?)(s?))", "chem",
                "(cluster(s?))", "(clutch((es)?))", "cntr", "(cyc((le)?)(s?))", "dead",
                "(egg(s?))", "(em((bryo)?)(s?))", "(failure(s?))", "(fet((us)?)((es)?))",
                "(fibre(s?))", "(field(s?))", "FM", "gland", "jv", "live", "ML",
                "(neuron(s?))", "no", "nuclei", "org", "(pair(s?))", "panicle",
                "(pl((ot)?(s?)))", "(pot(s?))", "preg", "raft", "(seed(s?))", "sad",
                "section", "(trail(s?))", "tubule")

## Straight forward replacements to make units more explicit
.ut_explicit <-
  data.frame(
    pattern     = c("lbs", "Nm", "ac",   "0/00", "wk(s?)", "u(g?)-atoms",
                    "pph(r?)", "[*]dyn", "gTI", "ga",  "cc/L",  "mi", "mo", "deg",
                    "sqft", "PH",           "pm",          "fs",             "mgL"),
    replacement = c("lb",  "nm", "acre", "ppt",  "week",   "ug",
                    "10ppt",   "dyne",   "g",   "gal", "cm3/L", "min", "month", "degree",
                    "ft2",  "pithardening", "postmolting", "floweringstage", "mg*L")
  )

## Function to replace unit fragments in text

.replace_ut_frag <- function(pattern, replacement, x) {
  gsub(paste0(.ut_sep1, pattern, .ut_sep2), replacement, x, perl = TRUE)
}

## Function to convert database field name to unit type

.field_to_unit_type <- function(field) {
  type <- .db_specs$foreign_key[endsWith(field, .db_specs$field_name)]
  type <- gsub("[_].*", "", type) |> unique()
  if (length(type) == 0) return("unknown")
  if (type == "sample") type <- "size"
  type
}

#' Process ECOTOX search results by converting `character` to units where relevant
#' 
#' `r lifecycle::badge('experimental')` The function `search_ecotox()` returns fields
#' from the ECOTOX database as is. Fields that represent units are not standardised in
#' the database. Therefore, this format is not consistently used throughout the
#' database. `process_ecotox_units()` takes a `data.frame` returned by
#' `search_ecotox()`, locates unit columns, represented by text, sanitises the text
#' and converts them to `units::mixed_units()` objects. It will sanitise the unit fields as
#' much as possible. Units that could not be interpreted are returned as arbitrary `unit`.
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
  unit_columns <- c(.db_specs$field_name[grepl("_unit$", .db_specs$field_name)], "_unit")
  patt <- sprintf("^.*?(%s)$",
                  paste(unit_columns, collapse = "|"))
  
  x |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches(patt, perl = TRUE),
        .fns  = ~ {
          if ("type" %in% names(formals(.fns))) {
            .fns(.x, type = .field_to_unit_type(dplyr::cur_column()), ...)
          } else {
            .fns(.x, ...)
          }
        },
        .names = .names
      )
    )
}

#' Text from the ECOTOX database to `mixed_units`
#' 
#' `r lifecycle::badge('experimental')` Convert text to units after
#' sanitising.
#' 
#' The following steps are performed (in the order as listed)
#' to sanitise text before coercing it to units:
#' 
#'  * The following is removed:
#'    * Leading/trailing white spaces
#'    * Square brackets and commas
#'    * A list of common prefixes
#'    * Double spaces are replaced by single spaces
#'    * Brackets around multiply symbol
#'  * The following is corrected/adjusted:
#'    * 'for' is interpreted as multiplication
#'    * Scientific notation of numbers is
#'      standardised where possible.
#'    * A list of ambiguous patterns is replaced with
#'      more explicit strings. For instance,
#'      'deg' is replaced with 'degree'.
#'  * The following miscellaneous corrections are made:
#'    * A list of 'known' annotations are removed from the units
#'    * A list of elements kown to represent counts are renamed
#'      'counts'.
#'    * Percentages are renamed as explicit concentration in
#'      mass per volume or volume per volume units where possible
#'    * 'CI' is renamed 'Curies'.
#'    * 'M' is renamed 'mol/L'.
#'    * Units expressed as 'parts per ...' are explicitly renamed
#'      to mass over volume, or volum over volume where possible
#'  * Type specific sanitation steps
#'    * Concentration units:
#'      * 'K' is renamed 'Karmen'
#'      * 'dpm' is renamed 'counts/min' (i.e., disintegrations per minute)
#'    * Media units:
#'      * 'K' is renamed 'Kelvin'
#'      * 'C' is renamed 'Celsius'
#'  * Some final miscellaneous adjustments:
#'    * Scientific notation in numbers is not supported by the units package.
#'      Numbers are formatted in decimal notation where possible.
#'    * Spaces are removed if preceded by numeric and followed by
#'      alphabetical character
#'    * All equivalents of ambiguous synonyms for time units are explicitly
#'      renamed to their respective unit (e.g., 'dph' (days post hatching) -> 'day')
#'    * unreported/missing units are renamed 'unit'
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
#' @param ... Ignored.
#' @param warn If set to `FALSE` warnings while converting text to units are suppressed.
#' @returns A vector of `?units::unit` class objects with the same length as `x`.
#' @author Pepijn de Vries
#' @examples
#' ## Try parsing a random set of units from the database:
#' c("ppm-d", "ml/2.5 cm eu", "fl oz/10 gal/1k sqft", "kg/100 L",
#'   "mopm", "ng/kg", "ug", "AI ng/g", "PH", "pm", "uM/cm3", "1e-4 mM",
#'   "degree", "fs", "mg/TI", "RR", "ug/g org/d", "1e+4 IU/TI", "pg/mg TE",
#'   "pmol/mg", "1e-9/l", "no >15 cm", "umol/mg pro", "cc/org/wk", "PIg/L",
#'   "ug/100 ul/org", "ae mg/kg diet/d", "umol/mg/h", "cmol/kg d soil",
#'   "ug/L diet", "kg/100 kg sd", "1e+6 cells", "ul diet", "S", "mmol/h/g TI",
#'   "g/70 d", "vg", "ng/200 mg diet", "uS/cm2", "AI ml/ha", "AI pt/acre",
#'   "mg P/h/g TI", "no/m", "kg/ton sd", "ug/g wet wt", "AI mg/2 L diet",
#'   "nmol/TI", "umol/g wet wt", "PSU", "Wijs number") |>
#'   as_unit_ecotox(warn = FALSE)
#' 
#' ## Adding the type of measurement can affect interpretation:
#' as_unit_ecotox(c("C", "K"), type = "concentration")
#' as_unit_ecotox(c("C", "K"), type = "media")
#' @family ecotox-sanitisers
#' @export
as_unit_ecotox <- function(
    x,
    type = c("concentration", "duration", "length", "media", "application", "size", "weight", "unknown"),
    ..., warn = TRUE) {
  if (inherits(x, "mixed_units")) return(x)
  if (typeof(x) != "character") stop(
    paste("`as_unit_ecotox` should only convert `characters`.",
          "I got", typeof(x), "instead."))
  
  type <- rlang::arg_match(type)
  # Declare variables to pass CRAN checks
  .data <- NULL
  
  fun <- if (warn) \(x) x else suppressWarnings
  
  x <-
    dplyr::tibble(code = x) |>
    dplyr::mutate(
      ## trim leading and trailing white spaces
      code          = trimws(.data$code),
      
      ## remove square brackets and commas
      code          = gsub("\\[|\\]|,", "", .data$code),
      
      ## remove known prefixes
      code          = gsub("(?<=^|[ ])(ae|AE|ai|AI|fl|litter) ", "",
                           .data$code, perl = TRUE),
      
      ## double spaces to single spaces
      code          = gsub("  ", " ", .data$code),
      
      ## remove spaces around multiply symbol
      code          = gsub(" ?[*] ?", "*", .data$code),

      ## interpret 'for' as multiplication
      code          = gsub(" for ", "*", .data$code),
      
      ## minus preceded by alphabetical (not 'e') and followed by numeric
      ## should be an exponent
      code          = gsub("(?<=[a-d|f-z|A-D|F-Z])-(?=[0-9])", "^-", .data$code, perl = TRUE),
      
      ## standardise scientific notation in numeric component
      code          = gsub(" ?x10x(?=[0-9])", "1e", .data$code, perl = TRUE),
      code          = gsub("10[xX]", "1e", .data$code),
      
      ## numeric followed by 'k' is a thousandfold
      code          = gsub(sprintf("(?<=[0-9])k%s", .ut_sep2), "000", .data$code, perl = TRUE),
      
      ## replace ambiguous patterns with more explicit strings
      code          = {
        result <- .data$code
        for (i in seq_len(nrow(.ut_explicit))) {
          result <- .replace_ut_frag(
            .ut_explicit$pattern[[i]],
            .ut_explicit$replacement[[i]],
            result)
        }
        result
      },

      ## remove various annotations (space followed by one of 'known' annotations)
      code          = gsub(sprintf(" (%s)%s", paste(.ut_suffix, collapse = "|"), .ut_sep2), "",
                           .data$code, perl = TRUE),
      
      ## make counts explicit
      code          = .replace_ut_frag(sprintf("%s", paste(.ut_counts, collapse = "|")),
                                       "counts", .data$code),
      
      ## percentage preceded by decade of grams is in the mass unit per decilitre
      ## which is more specific
      code          = gsub("g([ ]?)%( w/v)?", "g/dL", .data$code),
      
      ## percentage concentration mass over volume is more explicitly
      ## expressed as hectogram per millilitre
      code          = gsub("^%([ ]?)(m|w)(:|/)v$", "g/dL", .data$code),
      
      ## percentage concentration volume over volume is more explicitly
      ## expressed as litre per hectolitre
      code          = gsub("^%([ ]?)v(:|/)v$", "L/hL", .data$code),
      
      ## percentage concentration mass over mass is more explicitly
      ## expressed as grams per hectograms
      code          = gsub("^%([ ]?)(m|w)(:|/)(m|w)$", "g/hg", .data$code),
      
      ## percentage concentration volume over mass is more explicitly
      ## expressed as grams per decalitre
      code          = gsub("^%([ ]?)v(:|/)(m|w)$", "g/dL", .data$code),

      ## CI, mCI and uCI are Curies (milli and micro)
      code          = gsub(sprintf("(?<=^(u|m|c)|%s(u|m|c))CI%s", .ut_sep0, .ut_sep2),
                           "Curies", .data$code, perl = TRUE),
      
      ## mM, uM, M is mmol/L, umol/L and mol/L respectively
      code          = gsub(sprintf("(?<=^(u|m)|%s(u|m|c))M%s", .ut_sep0, .ut_sep2),
                      "mol/L", .data$code, perl = TRUE),
      code          = .replace_ut_frag("M", "mol/L", .data$code),
      
      ## in 'mol+', the '+' is just an annotation of positive ions
      code          = gsub("mol+", "mol", .data$code, fixed = TRUE),

      ## Da (Dalton) is same as unified atomic mass (u)
      code          = gsub(sprintf("((?<=k)?)Da%s", .ut_sep2), "u", .data$code, perl = TRUE),
      
      ## mho is same as Siemens
      code          = gsub("(mho(s?))", "Siemens", .data$code, perl = TRUE),
      
      ## 'parts per ...' followed by minus, should be product (i.e, ppt-h should be ppt*h)
      code          = gsub("(?<=pp(h|m|t))-", "*", .data$code, perl = TRUE),
      ## make ppt and ppm more explicit where possible
      ## ppmw/soil vol === ppm w/v === ppmv
      code          = gsub("(ppmw/soil vol)|(ppm w/v)", "mg/L", .data$code),
      code          = gsub("ppt v/v", "mL/L", .data$code),
      code          = gsub("ppt w/v", "g/L", .data$code),
      ## ppmw === ppm w/w === ppm dw
      code          = gsub("(ppm dw)|(ppm w/w)|(ppmw)", "ug/g", .data$code),
      code          = gsub("(ppt w/w)|(pptw)", "mg/g", .data$code),

      ## 'type' specific sanitation steps
      code          = if (type == "concentration") {
        
        result <- .data$code
        ## in case of type 'concentration' 'K' stands for Karmen units
        result <- .replace_ut_frag("K", "Karmen", result)
        ## in case of concentration dpm is disintegrations (counts) per minute
        result <- .replace_ut_frag("(c|d)pm", "counts/min", result)
        result
        
      } else          if (type == "media") {
        
        result <- .data$code
        ## in case of type 'media' 'C' stands for Celsius  
        result <- .replace_ut_frag("C", "Celsius", result)
        ## in case of type 'media' 'K' stands for Kelvin  
        result <- .replace_ut_frag("K", "Kelvin", result)
        result
        
      } else .data$code,
      
      code          = {
        ## scientific notation is not always parsed correctly by units package.
        ## In fact it's not even supported:
        ## https://github.com/r-quantities/units/issues/383#issuecomment-2703308481
        ## Therefore, the scientific notation is replaced by decimal notation where
        ## possible (abs(log10(number)) < 9)
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
      
      ## rename all equivalents of 'seconds' to 'second'
      code          = gsub(
        sprintf("%s(%s)%s", .ut_sep1, paste(.ut_second, collapse = "|"), .ut_sep2), "second",
        .data$code, perl = TRUE),

      ## rotations per minute
      code          = .replace_ut_frag("rpm", "counts/min", .data$code),
      
      ## rename all equivalents of 'minutes' to 'minute'
      code          = gsub(
        sprintf("%s(%s)%s", .ut_sep1, paste(.ut_minute, collapse = "|"), .ut_sep2), "minute",
        .data$code, perl = TRUE),

      ## rename all equivalents of 'hours' to 'hour'
      code          = gsub(
        sprintf("%s(%s)%s", .ut_sep1, paste(.ut_hour, collapse = "|"), .ut_sep2), "hour",
        .data$code, perl = TRUE),
      
      ## rename all equivalents of 'day' to 'day'
      code          = gsub(
        sprintf("%s(%s)%s", .ut_sep1, paste(.ut_day, collapse = "|"), .ut_sep2), "day",
        .data$code, perl = TRUE),
      
      ## rename all equivalents of 'weeks' to 'week'
      code          = gsub(
        sprintf("%s(%s)%s", .ut_sep1, paste(.ut_week, collapse = "|"), .ut_sep2), "week",
        .data$code, perl = TRUE),
      
      ## rename all equivalents of 'months' to 'month'
      code          = gsub(
        sprintf("%s(%s)%s", .ut_sep1, paste(.ut_month, collapse = "|"), .ut_sep2), "month",
        .data$code, perl = TRUE),
      
      ## rename all equivalents of 'years' to 'year'
      code          = gsub(
        sprintf("%s(%s)%s", .ut_sep1, paste(.ut_year, collapse = "|"), .ut_sep2), "year",
        .data$code, perl = TRUE),
      
      ## Use 'unit' by default as arbitrary unit
      code          = gsub("^(|--|NR|NC)$", "unit", .data$code)
    )
  
  result <-
    x |>
    dplyr::distinct() |>
    dplyr::mutate(
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
    dplyr::pull("unit")
}

#' Convert mixed units to a specific unit
#' 
#' Converts a list of mixed units to a specific unit, using the `units`
#' package.
#' @param x A mixed units object (`units::mixed_units()`) to be converted
#' to the `target_unit`
#' @param target_unit A `character` string representing the target
#' unit
#' @returns Returns a units object (`?units::units`). Values with units
#' that cannot be converted to the `target_unit` is returned as `NA`.
#' @examples
#' mishmash <- as_unit_ecotox(c("mg/L", "ppt w/v", "% w/v", "mmol/L"))
#' 
#' ## Note that 'mmol/L' cannot be converted to 'ug/L'
#' ## without a molar mass. It is returned as `NA`
#' mixed_to_single_unit(mishmash, "ug/L")
#' 
#' mishmash <- as_unit_ecotox(c("h", "sec", "mi", "dph"))
#' 
#' mixed_to_single_unit(mishmash, "h")
#' @author Pepijn de Vries
#' @family ecotox-sanitisers
#' @export
mixed_to_single_unit <- function(x, target_unit) {
  result <-
    lapply(x, \(y)
           tryCatch(units::set_units(units::mixed_units(y), target_unit)[[1]],
                    error = function(e) units::as_units(NA, target_unit))
    )
  do.call(c, result)
}