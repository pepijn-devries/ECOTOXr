.onAttach <- function(libname, pkgname){
  packageStartupMessage({
    if (check_ecotox_availability()) {
      crayon::green("ECOTOX database file located, you are ready to go!\n")
    } else {
      crayon::red("ECOTOX database file not present! Invoke download and\ndatabase build using 'download_ecotox_data()'\n")
    }
  })
}

#' @importFrom dplyr collect left_join inner_join select sql tbl
#' @importFrom lifecycle badge
#' @importFrom rlang :=
#' @importFrom RSQLite dbExecute dbConnect dbDisconnect dbWriteTable
NULL

#' Objects exported from other packages
#' 
#' Objects imported and exported from other packages. See original documentation for more details.
#' 
#' \describe{
#' \item{dplyr}{[`\%>\%()`][dplyr::reexports]}
#' }
#' @importFrom dplyr %>%
#' @export %>%
#' @name %>%
#' @rdname reexports
NULL
