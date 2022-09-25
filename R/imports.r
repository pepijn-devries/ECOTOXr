.onAttach <- function(libname, pkgname){
  packageStartupMessage({
    if (check_ecotox_availability()) {
      crayon::green("ECOTOX database file located, you are ready to go!\n")
    } else {
      crayon::red("ECOTOX database file not present! Invoke download and\ndatabase build using 'download_ecotox_data()'\n")
    }
  })
}

#' @importFrom RSQLite dbExecute dbConnect dbDisconnect dbWriteTable
#' @importFrom dplyr collect inner_join select sql tbl
#' @importFrom rlang :=
NULL

#' Objects exported from other packages
#' 
#' Objects imported and exported from other packages. See original documentation for more details.
#' 
#' \describe{
#' \item{dplyr}{\code{\link[dplyr:reexports]{\%>\%}}}
#' }
#' @importFrom dplyr %>%
#' @export %>%
#' @name %>%
#' @rdname reexports
NULL
