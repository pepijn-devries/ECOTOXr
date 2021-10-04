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
NULL
