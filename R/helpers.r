.add_tags <- function(x, sqlite) {
  if (missing(sqlite)) sqlite <- attributes(x)$database_file
  attributes(x)$date_created  <- Sys.Date()
  attributes(x)$created_with  <- sprintf("Package ECOTOXr v%s", utils::packageVersion("ECOTOXr"))
  attributes(x)$database_file <- sqlite
  return(x)
}

.fail_on_missing <- function(path = get_ecotox_path()) {
  test <- check_ecotox_availability(path)
  if (!test) {
    stop("No local database located. Download data first by calling 'download_ecotox_data()'")
  } else return(test)
}
