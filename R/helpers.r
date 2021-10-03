.add_tags <- function(x, sqlite) {
  if (missing(sqlite)) sqlite <- attributes(x)$database_file
  attributes(x)$date_created <- Sys.Date()
  attributes(x)$created_with <- sprintf("Package ECOTOXr v%s", utils::packageVersion("ECOTOXr"))
  attributes(x)$database_file <- sqlite
  return(x)
}
