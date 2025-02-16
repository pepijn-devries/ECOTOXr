if (check_ecotox_availability()) {
  library(dplyr)
  con <- dbConnectEcotox()
  random_results <-
    tbl(con, "results") |>
    slice_min(n = 20, order_by = runif(n())) |>
    pull("result_id")
  fields <- list_ecotox_fields("full")
  fields <- fields[!grepl("doi", fields)]
  random_results <-
    search_ecotox(
      list(result_id = list(terms = random_results, method = "exact")),
      fields)
  file <- gsub("\\.sqlite$", ".zip", get_ecotox_sqlite_file()) |> suppressWarnings()
  if (!file.exists(file)) stop("Need the original source (zip) to create test file")
  files       <- unzip(file, list = TRUE)
  files       <- files[grepl("*.?\\.txt$", files$Name),]
  threshold   <- 10*1024
  big_files   <- files[files$Length >  threshold,]
  small_files <- files[files$Length <= threshold,]
  nd1 <- file.path(tempdir(), "ecotox-test")
  if (!dir.exists(nd1)) dir.create(nd1)
  nd2 <- file.path(tempdir(), "ecotox-test", "validation")
  if (!dir.exists(nd2)) dir.create(nd2)
  unzip(file, files = small_files$Name, exdir = nd1)
  
  for (i in seq_len(nrow(big_files))) {
    print(sprintf("%02i %s", i, big_files$Name[i]))
    con_in  <- unz(file, big_files$Name[i])
    open(con_in)
    fo      <- file.path(nd1, big_files$Name[i])
    if (file.exists(fo)) unlink(fo)
    con_out <- file(fo, "w")
    header  <- readLines(con_in, 1L)
    field   <- strsplit(header, "[|]")[[1]][[1]]
    if (field == "cas_number")
      field2 <- c("chemical_carriers.cas_number", "tests.test_cas") else
      field2 <- paste(
        gsub(".txt$", "",
             gsub("^.*?/", "",
               big_files$Name[i])),
        field, sep = ".")
    if (field == "species_number") field2 <- "tests.species_number"
    
    sub_results <-
      search_ecotox(
        list(result_id = list(terms = random_results$result_id, method = "exact")),
        c("results.result_id", field2)) |>
      suppressWarnings() |>
      suppressMessages()

    writeLines(header, con_out)
    
    chunk_size <- 10000L
    
    repeat {
      lns     <- readLines(con_in, chunk_size) |> iconv(to = "UTF8", sub = "*")
      len     <- length(lns)
      reg     <- sprintf("^(%s)[|]", paste(unique(na.omit(unlist(
        sub_results[
          , ifelse(field == "result_id", 1, -1)
        ]))), collapse = "|"))
      if (length(reg) == 0) break
      lns     <- lns[
        grepl(reg, lns)
      ]
      writeLines(lns, con_out)
      if (len < chunk_size) break
    }
    
    close(con_in)
    close(con_out)
  }
  
  target_file <- file.path("inst", "ecotox-test.zip")
  if (file.exists(target_file)) unlink(target_file)
  wd <- getwd()
  setwd(nd1)
  zip(file.path(wd, target_file), "./")
  setwd(wd)
  unlink(nd1, recursive = TRUE)
} else {
  warning("Build a local copy of the ECOTOX database before you create the test files")
}