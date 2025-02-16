if (requireNamespace(c("DiagrammeR", "DiagrammeRsvg", "dplyr", "ECOTOXr"))) {
  library(ECOTOXr)
  library(DiagrammeR)
  library(DiagrammeRsvg)
  library(dplyr)
  
  db_specs <- ECOTOXr:::.db_specs
  tables <- db_specs$table |> unique()
  tables_wrap <- strsplit(tables, "_") |>
    lapply(\(x) {
      paste(
        c(
          paste(na.omit(x[1:2]), collapse = "_"),
          paste(na.omit(x[3:4]), collapse = "_")
        ),
        collapse = "_\n"
      )
    }) |>
    unlist()
  tables_wrap <- gsub("_\n$", "", tables_wrap)
  nodes <-
    data.frame(
      id = seq_along(tables),
      label = tables_wrap,
      table = tables,
      type = ifelse(endsWith(tables, "_codes"), "Lookup", "Data")
    )
  links <-
    db_specs$foreign_key[db_specs$foreign_key != ""] |>
    strsplit("[\\(\\)]")
  links <- as.data.frame(do.call(rbind, links))
  links <-
    links |>
    rename(target_table = 1, key = 2) |>
    mutate(source_table = db_specs$table[db_specs$foreign_key != ""]) |>
    distinct() |>
    mutate(from = match(source_table, tables), to = match(target_table, tables))
  
  links_to_check <- links[is.na(links$to),]
  if (nrow(links_to_check) > 0) stop("Database specs contain erronous links")
  links <- links[!is.na(links$to),]
  
  graph_dot <- sprintf(
"digraph {

graph [layout = 'fdp',
       sep='0',
       K='0.5',
       outputorder = 'edgesfirst',
       bgcolor = 'white']

node [fontname = 'Helvetica',
      fontsize = '10',
      shape = 'rectangle',
      fixedsize = 'true',
      width = '1.5in',
      height = '0.35in',
      style = 'filled',
      fillcolor = 'lightblue',
      color = 'gray70',
      fontcolor = 'black']

edge [fontname = 'Helvetica',
     fontsize = '8',
     len = '1.5',
     color = 'gray80',
     arrowsize = '0.5',
     spline = 'spline']

%s

%s
}",
sprintf("'%s' [label = '%s',%s class = 'schema_any schema_%02i eco_%s zoomTarget', URL='#ec_%s']",
        nodes$id, nodes$label,
        ifelse(nodes$type == "Data", " fillcolor = 'lightyellow',", ""),
        nodes$id, nodes$type,
        nodes$table) |>
  paste(collapse = "\n"),
sprintf("'%s'->'%s' [label = '%s', class = 'schema_any schema_%02i schema_%02i']",
        links$from, links$to, links$key, links$from, links$to) |>
  paste(collapse = "\n")
  )
  
  svgdat <-
    graph_dot |> grViz() |>
    DiagrammeRsvg::export_svg()
  
  svgdat <- gsub("<svg ", "<svg id=\"ecotoxschema\"", svgdat)
  svgdat <- gsub("0.00 0.00 1506.00 1014.00", "0.00 0.00 3012.00 2028.00", svgdat)
  svgdat <- gsub("<!--(.+?)-->\n", "", svgdat)
  svgdat <- gsub("<title>(.+?)</title>\n", "", svgdat)
  svgdat |>
    writeLines("man/figures/ecotox-schema.svg")
} else {
  stop("Install required packages first and try again")
}