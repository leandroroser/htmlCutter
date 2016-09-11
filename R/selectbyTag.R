#' Filter the SQLite database by tag and attributes
#' @description Filter the SQLite database by tag and attributes
#' @param my_db Input SQLite database.
#' @param tag Tag used to filter the database.
#' @param attrList List of attributes of the selected tag, used to filter the database.
#' @param out_db Name of the output (filtered) database.
#' @param chunkSize Number of chunks used to write the output database.
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export


selectbyTag <- function(my_db, tag, attrList = NULL, out_db, chunkSize = 500) {

  sqlite <- dbDriver("SQLite")
  my_db <- dbConnect(sqlite,my_db)
  out_db <- dbConnect(SQLite(), dbname= out_db)

  clusters <- paste0("SELECT * FROM clusters WHERE id IN (SELECT id FROM clusters WHERE tag =",
                     paste0("'", tag, "'"),")")
  clusters <- dbSendQuery(my_db, clusters)

  while (!dbHasCompleted(clusters)) {

    dbWriteTable(out_db, "clusters", dbFetch(clusters, n = chunkSize) , row.names = FALSE, append = TRUE)

    contents <- paste0("SELECT * FROM contents WHERE id IN (SELECT id FROM clusters WHERE tag =",
                       paste0("'", tag, "'"),")")
    dbWriteTable(out_db, "contents", dbFetch(dbSendQuery(my_db, contents), n = chunkSize), row.names = FALSE,  append = TRUE)

    fatherSon <- paste0("SELECT * FROM fatherSon WHERE id IN (SELECT id FROM clusters  WHERE tag =",
                        paste0("'", tag, "'"),")")
    dbWriteTable(out_db, "fatherSon", dbFetch(dbSendQuery(my_db, fatherSon), n = chunkSize), row.names = FALSE,  append = TRUE)

    tagNames <- paste0("SELECT * FROM tagNames WHERE id IN (SELECT id FROM clusters  WHERE tag =",
                       paste0("'", tag, "'"),")")
    dbWriteTable(out_db, "tagNames", dbFetch(dbSendQuery(my_db, tagNames), n = chunkSize), row.names = FALSE,  append = TRUE)

    tagValues <- paste0("SELECT * FROM tagValues WHERE id IN (SELECT id FROM clusters  WHERE tag =",
                        paste0("'", tag, "'"),")")
    dbWriteTable(out_db, "tagValues", dbFetch(dbSendQuery(my_db, tagValues), n = chunkSize), row.names = FALSE,  append = TRUE)

  }

  # aca sigue asi:
  dbDisconnect(my_db)
  dbDisconnect(out_db)
  cat("\ndone\n\n!")
}
