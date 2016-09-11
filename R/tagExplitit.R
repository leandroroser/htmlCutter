
#' Reconstruct an implicit HTML expression of the SQL database
#' @description reconstruct an implicit HTML expression of the SQL database
#' @param my_db Database.
#' @param tagId number of tag.
#' @param maxLevel Maximum level of nested tags converted into explicit expressions.
#' Default 1, for all the levels explicit use te value "ALL".
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

tagExplicit <- function(my_db, tagId, maxLevel = 1) {
  library(RSQLite)
  sqlite <- dbDriver("SQLite")
  my_db <- dbConnect(sqlite,my_db)
  #cat(" Tables:\n", dbListTables(my_db))
  #dbListFields(my_db, "clusters")
  #results <- dbSendQuery(my_db, "SELECT * FROM clusters WHERE tag == 'a'")
  #dbFetch(results,10)
  #dbClearResult(results)
  myQuery <- paste("SELECT content FROM contents WHERE id ==",  tagId)
  myChain <- dbSendQuery(my_db, myQuery)
  myChain <- dbFetch(myChain)
  myChain <- myChain[[1]]

  myQuery <- paste("SELECT id,son FROM fatherSon WHERE id =",  tagId)
  results <- dbSendQuery(my_db, myQuery)
  # se pone n = -1 para que devuelva todos los resultados!
  results <- dbFetch(results, n = -1)
  dbWriteTable(my_db, "temporal", results, row.names = FALSE, overwrite = TRUE)
  sustitucion <- results[, 2]

  level <- 1
  if(maxLevel > 1) {
    countdiff <- 1
    while(countdiff > 0 && level < maxLevel) {
      myQuery <- paste("SELECT fatherSon.id AS id, fatherSon.son AS son from fatherSon",
                       "JOIN (SELECT son FROM temporal) as b",
                       "ON  fatherSon.id = b.son;", sep = " ")
      results <- dbSendQuery(my_db, myQuery)
      results <- dbFetch(results)
      countdiff <- nrow(results)
      if(countdiff > 0) {
        sustitucion <- c(sustitucion, results[, 2])
        dbWriteTable(my_db, "temporal", results, row.names = FALSE, overwrite = TRUE)
      }
      if(maxLevel != "ALL") {
        level <- level  + 1
      }
    }
  }
  #borrar tabla temporal
  dbSendQuery(my_db, "DROP TABLE temporal")

  sustitucion <- unique(sustitucion)
  sustitucion <- paste0("(", paste(sustitucion, collapse =","), ")")
  myQuery <- paste("SELECT contents.id,contents.content FROM contents WHERE id IN", sustitucion)
  results <- dbSendQuery(my_db, myQuery)
  results <- dbFetch(results, n = -1)

  for(i in 1:nrow(results)) {
    pattern <- paste0("[{]<", results[i, 1], ">[}]")
    howMany <- gregexpr(pattern, myChain)
    if(length(howMany[[1]]) == 2) {
      doublePattern <- paste0(pattern, ".*?", pattern)
      myChain <- gsub(doublePattern, results[i, 2], myChain)
    } else  {
      myChain <- gsub(pattern, results[i, 2], myChain)
    }
    cat("\r", round(100*i/nrow(results), 2), "%")
  }

  # aca sigue asi:
  dbDisconnect(my_db)
  myChain
}
