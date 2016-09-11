
#' Send an SQL query to SQLite database
#' @description Send an SQL query to SQLite database
#' @param myDb SQLite database
#' @param myQuery SQLite query
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

sendQuery <- function(myDb, myQuery) {
  sqlite <- dbDriver("SQLite")
  myDb <- dbConnect(sqlite, myDb)
  myResult <- dbSendQuery(myDb, myQuery)
  myResult <-fetch(myResult)
  dbDisconnect(my_db)
  myResult
}
