
#' Obtain a page over a series of dates using the Wayback Machine
#' @description Obtain a page over a series of dates using the Wayback Machine
#' @param myUrl URL of the page
#' @param start start date as ISOdatetime object
#' @param end end date as as ISOdatetime object
#' @param by argument passed to seq, defining the interval. Default "hour".
#' @param IntervalVector Optional vector of dates as a sequence with format "%Y%m%d%H%M%S"
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

waybackScrapper <- function(myUrl, start, end, by = "hour", intervalVector = NULL) {

  if(is.null(intervalVector)) {
  fecha <- format(seq(startDate,endDate, by= by), "%Y%m%d%H%M%S", "ART")
  dummy <- which(((1:length(fecha)) %% interval == 1))
  fecha <- fecha[dummy]
  } else  {
  fecha <- intervalVector
  }
  paste("https://web.archive.org/web/", fecha, "/http://", url[1], "/", sep = "")

  for (x in 1:length(myUrl)) {

    dir.create(myUrl[x])
    setwd(myUrl[x])
    buscador <- fun.search(myUrl[x], start, end)
    writeLines(buscador, "buscador.txt")
    for(i in 1:length(buscador)) {
      URL <- buscador[i]
      pp <- readLines(URL)
      write(pp, paste(myUrl[x], "_", i, ".html", sep = ""))
    }
    setwd("..")
  }

}
