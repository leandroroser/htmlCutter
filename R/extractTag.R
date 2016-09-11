#' Remove a tag from an HTML file
#' @description Remove a tag from an HTML file
#' @param filecon File connection
#' @param tag Tag name
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export


extractTag <- function(filecon, tag) {
  my_file <- readLines(filecon)
  my_file <- as.list(my_file)
  my_file <- do.call("c", my_file)
  my_file <- paste0(my_file, collapse = "TEXTOCOLAPSADO")
  #start <- paste("<", tag, ">|(<",tag, ")([^<]*)(>)", sep =  "")
  #  The above line work as well. The line below use lazy search (non-greedy), i.e., the search
  # stops when the first > is reached.
  start <- paste("(<",tag, ")(.*?>)", sep =  "")
  end <- paste("</", tag, ">", sep =  "")
  start <- gregexpr(start, my_file)
  start <- start[[1]]
  end <- gregexpr(end, my_file)
  end <- end[[1]] + nchar(tag) + 2

  # VER SI ACA CONVIENE USAR REGMATCHES
  out.tag <- ""
  for(i in 1:length(start)) {
    out.tag <- paste0(out.tag, substr(my_file, start[i], end[i]), sep = "TEXTOCOLAPSADO")
  }
  out.tag <- strsplit(out.tag, "TEXTOCOLAPSADO")
  #write(bjs[[1]], output)
  #cat("\n!done\n")
  out.tag[[1]]
}
