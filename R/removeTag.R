#' Remove a tag from an HTML file
#' @description Remove a tag from an HTML file
#' @param filecon HTML input file.
#' @param tag Name of the tag.
#' @param single Is this a single (not-closed) tag?
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

removeTag <- function(filecon, tag, single = FALSE) {
  my_file <- readLines(filecon)
  my_file <- as.list(my_file)
  my_file <- do.call("c", my_file)
  my_file <- paste0(my_file, collapse = "TEXTOCOLAPSADO")
  start <- paste("(<",tag, ")(.*?>)", sep =  "")
  end <- paste("</", tag, ">", sep =  "")
  start <- gregexpr(start, my_file)
  start <- start[[1]]
  end <- gregexpr(end, my_file)
  end <- end[[1]] + nchar(tag) + 2
  intervalos <- data.frame(start, end)
  longitud <- 1:nchar(my_file)
  for(i in 1:nrow(intervalos)) {
    longitud[intervalos[i, 1]:intervalos[i, 2]] <- "CHAU"
  }

  longitud2 <- paste(longitud, collapse = " ")
  longitud2 <- gsub("(CHAU\ )+", "ADIOS ", longitud2)
  longitud2 <- strsplit(longitud2, "ADIOS ")
  longitud2 <- lapply(1:length(longitud2[[1]]), function(i) strsplit(longitud2[[1]][i], " "))
  longitud2 <- lapply(1:length(longitud2), function(x)as.numeric(longitud2[[x]][[1]]))
  maximo <- lapply(longitud2, max)
  minimo <- lapply(longitud2, min)
  end <- do.call("c", maximo)
  start <- do.call("c", minimo)

  cuanto <- length(start)

  pat <- lapply(1:cuanto, function(i) substr(my_file, start[i], end[i]))

  pat <- paste0(pat, collapse = " ")

  strsplit(pat, "TEXTOCOLAPSADO")->salida
  #write(salida[[1]], "cuerpo_sin_script.txt")
  salida[[1]]
}
