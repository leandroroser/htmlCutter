#' Cut an HTML file into its constitutive elements, creating an SQLite database with tags, contents, attributes and values
#' @description
#' This program creates a series of SQLite tables dividing into pieces the content of an HTML file.
#' To find tags, the program uses a nested set model algorithm and regular expressions. Tags nested within a father tag
#' are substituted by the characters "\{<ID>\}", where ID is the number of the son tag.
#' In the default action, the substitution is not recursive, i.e., only one level of nesting is
#' allowed. This saves space in the database avoiding repeated elements.
#' A given tag can be recovered at its initial state with the function tagExplicit.
#' @param my_file Input HTML file.
#' @param sqlite.export Export into an sqlite database? Default TRUE.
#' The case FALSE returns the results as data.frames.
#' @param db.name Name of the output SQLite database.
#' @param single.tags Tags that must be considered as single (not closed).
#' @param overwrite Overwrite if the output database exists.
#' @param recursive Create a database showing all the nested elements within a tag
#' in an explicit fashion? Default FALSE (only one level of recursion is allowed).
#' @param minString Use substitution within a tag with the codes \{<ID>\}, where the ID's are the
#' name of the sons? Default TRUE.
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

htmlCut <- function(my_file, sqlite.export = TRUE,
                    db.name = "out.sqlite",
                    single.tags = c("\\!", "link", "META",
                                    "meta", "img", "input",
                                    "br", "base", "col",
                                    "command", "embed",
                                    "hr", "param", "source"),
                    overwrite = TRUE,
                    recursive = FALSE,
                    minString = TRUE) {

  require(RSQLite)
  cat("\nprocessing...\n\n")
  my_file <- minifyHtml(my_file)

  # Mask javascript
  masked <- regmatches(my_file, gregexpr("<script.*?>.*?</script>", my_file))[[1]]
  firstTag <- gsub("^(<script.*?>)(.*)(</script>)$", "\\1", masked)
  masked <- gsub("^(<script.*?>)(.*)(</script>)$", "\\2", masked)
  masked <- gsub(".", "x", masked)
  masked <- paste0(firstTag, masked, "</script>")
  temporal <- my_file
  regmatches(temporal, gregexpr("<script.*?>.*?</script>", temporal)) <- list(masked)
  # check
  #a <- regmatches(cuales.start[[1]], gregexpr("(?<=[<])[[:alnum:]]+(?=[\\s>])", cuales.start[[1]], perl=TRUE))
  #table(a)
  # no comments
  #start <- "<(?!--)(?!\\!DOCTYPE)(?![^/]*?>"
  noclosed.tags <- paste0("(?!", single.tags, ")", collapse = "")
  noclosed.tags <- paste0("(?!", "\\=", ")", noclosed.tags, collapse = "")
  noclosed.tags <- paste0("<", noclosed.tags, ">")
  start <- gsub(">$", "(?!\\/)[^<]+?>", noclosed.tags)
  start <- gregexpr(start, temporal, perl = TRUE)
  cuales.start <- regmatches(temporal, start)
  cuales.start <- cuales.start[[1]][1:length(cuales.start[[1]])]
  #etiquetas <- regmatches(cuales.start, gregexpr("(?<=[<])[[:alnum:]]+(?=[\\s>])", cuales.start, perl=TRUE))

  end <- "</.*?>"
  end <- gregexpr(end, temporal, perl = TRUE)
  # check
  cuales.end <- (regmatches(temporal, end))[[1]]

  #comentarios <- "<(\\!--).+?>"
  #comentarios <- gregexpr(comentarios, temporal)
  #cuales.comentarios <- (regmatches(temporal, comentarios))[[1]]

  # other tags
  otros.tags <- paste0("(", single.tags, ")|", collapse = "")
  otros.tags <- paste0("<[^\\d]?", "(", otros.tags, ")")
  otros.tags <- gsub("\\|\\)$", ")(.*?)>", otros.tags)
  # pongo a todos los otros tags e incluyo aca los scripts que los voy a procesar aparte
  otros.tags <- gregexpr(otros.tags, temporal)
  otros.tags.start <- otros.tags
  otros.tags.end <- otros.tags[[1]]+ attr(otros.tags[[1]], "match.length") -1
  # check
  cuales.otros <- regmatches(temporal, otros.tags)[[1]]

  a <- as.vector(start[[1]])
  b <- as.vector(end[[1]])
  posiciones <- c(rep(1, length(a)), rep(2, length(b)))
  names(posiciones) <- c(a,b)
  posiciones <- posiciones[order(as.numeric(names(posiciones)))]
  #posiciones <- integer(length(ab))
  # triada padre, nodo hijo, cierre
  triada <- list()
  anidamiento <- vector()
  arbol <- vector()
  j <- 1

  while(length(posiciones) >= 1) {
    i <- 2
    while(posiciones[i] == 1) {
      i <- i + 1
    }
    triada[[j]] <- c(names(posiciones[i-2]), names(posiciones[i-1]), names(posiciones[i]))
    posiciones <- posiciones[-c(i-1, i)]
    anidamiento[j] <- i - 1
    j <- j + 1
  }

  triada <- lapply(triada, as.numeric)
  #root
  triada[[j-1]] <- c(0, triada[[j-1]])
  triada <- do.call("rbind", triada)
  triada <- triada[nrow(triada):1, ]
  largo.start <- attr(start[[1]], "match.length")[pmatch(start[[1]], triada[, 2])]
  largo.end <- attr(end[[1]], "match.length")[pmatch(end[[1]], triada[, 3])]
  triada[, 3] <- triada[, 3]+ largo.end - 1

  triada.out <- cbind(triada, rep("DT", nrow(triada)))
  otros.out <- cbind(rep(0, length(otros.tags.start[[1]])), otros.tags.start[[1]], otros.tags.end, rep("ST", length(otros.tags.start[[1]])))
  #comentarios.out <- cbind(rep(0, length(comentarios.start[[1]])), comentarios.start[[1]], comentarios.end, rep("CT", length(comentarios.start[[1]])))

  #out <- rbind(triada.out, otros.out, comentarios.out)
  out <- rbind(triada.out, otros.out)
  colnames(out) <- c("father.node", "node.start", "node.end", "tag.class")
  out <- as.data.frame(out)

  #matriz de atributos
  atributos <- data.frame(c(start[[1]], otros.tags.start[[1]]),
                          c(attr(start[[1]], "match.length"),
                            attr(otros.tags.start[[1]], "match.length")))

  colnames(atributos) <- c("start", "end")
  atributos[, 2] <- atributos[, 1]+ atributos[, 2] -1
  atributos <- atributos[pmatch(atributos[, 1], out[, 2]), ]

  for(i in 1:3) {
    out[, i] <- as.numeric(as.character(out[, i]))
  }
  #remove negativ values
  for(i in 1:nrow(out)) {
    if(out[i, 2] < 0) {
      out <- out[-i, ]
      atributos <- atributos[-i, ]
    }
  }
  # values in ascendent order
  orden <- order(out[, 2])
  atributos <- atributos[orden, ]
  out.cluster <- out[orden, ]
  #out.cluster <- out[out[, 4] == "DT", ]
  out.cluster$id <- 1:nrow(out.cluster)
  nodo.cluster <- as.list(rep(0, nrow(out.cluster)))
  cl.length <- nrow(out.cluster)

  #-----------------------------------------------
  #-----------------------------------------------
  x <- integer(nchar(my_file))
  for(i in 1:nrow(out.cluster)) {
    x[(out.cluster$node.start)[i]] <- i
    x[(out.cluster$node.end)[i]] <- i
  }
  x <- x[x != 0]
  father <- solveTree(x, recursive = recursive)

  archivo <- (strsplit(my_file, ""))[[1]]

  # minify strings?
  if(minString) {
  #cadenas <- sapply(1:nrow(out.cluster), function(i) archivo[out.cluster$node.start[i]:out.cluster$node.end[i]])
  individuo <- which(out.cluster$id %in% as.numeric(names(father)))
  listaHP <- father[names(father) %in% largoP]
  cadenas <- character(length(out.cluster$id))
  cadenas <- as.list(cadenas)

  for(i in 1:length(cadenas)) {
    cadenas[[i]] <- archivo
  }
  tags.contenidos <- cadenas

  # substitution of sons into father nodes
  for(i in 1:length(individuo)) {
    padre <- individuo[i]
    hijo <-  listaHP[i]
    for(j in hijo[[1]]) {
      cadenas[[padre]][(out.cluster$node.start)[j]:(out.cluster$node.end[j])] <- paste0("{<", j, ">}")
    }
  }
  cadenas <- lapply(1:length(cadenas), function(i) cadenas[[i]] <<- cadenas[[i]][((out.cluster$node.start)[i]:(out.cluster$node.end)[i])])
  cadenas <- lapply(cadenas,  paste, collapse = "")

  # remove tag repetitions
  for(i in 1:length(individuo)) {
    padre <- individuo[i]
    hijo <-  listaHP[i]
    lapply(1:length(hijo[[1]]), function(j) {
        cadenas[[padre]] <<- gsub(paste0("([{]<", hijo[[1]][j], ">[}])+"), paste0("{<", hijo[[1]][j], ">}"), cadenas[[padre]])
      })
  }
  } else  {
    tags.contenidos <- list()
    lapply(1:nrow(out.cluster), function(i) {
      tags.contenidos[[i]] <<- paste(archivo[((out.cluster$node.start)[i]:(out.cluster$node.end)[i])], collapse = "")
    })
  }

  # create table
  names(cadenas)<-out.cluster$id
  cadenas <- sapply(cadenas, function(x)as.character(x))
  tags.contenidos <- data.frame(out.cluster$id, cadenas, stringsAsFactors = FALSE)
  colnames(tags.contenidos) <- c("id", "content")


  # reorder out.cluster
  padres <- out.cluster$id[pmatch(out.cluster$father.node, out.cluster$node.start, duplicates.ok = TRUE)]
  padres[is.na(padres)] <- 0
  out.cluster[, 1] <- padres
  st <- (out.cluster[out.cluster[, 4] == "ST", ])$id
  st<- st[st != 1]
  for(i in st) {
    up <- TRUE
    arriba <- i -1
    while(up == TRUE && arriba != 0) {
      if(out.cluster[arriba, 4] == "ST") {
        arriba <- arriba - 1
      } else {
        up <- FALSE
      }
    }
    if(out.cluster[i, 3] < out.cluster[arriba, 3]) {
      out.cluster[i, 1] <- (out.cluster$id)[arriba]
    }
  }
  out.cluster <- data.frame(out.cluster[, 5], out.cluster[, 1:4])
  names(out.cluster)[1]<-"id"
  for(i in 1:4) {
    out.cluster[, i] <- as.integer(out.cluster[, i])
  }


  ##
  first.tag <- ""
  names.tags <- atributos.list <- atributos.values <-  character()
  for(i in 1:nrow(atributos)) {
    first.tag <- paste0(archivo[atributos[i, 1]:atributos[i, 2]], collapse = "")
    names.tags[i] <- gsub("^(<)([\\S>]+)(.*>)", "\\2", first.tag, perl = TRUE)
    atributos.list[i] <- regmatches(first.tag, gregexpr("(?<=\\s)[\\S+]+?(?=[=])", first.tag, perl = TRUE))
    # este podia ser pero pued que no necesariamente tencan comillas...
    #atributos.values[i] <-  regmatches(first.tag, gregexpr("(\")(.*?)(\")", first.tag, perl = TRUE))
    atributos.values[i] <-  regmatches(first.tag, gregexpr("(?<=[=])[\\S]+(?=[\\s>])", first.tag, perl = TRUE))
    #atributos.values[i] <- gsub("^\"", "", atributos.values[i])
  }

# remove extra commas
atributos.values <- lapply(atributos.values, function(x) gsub("(^\\\")|(\\\"$)", "", x))

# table of attributes
#nombres <- as.numeric(gsub("(.*)([.])(.*)", "\\1", names(atributos.list)))
names(atributos.list) <- paste0(out.cluster$id, ".")
atributos.names <- do.call("c", atributos.list)
nombres <- names(atributos.names)
nombres <- gsub("(\\d+)(.*)", "\\1", names(atributos.names))
atributos.names <- data.frame(nombres, atributos.names)
atributos.values <- data.frame(nombres, do.call("c", atributos.values))
colnames(atributos.names) = c("id", "tag")
colnames(atributos.values) = c("id", "value")

# add tag column to out.cluster
out.cluster$tag <- names.tags

  if(sqlite.export) {
    if(overwrite == TRUE) {
      if(file.exists(db.name)) {
        file.remove(db.name)
      }
    }
    db <- dbConnect(SQLite(), dbname= db.name)
    dbWriteTable(db, "clusters", out.cluster, row.names = FALSE)
    dbWriteTable(db, "fatherSon", padrehijo, row.names = FALSE)
    dbWriteTable(db, "contents", tags.contenidos, row.names = FALSE)
    dbWriteTable(db, "tagNames", atributos.names, row.names = FALSE)
    dbWriteTable(db, "tagValues", atributos.values, row.names = FALSE)
    #dbSendQuery(conn = db,
    #            "CREATE TABLE tags
    #            (id INTEGER PRIMARY KEY AUTOINCREMENT,
    #             tagContent TEXT)"
    #            )
    #for(i in 1:nrow(out.cluster)) {
    #dbSendQuery(db, paste0("INSERT INTO tags (tagContent) VALUES",  "('", tags.contenidos[[i]], "'", ")"))
    #}
    cat("file written in:", db.name, "\n\n")
    cat("\ndone!\n\n")
  } else {
    cat("\ndone!\n\n")
    list(clusters = out.cluster, fatherSon = padrehijo, contents = tags.contenidos, tagNames = atributos.names, tagValues = atributos.variables)
  }
}

