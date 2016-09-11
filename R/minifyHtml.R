
#' Minify an HTML file into a single string removing unnecesary spaces
#' @description Minify an HTML file into a single string removing unnecesary spaces
#' @param input HTML input file.
#' @param output Name for the output file.
#' @param rm.comments Remove comments?
#' @param wb.remove Remove Wayback Machine comments?
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

minifyHtml <- function(input,
                       output = "output.html",
                       rm.comments = FALSE,
                       wb.remove = TRUE) {

  my_file <- readLines(input)
  my_file <- as.list(my_file)
  my_file <- do.call("c", my_file)
  my_file <- paste0(my_file, collapse = "¬COLLAPSETAG¬")
  my_file <- gsub(">\\s+<", "><",my_file, perl = TRUE)
  my_file <-  gsub(">(\\s*¬COLLAPSETAG¬\\s*)+<", "><",my_file, perl = TRUE)
  my_file <- gsub("(^\\s+)|(\\s+$)", "", my_file, perl = TRUE)

  # funcion para remover espacios fuera de texto entrecomillado
  remove.space <- function(input) {

    #  input <- lapply(input, function(x) gsub("\\}[[:space:]]*", "}",x , perl=TRUE))
    #  input <- lapply(input, function(x) gsub("[[:space:]]*\\{", "{",x , perl=TRUE))
    #  input <- lapply(input, function(x) gsub("[[:space:]]*\\}", "}",x , perl=TRUE))
    #  input <- lapply(input, function(x) gsub("\\{[[:space:]]*", "{",x , perl=TRUE))
    #  input <- lapply(input, function(x) gsub("(;[[:space:]]*)|([[:space:]]*;)", ";",x , perl=TRUE))

    input <- lapply(input, function(input.value) subUnquoted(input.value, "([[:punct:]])([[:space:]]+)", "\\1")[[1]])
    input <- lapply(input, function(input.value) subUnquoted(input.value,"([[:space:]]+)([[:punct:]])", "\\2")[[1]])
    #input <- lapply(input, function(x) gsub("([[:punct:]])([[:space:]]*)", "\\1",x , perl=TRUE))
    #input <- lapply(input, function(x) gsub("([[:space:]]*)([[:punct:]])", "\\2",x , perl=TRUE))

    if(rm.comments) {
      input <- lapply(input, function(x) gsub("\\/\\*.*?\\*\\/", "",x , perl=TRUE))
    }

    input
  }


  css <- "<style.*?>.*?</style>"
  if(rm.comments) {
    contenido.css <- lapply(contenido.css, function(x) gsub("\\/\\*.*?\\*\\/", "",x , perl=TRUE))
  }
  donde.css <- gregexpr(css, my_file)
  contenido.css <- regmatches(my_file, donde.css)[[1]]
  #contenido.css <-  lapply(contenido.css, function(x)gsub("<style.*?>|</style>", "", x))
  contenido.css <- lapply(contenido.css, function(x) gsub("¬COLLAPSETAG¬|", "", x))
  contenido.css <- unlist(lapply(contenido.css, remove.space))
  regmatches(my_file, gregexpr(css, my_file)) <- list(contenido.css)

  #formateo scripts
  script <- "<script.*?>.*?</script>"
  donde.script <- gregexpr(script, my_file)

  # me guardo contenido de script
  # ESTO HAY QUE EDITARLO, llamar funcion format.script y concatenar con BREAK y los espacios que se necesiten antes
  contenido.script <- regmatches(my_file, donde.script)[[1]]
  #contenido.script <- gsub("<script>|</script>", "", contenido.script)
  contenido.script <- lapply(contenido.script, function(x) gsub("(\\/\\/)(.*?)(¬COLLAPSETAG¬)", "/*\\2*/", x))
  contenido.script <- lapply(contenido.script, function(x) gsub("¬COLLAPSETAG¬", "", x))
  contenido.script <- remove.space(contenido.script)
  regmatches(my_file, gregexpr(script, my_file)) <- list(contenido.script)
  # contenido.script <- lapply(contenido.script, function(x) gsub("¬COLLAPSETAG¬", "\n", x))[[1]]
  #  my_file <- gsub(script, "<script/>", my_file)

  # uglify
  # hay que ver como hacer para que no remova los comentarios aca
  #for(i in 1:length(contenido.script)) {
  #  archivo <- paste0("temporal_script_", i, ".js")
  #  archivo.output <- paste0("mod.", archivo)
  #  cat(contenido.script, file= archivo)
  #  args <- paste("uglifyjs", archivo,  "-c -m -o", archivo.output)
  #  system(args)
  #  contenido.script[i] <- ""
  #  contenido.script[i] <- readLines(archivo.output)
  #  file.remove(archivo, archivo.output)
  #}
  #----------------------------------
  #contenido.script <- paste0("<script>", contenido.script, "</script>")
  # sustituir
  #regmatches(my_file, gregexpr("<script/>", my_file)) <- list(contenido.script)


  my_file <- gsub("¬COLLAPSETAG¬", "", my_file)
  # si remuevo comentarios
  if(rm.comments) {
    my_file <- gsub("<!--.*?-->", "", my_file)
  }
  if(wb.remove) {
    my_file <- gsub("(<!-- BEGIN WAYBACK TOOLBAR INSERT -->)(.*)(<!-- END WAYBACK TOOLBAR INSERT -->)", "", my_file)
    my_file <- gsub("</html>.*$","</html>", my_file)
    }
  #cat(my_file, file = output)
  my_file
}
