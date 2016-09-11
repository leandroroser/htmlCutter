#' Remove Wayback Machine content included in a page
#' @description Remove Wayback Machine content included in a page
#' @param my_file HTML file to edit.
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

waybackRemove <- function(my_file) {
a <- gsub("(<!-- BEGIN WAYBACK TOOLBAR INSERT -->)(.*)(<!-- END WAYBACK TOOLBAR INSERT -->)", "", my_file)
gsub("</html>.*$","</html>", a)
}

#write(wayback_remove(minifyHtml("LN_10.html")), "limpio.html")
