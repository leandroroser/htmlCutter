
#' Newick representation of an HTML tree
#' @description Newick representation of a tree
#' @param a Representation of the tree as a linear vector with nodes numbers (nested set model).
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

parNesting <- function(a) {
i <- 1
first <- a[1]
largo <- length(a)
while(i < (largo-1)) {
  if(a[i+1] %in% first) {
    x <- paste0(x, a[i+1], ")")
  } else {
    first <- c(first,  a[i+ 1])
    x <- paste0(x, "(", a[i+1])
  }
  i <- i+1
}
x <- paste0(x, a[largo], ")")
x
}

