#' Change text as a regex pattern, inside or outside quotes.
#' @description Change text as a regex pattern, inside or outside quotes
#' @param x Input text
#' @param From pattern to substitute
#' @param To substitution
#' @param invert If TRUE, changes are made over text within quotes.
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export

subUnquoted <- function(x, from, to, invert = TRUE, perl = TRUE) {
  pattern <- regmatches(x, gregexpr('["].*?["]', x, perl = perl), invert = invert)[[1]]
  #hay que poner regmatches, sino no funciona (no se puede usar pattern)
  regmatches(x, gregexpr('["].*?["]', x, perl = perl), invert = invert)[[1]] <- gsub(from,to, pattern)
  x
}
