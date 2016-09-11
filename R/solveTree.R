
#' Find nodes and branches of an HTML tree in a nested set model representation.
#' @description Find nodes and branches of an HTML tree in a nested set model representation
#' @param a Representation of the tree as a linear vector nodes  numbers (nested set model)
#' @author Leandro Roser \email{leandroroser@@gmail.com}
#' @export


# https://en.wikipedia.org/wiki/Nested_set_model
solveTree <- function(a, recursive  = TRUE) {
  first <- a[1]
  lista <- as.list(rep(0, length(unique(a))))
  #names(lista) <- unique(a)

  i <- 1
  indice <- 1
  while(i < length(a)) {
    if(i > 1) {
      cumul <- a[1:(i-1)]
    } else  {
      cumul <- 0
    }
    if(a[i] %in% unique(cumul)) {
      i <- i + 1
      next
    }
    first <- a[i]
    names(lista)[indice] <- a[i]
    j <- i+1
    while(a[j] != first && i <= length(a)) {
      if(!(a[j] %in% cumul)) {
        cumul <- unique(c(cumul, a[j]))
        lista[[indice]] <- c(lista[[indice]], a[j])
      }
      j <- j + 1
    }
    if(a[i+1] %in% a[1:i]) {
      i <- i + 1
    }
    i <- i + 1
    indice <- indice  + 1
  }

  nombres <- unlist(sapply(1:length(lista), function(x)rep(names(lista)[x], length(lista[[x]]))))
  lista <- unlist(lista)
  names(lista) <- nombres
  lista <- lista[lista != 0]
  #remove nesting for > 1 level
  padrehijo <- data.frame(names(lista), lista, stringsAsFactors = FALSE)
  padrehijo[,1] <- as.numeric(padrehijo[, 1])
  padrehijo[,2] <- as.numeric(padrehijo[, 2])
  colnames(padrehijo)<-c("id", "son")
  out <- tapply(padrehijo[, 2], padrehijo[, 1], "c")
  if(!(recursive)) {
    for(i in 1:length(out)) {
      # remove grandsons
      out[[i]] <- out[i][[1]][!out[i][[1]] %in% unique(unlist(out[names(out) %in% out[i][[1]]]))]
    }
  }
  out
}


