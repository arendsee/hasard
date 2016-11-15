#' A function that recusively determines type from an object
#' 
#' @param x any thing
#' @return character a class definition
#' @export
rclass <- function(x){
  lapply(x, function(.) ifelse(is.list(.), rclass(.), class(.)[1])) %>%
    unlist %>%
    paste0(collapse=", ") %>%
    sub(pattern="(.*,.*)", replacement="(\\1)")
}

#' Build a tuple (based on a list) from arguments
#' 
#' @param ... data to include in the output tuple
#' @return a list with 'tuple' class
#' @export
tuple <- function(...) {
  tuple <- list(...)
  htype(tuple) <- rclass(tuple) 
  tuple <- add_class(tuple, 'tuple')
  tuple
}

#' Flatten one level of a tuple
#' 
#' @param x tuple a possibly nested tuple
#' @return tuple
#' @export
flatten <- function(x){
  flattened <- list()
  for(. in x){
    if(classcheck('tuple')){
      for(val in .){
        flattened <- append(flattened, val)
      }
    } else {
      flattened <- append(flattened, .)
    }
  }
  flattened 
}

#' Build a function for subsetting a tuple 
#' 
#' @param x a tuple (or list)
#' @param i indices to extract from x
#' @return a function for subsetting a tuple
#' @export
tuplesubset <- function(x, i){
  if(! all(i %in% 1:length(x))){
    error("Invalid indices, select from 1-%s\n", length(x))
  }

  if(!classcheck('tuple', x)){
    error("x must be a tuple")
  }

  tuple <- x[i]

  htype(tuple) <- sprintf("(%s)", paste0(htype(x)[i], collapse=", "))

  tuple <- add_class(tuple, 'tuple')
  tuple
}
