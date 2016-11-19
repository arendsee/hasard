#' Check if two functions are composable
#' 
#' @param f,g unary class functions
#' @return logical
#' @export
are_composable <- function(f, g){
  classcheck('unary', f, g) && ((op(f) == ip(g)) || (ip(g) == "*"))
}

#' Make composition of functions
#' 
#' @param ... two or more functions
#' @return A function that is a composition of the input functions
#' @export
compose <- function(...){
  compose_ <- function(f, g) {
    fun <- function(., ...) { g(f(., ...)) }
    if(classcheck('hnode', f, g)){
      if(! are_composable(f, g) ){
        msg <- "Illegal composition of (%s -> %s) and (%s -> %s)"
        error(msg, ip(f), op(f), ip(g), op(g))
      }
      htype(fun) <- c(ip(f), op(g))
      inode(fun) <- inode(f)
      fun <- add_class(fun, 'hnode')
    }
    fun
  }
  Reduce(compose_, list(...)) 
}

#' Combine takes any number of arguments, but yields a function with a constant
#' number of arguments.
#' 
#' @param ... input functions from which types are induced
#' @return combining function
#' @export
combine <- function(...){
  if(!classcheck('unary', ...) || !classcheck('typed', ...)){
    error("Can only combine unary, typed functions")
  }

  itypes <- lapply(list(...), op)
  otype <- sprintf("(%s)", paste0(itypes, collapse=", "))

  N <- length(list(...))

  fun <- function(...){
    if(length(list(...)) != N){
      error("Expected list of length %d", N)
    }
  }

  htype(fun) <- c(itypes, otype)
  class(fun) <- c('typed', 'function')
  fun
}
