#' Collapse all arguments into unary closure
#'
#' @param f arbitrary function
#' @param ... additional arguments
#' @export
unify <- function(f, ...){
  function(x){
    f(x, ...)
  }
}

#' Get number of formal positional arguments
#'
#' @param f arbitrary function
#' @export
npositional <- function(f){
  formals(f)                  %>%
    lapply(deparse)           %>%
    lapply(nchar)             %>%
    unlist                    %>%
    { .[!names(.) == '...'] } %>%
    { sum(. == 0) }
}

#' Get number of formal arguments
#'
#' @param f arbitrary function
#' @export
nformals <- function(f){
  length(methods::formalArgs(f))
}

#' Run functions for their side effects
#' 
#' @param f a function or a list of functions
#' @param ... arguments sent to f
#' @export
runall <- function(f, ...){
  if(!all(class(f) == 'list')){
    f <- list(f)
  }
  if(all(sapply(f, is.function))){
    lapply(f, execute, ...)
  } else {
    stop("f must be a function or list of functions")
  }
}

#' Check the htype
#' 
#' @param CLASS expected class
#' @param ... any number of objects
#' @return TRUE if all elements in ... have class CLASS
#' @export
classcheck <- function(CLASS, ...){
  if(!is.character(CLASS)){
    stop("CLASS must be a character vector")
  }
  all(unlist(lapply(list(...), function(x) CLASS %in% class(x))))
}

#' Prepend a class to the class vector
#' 
#' @param x anything
#' @param ... classes to add
#' @return x with the new class prepended
#' @export
add_class <- function(x, ...){
  add_class_ <- function(x, cls) {
    if(!classcheck(cls, x)){
      class(x) <- c(cls, class(x))
    }
    x
  }
  Reduce(add_class_, unlist(list(...)), init=x)
}
