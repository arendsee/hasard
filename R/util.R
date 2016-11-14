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

#' Get number of positional arguments
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

#' A wrapper for warning handling and reporting
#' 
#' @param ... arguments passed to sprintf
#' @export
warn <- function(...){
  warning(sprintf(...))
}

#' A wrapper for error handling and reporting
#' 
#' @param ... arguments passed to sprintf
#' @export
error <- function(...){
  msg <- force(sprintf(...))
  stop(msg)
}

#' Run functions for their side effects
#' 
#' @param x a function or a list of functions
#' @param ... arguments sent to x
#' @return NULL
#' @export
runall <- function(x, ...){
  if(is.list(x)){
    if(all(unlist(lapply(x, is.function)))){
      lapply(x, execute, ...)
    } else {
      error("All elements of x must be functions")
    }
  } else {
    x(...)
  }
  NULL
}

#' Check the htype
#' 
#' @param CLASS expected class
#' @param ... any number of objects
#' @return TRUE if all elements in ... have class CLASS
#' @export
classcheck <- function(CLASS, ...){
  if(!is.character(CLASS)){
    error("CLASS must be a character vector")
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

#' Check if two functions are composable
#' 
#' @param f,g unary class functions
#' @return logical
#' @export
are_composable <- function(f, g){
  classcheck('unary', f, g) && ((op(f) == ip(g)) || (ip(g) == "*"))
}

#' Lift a value into a context
#' 
#' @param value anything
#' @param state anything
#' @return the value inside a context
#' @export
vlift <- function(value, state=list(ok=TRUE)) {
  val <- list(value=value, state=state)
  val <- add_class(val, 'state_bound')
  val
}

#' Generic flift function: (a -> b) -> (F a -> F b)
#' 
#' @param f function of type (a -> b)
#' @param s function of type (F a -> b -> F b)
#' @return a state_bound datum
#' @export
#' 
flift <- function(f, s) {
  UseMethod("flift")
}

#' (a -> b) -> (F a -> F b)
#'
#' @param f function of type (a -> b)
#' @param s function of type (F a -> b -> F b)
#' @return a state_bound datum
#' @export
flift.state_bound <- function(f, s) {
  function(x) {
    val <- f(x$value)
    state <- s(x, val)
    vlift(value=val, state=state)
  }
}
