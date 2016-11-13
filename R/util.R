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

#' Get types for unary function
#' 
#' @param f an input function
#' @return vector of types
#' @export
htype <- function(f){
  attr(f, 'htype')
}

#' Assign types to function
#' 
#' @param f left hand value
#' @param value right hand value
#' @export
#' 
`htype<-` <- function(f, value){
  if(classcheck('unary', f) && length(value) > 2){
    error("2 types expected for unary function, %d found", length(value))
  }
  attr(f, 'htype') <- value
  f <- add_class(f, 'typed')
  f
}

#' The number of types a function has
#'
#' For a unary function, this will be two: input and output types. Currently,
#' this is the only legal kind of typed function. But eventually I may change
#' this.
#'
#' @param f a function with the 'typed' class
#' @return integer
#' @export
nhtypes <- function(f){
  length(htype(f))
}

#' Get input type
#' 
#' @param f a function with the 'typed' class
#' @return input type
#' @export
ip <- function(f) {
  if(classcheck('unary', f)){
    htype(f)[1]
  } else {
    warn("This function is only defined for unary functions (set in monify)")
    NULL
  }
}

#' Get output type
#' 
#' @param f a function with the 'typed' class
#' @return output type
#' @export
op <- function(f) {
  if(classcheck('unary', f)){
    htype(f)[2]
  } else {
    warn("This function is only defined for unary functions (set in monify)")
    NULL
  }
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
