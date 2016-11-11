#' Check the htype
#' 
#' @param x anything
#' @param type expected type
#' @return logical
#' @export
typecheck <- function(x, type){
  good = TRUE
  if(is.null(x)){
    good = FALSE 
  }
  if(! type %in% class(x)){
    good = FALSE
  }
  return(good)
}

#' Check the htype for a list
#' 
#' @param x a list
#' @param type expected type
#' @return logical
#' @export
ltypecheck <- function(x, type){
  good = TRUE
  if(is.null(x)){
    good = FALSE 
  }
  good = all(unlist(lapply(x, typecheck)))
  return(good)
}

#' Assert two functions can be composed
#' 
#' @param f,g two functions 
#' @return logical
#' @export
composition_test <- function(f, g){
  typecheck(f, 'unary')
  typecheck(g, 'unary')
  if( ! are_composable(f, g) ){
    stop("Composition g . f is illegal")
  }
}

#' Lift a value into a context
#' 
#' @param value anything
#' @param state anything
#' @return the value inside a context
#' @export
vlift <- function(value, state=list(ok=TRUE)) {
  val <- list(value=value, state=state)
  class(val) <- c('state_bound', class(val))
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

#' Create a new validator
#' 
#' @param f function with type: f :: vclass -> logical
#' @param vclass the input class
#' @return a unary, validator function
#' @export
make_validator <- function(f, vclass){
  fun <- function(x) {
    f(x)
  }
  class(fun) <- c('validator', 'unary', class(fun)) 
  attributes(fun)$itype <- vclass
  attributes(fun)$otype <- 'logical'
  fun
}

