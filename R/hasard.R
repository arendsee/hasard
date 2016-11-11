#' Get types for unary function
#' 
#' @param f an input function
#' @return vector of types
#' @export
ftype <- function(f){
  c(attributes(f)$.itype, attributes(f)$.otype)
}

#' Assign types to function
#' 
#' @param f left hand value
#' @param value right hand value
#' @export
#' 
`ftype<-` <- function(f, value){
  attributes(f)$.itype <- value[1]
  if (length(value) > 1) {
    attributes(f)$.otype <- value[2]
  } else { 
    attributes(f)$.otype <- value[1]
  }
  f
}

#' Check if two functions are composable
#' 
#' @param f,g unary class functions
#' @return logical
#' @export
are_composable <- function(f, g){
  (ftype(f)[2] == ftype(g)[1]) || ftype(g)[1] == "*"
}

#' Make composition of functions
#' 
#' @param ... two or more functions
#' @return A function that is a composition of the input functions
fcompose <- function(...){
  compose_ <- function(f, g) {
    composition_test(f, g)
    fun <- function(x) {
      g(f(x))
    }
    ftype(fun) <- c(ftype(f)[1], ftype(g)[2])
  }
  Reduce(compose_, list(...)) 
}

#' Split output of a function into downstream functions
#' 
#' @param f,g,h input functions
#' @return a list of two functions
#' @export
tee <- function(f, g, h) {
  function(x) {
    list(g(f(x)), h(f(x)))
  }
}

#' Wrap a pure function in a monad ...
#' 
#' @param fpure,fival,foval,fstat,feff some functions
#' @return wrapped function
#' @export
active_node <- function(fpure, fival, foval, fstat, feff){
  typecheck(fpure, 'unary')
  fun <- function(M) {
    a <- M$value
    b <- NULL
    if(!is.null(a) && fival(a)){
      b <- fpure(a)
      if(foval(b)){
        feff(M, b)
      } else {
        b <- NULL
      }
    }
    state <- fstat(M, b)
    vlift(value=b, state=state)
  }
  class(fun) <- c('active_node', 'function')
  attributes(fun)$.itype <- attributes(fpure)$.itype
  attributes(fun)$.otype <- attributes(fpure)$.otype
  fun
}

#' Contextualize an active function
#' 
#' @param f input active_node
#' @param inode,onode input and output nodes
#' @return a wired_node
#' @export
wired_node <- function(f, inode=NULL, onode=NULL){
  stopifnot(
    typecheck(f, 'active_node')      &&
    ltypecheck(inode, 'active_node') &&
    ltypecheck(onode, 'active_node')
  )
  class(f) <- c('wired_node', class(f))
  attributes(f)$inode <- inode
  attributes(f)$onode <- onode
  attributes(f)$cache <- NULL
  return(f)
}

#' Execute a node and cache result
#' 
#' @param f node to be executed
#' @export
run <- function(f) {
  typecheck(f, 'wired_node')
  ltypecheck(f$inode)
  ltypecheck(f$onode)
  # run using data from inode caches, if NULL, run them first, and cache
}

#' Reduce a function to a single input, single output function
#' 
#' @param f input function
#' @param itype,otype input and output types
#' @param ... constant arguments that will be written into the closure
#' @return a unary class function
#' @export
unify <- function(f, itype='*', otype='*', ...){
  fun <- function(x) {
    f(x, ...)
  }
  class(fun) <- c('unary', class(fun))
  attributes(fun)$.itype = itype
  attributes(fun)$.otype = otype
  fun
}
