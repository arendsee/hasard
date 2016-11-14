#' @include atom.R
#' @include is.R
#' @include util.R
NULL

#' Reduce a function to a single input, single output function
#' 
#' @param f input function
#' @param ... constant arguments that will be written into the closure
#' @return a unary class function
#' @export
monify <- function(f, ...){
  fun <- function(x) {
    f(x, ...)
  }
  fun <- add_class(fun, 'unary')
  fun
}

#' Set the type of a unary function
#' 
#' @param f input function
#' @param itype,otype input and output types
#' @return a typed f
#' @export
typify <- function(f, itype='a', otype='b'){
  htype(f) <- c(itype, otype)
  f
}

#' Add input type checking to a typed function
#'
#' @param f a typed, unary function
#' @param val (a -> Bool) - determines in input is correct
#' @param pass function called if val returns TRUE
#' @param fail function called if val returns FALSE
#' @return f
#' @export
valify <- function(f, val=true, pass=execute, fail=nothing){
  if(classcheck('typed', val, f)){
    v <- htype(val)
    a <- htype(f)
    if(v[1] != a[1] || v[2] != "Bool"){
      msg <- "expect val :: (%s -> Bool), got (%s -> %s)"
      warn(msg, a[1], v[1], v[2])
      return(f)
    }
  }

  fun <- function(.) {
    if(val(.)){
      pass(f, .) 
    } else {
      fail(f, .)
    }
  }
  class(fun) <- c('validated', class(f))

  return(fun)
}

#' Add a side-effect to the function call
#' 
#' @param f a unary function (a -> b)
#' @param effect a function of a and b
#' @return a closure that calls effect(a,b) without affecting f(a)
#' @export
effify <- function(f, effect=nothing){
  fun <- function(a){
    b <- f(a)
    effect(a, b)
    b
  }
  class(fun) <- c('effectual', class(f))
  fun
}

#' Contextualize an active function
#'
#' @param f input active_node
#' @param inode,onode input and output nodes
#' @return a wired_node
#' @export
conify <- function(f, inode=NULL, onode=NULL){
  attr(f, 'inode') <- inode
  attr(f, 'onode') <- onode
  f <- add_class(f, 'connected')
  f
}

#' Get output for a function
#' 
#' @param f any function
#' @param cacher the caching function
#' @return function ouptut
#' @export
memify <- function(f, cacher=execute){
  fun <- function(x){
    cacher(f, x)
  }
  fun <- add_class(fun, 'cached')
  fun
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
  fun <- add_class(fun, 'validator', 'unary')
  htype(fun) <- c(vclass, 'Bool')
  fun
}


#' Make composition of functions
#' 
#' @param ... two or more functions
#' @return A function that is a composition of the input functions
#' @export
compose <- function(...){
  if(classcheck('typed', ...)){
    compose_ <- function(f, g) {
      if(! are_composable(f, g) ){
        msg <- "Illegal composition of (%s -> %s) and (%s -> %s)"
        error(msg, htype(f)[1], htype(f)[2], htype(g)[1], htype(g)[2],)
      }
      fun <- function(x) {
        g(f(x))
      }
      htype(fun) <- c(htype(f)[1], htype(g)[2])
      if(!classcheck('validated', f)){
        fun$ival <- true
      } else {
        fun$ival <- f$ival
      }
      if(!classcheck('validated', g)){
        fun$oval <- true
      } else {
        fun$oval <- g$oval
      }
      class(fun) <- c("typed", "unary", "validated", "function")
      fun
    }
  } else {
    compose_ <- function(f, g) {
      function(.) { g(f(.)) }
    }
  }
  Reduce(compose_, list(...)) 
}

#' A function that recusively determines class from an object
#' 
#' @param . any thing
#' @return character a class definition
rclass <- compose(
  monify(lapply, function(x) ifelse(is.list(x), list_class(x), class(x)[1])),
  monify(unlist),
  monify(paste0, collapse=", "),
  monify(sub, pattern="(.*,.*)", replace="(\\1)")
)

#' Build a tuple (based on a list) from arguments
#' 
#' @param ... data to include in the output tuple
#' @return a list with 'tuple' class
#' @export
tuplify <- function(...) {
  tuple <- list(...)
  htype(tuple) <- rclass(tuple) 
  tuple <- add_class(tuple, 'tuple')
  tuple
}

#' Extract a subset from a tuple
#' 
#' @param x a tuple (or list)
#' @param i indices to extract from x
#' @return a subset of x
#' @export
parsubset <- function(x, i){
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

  itypes <- lapply(list(...), function(.) htype(.)[2])
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
