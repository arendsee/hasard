#' @importFrom magrittr "%>%"
#' @include atom.R
#' @include is.R
#' @include util.R
#' @include assignment.R 
NULL

#' Build function
#'
#' @param f pure function
#' @param .itype,.otype input and output types
#' @param .inode,.onode input and output nodes
#' @param .val (a -> Bool) - determines in input is correct
#' @param .iwrap,.owrap input and output wrappers
#' @param .pass function called if val returns TRUE
#' @param .fail function called if val returns FALSE
#' @param .effect a function of a and b
#' @param .cacher the caching function
#' @param ... constant arguments that will be written into the closure
hnode <- function(
  f,
  .itype  = '*',
  .otype  = '*',
  .inode  = NULL,
  .onode  = NULL,
  .val    = true,
  .iwrap  = id,
  .owrap  = id,
  .pass   = execute,
  .fail   = nothing,
  .effect = nothing,
  .cacher = nocache,
  ...
){

  if(!(.itype == "*" || ip(.val) == .itype) || op(.val) != "Bool"){
    msg <- "expect val :: (%s -> Bool), got (%s -> %s)"
    warn(msg, .itype, ip(.val), op(.val))
  }

  fun <- function(
    x,
    f      = f,
    val    = .val,
    iwrap  = .iwrap,
    owrap  = .owrap,
    pass   = .pass,
    fail   = .fail,
    effect = .effect,
    cacher = .cacher,
    delete = FALSE,
    args   = list(...)
  ) {
    b = NULL
    if(delete){ cacher('del') }
    if(!cacher('chk')){
      a <- iwrap(x)
      if(val(a)){
        .b <- do.call(pass, append(list(f, a), args)) 
      } else {
        .b <- do.call(fail, append(list(f, a), args)) 
      }
      b <- owrap(tuple(A=x, b=.b))
      runall(effect, a=a, b=b)
      cacher('put', b)
    } else {
      b <- cacher('get')
    }
  }

  htype(fun) <- c(.itype, .otype)

  inode(fun) <- inode
  onode(fun) <- onode

  fun <- add_class(fun, 'hnode', 'unary')

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

#' Create a caching function
#'
#' @param fchk determines if a cached value exists
#' @param fdel deletes any cached value
#' @param fput caches the input
#' @param fget returns the cached value
#' @return a caching function
make_cacher <- function(fchk, fdel, fput, fget){
  function(op, ...){
    switch(
      op,
      chk = fchk(...),
      del = fdel(...),
      put = fput(...),
      get = fget(...)
    )
  }
}

#' Make composition of functions
#' 
#' @param ... two or more functions
#' @return A function that is a composition of the input functions
#' @export
compose <- function(...){
  compose_ <- function(f, g) {
    fun <- function(.) { g(f(.)) }
    if(classcheck('hnode', f, g)){
      if(! are_composable(f, g) ){
        msg <- "Illegal composition of (%s -> %s) and (%s -> %s)"
        error(msg, ip(f), op(f), ip(g), op(g))
      }
      htype(fun) <- c(ip(f), op(g))
      inode(fun) <- inode(f)
      onode(fun) <- onode(g)
      fun <- add_class(fun, 'hnode')
    }
    fun
  }
  Reduce(compose_, list(...)) 
}

#' A function that recusively determines class from an object
#' 
#' @param x any thing
#' @return character a class definition
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
