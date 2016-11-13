#' @include util.R
NULL

#' A function that always returns TRUE
#' 
#' @param ... anything
#' @return TRUE
#' @export
true <- function(...) {
  TRUE
}

#' A function that always returns FALSE
#' 
#' @param ... anything
#' @return FALSE
#' @export
false <- function(...) {
  FALSE
}

#' A function that does nothing
#' 
#' @param ... anything
#' @export
nothing <- function(...){ }

#' Passes arguments 2-n to argument 1
#' 
#' @param f a function
#' @param ... anything
#' @export
execute <- function(f, ...) {
  if(!classcheck('function', f)){
    error("the first argument in `execute` must be a function")
  }
  f(...)
}

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

#' Add input type checking to a typed function
#'
#' @param f a typed, unary function
#' @param val (a -> Bool) - determines in input is correct
#' @param pass function called if val returns TRUE
#' @param fail function called if val returns FALSE
#' @return f
#' @export
valify <- function(f, val=true, pass=execute, fail=nothing){
  if(length(val) != nhtypes(f)){
    warn("FAILED: expected 2 validators, got %d", nhtypes(f))
    return(f)
  }

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

#' Make composition of functions
#' 
#' @param ... two or more functions
#' @return A function that is a composition of the input functions
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
    stop(sprintf("Invalid indices, select from 1-%s\n", length(x)))
  }
  tuple <- x[i]
  htype(tuple) <- rclass(tuple) 
  tuple <- add_class(tuple, 'tuple')
  tuple
}

# #' Split output of a function into downstream functions
# #'
# #' @param f,g,h input functions
# #' @return a list of two functions
# #' @export
# tee <- function(f, g, h) {
#   ~ list(g(f(.)), h(f(.)))
# }

# #' Wrap a pure function in a monad ...
# #'
# #' @param fpure,fival,foval,fstat,feff some functions
# #' @return wrapped function
# #' @export
# active_node <- function(fpure, fival, foval, fstat, feff){
#   typecheck(fpure, 'unary')
#   fun <- function(M) {
#     a <- M$value
#     b <- NULL
#     if(!is.null(a) && fival(a)){
#       b <- fpure(a)
#       if(foval(b)){
#         feff(M, b)
#       } else {
#         b <- NULL
#       }
#     }
#     state <- fstat(M, b)
#     vlift(value=b, state=state)
#   }
#   class(fun) <- c('active_node', 'function')
#   attributes(fun)$.itype <- attributes(fpure)$.itype
#   attributes(fun)$.otype <- attributes(fpure)$.otype
#   fun
# }

# #' Contextualize an active function
# #'
# #' @param f input active_node
# #' @param inode,onode input and output nodes
# #' @return a wired_node
# #' @export
# wired_node <- function(f, inode=NULL, onode=NULL){
#   stopifnot(
#     typecheck(f, 'active_node')      &&
#     ltypecheck(inode, 'active_node') &&
#     ltypecheck(onode, 'active_node')
#   )
#   class(f) <- c('wired_node', class(f))
#   attributes(f)$inode <- inode
#   attributes(f)$onode <- onode
#   attributes(f)$cache <- NULL
#   return(f)
# }

# #' Execute a node and cache result
# #'
# #' @param f node to be executed
# #' @export
# run <- function(f) {
#   typecheck(f, 'wired_node')
#   ltypecheck(f$inode)
#   ltypecheck(f$onode)
#   # run using data from inode caches, if NULL, run them first, and cache
# }
