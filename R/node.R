#' Build function
#'
#' @section Arguments:
#'
#' \itemize{
#'   \item type function type
#'   \item f function
#'   \item inode input nodes
#'   \item val (a -> Bool) - determines in input is correct
#'   \item pass function called if val returns TRUE
#'   \item fail function called if val returns FALSE
#'   \item effect a function of a and b
#'   \item cacher the caching function
#' }
#'
#' @param type a functional type ala Haskell
#' @param ... see Arguments
#' @name node
NULL

hsource_ <- function(
  type,
  f      = nothing,
  effect = nothing,
  cacher = nocache,
  args   = list()
){

  fun <- function(){}
  body(fun) <- quote( 
    {
      if(.delete){ .cacher('del') }
      if(!.cacher('chk')){
        b <- do.call(.fun, .args)
        runall(.effect, b)
        .cacher('put', b)
      } else {
        b <- .cacher('get')
      }
      b
    }
  )

  htype(fun) <- type
  fun <- add_class(fun, 'hnode', 'source')

  h_fun(fun)    <- substitute(f)
  h_effect(fun) <- substitute(effect)
  h_cacher(fun) <- substitute(cacher)
  h_delete(fun) <- FALSE
  h_args(fun)   <- substitute(args)

  fun
}

default_fun <- function(type){
  N <- nhargs(type)
  fun <- nothing
  if(N == 0){
    formals(fun) <- c()
  } else {
    formals(fun) <-
      c(letters[1:N]) %>%
      {parse(text=sprintf('alist(%s =)', paste(., collapse="= ,")))} %>%
      eval
  }
  # htype(fun) <- type
  fun
}

default_inode <- function(type){
  N <- nhargs(type)
  if(N == 0){
    inode <- list()
  } else if(N == 1) {
    inode <- list(nothing)
    formals(inode[[1]]) <- alist(x=)
  } else {
    inode <- list()
    for(i in 1:N){
      fun <- nothing
      formals(fun) <- eval(parse(text=sprintf('alist(%s =)', letters[i])))
      inode[[i]] <- fun
    }
  }
  inode
}

hpipe_ <- function(
  type,
  f       = default_fun(type),
  inode   = default_inode(type),
  val     = true,
  pass    = execute,
  fail    = nothing,
  effect  = nothing,
  cacher  = nocache,
  args    = list()
){

  fun <- function(){}
  body(fun) <- quote(
    {
      if(.delete){ .cacher('del') }

      if(.cacher('chk')){
        return(.cacher('get'))
      }

      if(class(.inode)[1] != 'list'){
        .inode <- list(.inode)
      }

      a <- lapply(.inode, execute)

      funlist <- append(.fun, append(a, args))

      if(do.call(.val, a)){
        b <- do.call(.pass, funlist)
      } else {
        b <- do.call(.fail, funlist)
      }

      runall(.effect, b, h_input=a)
      .cacher('put', b)
      b
    }
  )

  htype(fun) <- type
  fun <- add_class(fun, 'hnode')

  if(missing(f)){
    do_nothing <- force(f)
    h_fun(fun) <- do_nothing
  } else {
    h_fun(fun) <- substitute(f)
  }

  if(missing(inode)){
    input_nothing <- force(inode)
    h_inode(fun) <- input_nothing
  } else {
    h_inode(fun) <- substitute(inode)
  }

  h_val(fun)    <- substitute(val)
  h_pass(fun)   <- substitute(pass)
  h_fail(fun)   <- substitute(fail)
  h_effect(fun) <- substitute(effect)
  h_cacher(fun) <- substitute(cacher)
  h_args(fun)   <- substitute(args)
  h_delete(fun) <- FALSE

  fun
}

#' @rdname node
#' @export
hnode <- function(type, ...){
  type <- parse_type(type)
  if(nhargs(type) == 0){
    fun <- hsource_(type, ...)
  } else {
    fun <- hpipe_(type, ...)
  }
  parent.env(environment(fun)) <- parent.frame()
  fun
}
