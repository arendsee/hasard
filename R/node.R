#' Build function
#'
#' @section Arguments:
#'
#' \itemize{
#'   \item .itype input type
#'   \item .otype output type
#'   \item .inode input nodes
#'   \item .val (a -> Bool) - determines in input is correct
#'   \item .pass function called if val returns TRUE
#'   \item .fail function called if val returns FALSE
#'   \item .effect a function of a and b
#'   \item .cacher the caching function
#' }
#'
#' @param f pure function
#' @param ... see Arguments
#' @name node
NULL

hsource_ <- function(
  f,
  otype  = '*',
  effect = nothing,
  cacher = nocache,
  args   = list()
){

  fun <- function(
    .f      = f,
    .effect = effect,
    .cacher = cacher,
    .delete = FALSE,
    .args   = args
  ) {
    b = NULL
    if(.delete){ .cacher('del') }
    if(!.cacher('chk')){
      b <- do.call(.f, .args) 
      runall(.effect, b=b)
      .cacher('put', b)
    } else {
      b <- .cacher('get')
    }
    b
  }

  htype(fun) <- c(NA, otype)

  fun <- add_class(fun, 'hnode', 'unary', 'source')

  fun
}

hpipe_ <- function(
  f,
  inode,
  itype   = rep('*', length(inode)),
  otype   = '*',
  val     = true,
  pass    = execute,
  fail    = nothing,
  effect  = nothing,
  cacher  = nocache,
  args    = list()
){

  fun <- function(
    .fun     = f,
    .inode   = inode,
    .val     = val,
    .pass    = pass,
    .fail    = fail,
    .effect  = effect,
    .cacher  = cacher,
    .delete  = FALSE,
    .args    = args
  ){
    if(.delete){ .cacher('del') }

    if(.cacher('chk')){
      return(.cacher('get'))
    }

    if(class(.inode) != 'list'){
      error("expected class(.inode) == 'list', found '%s'", class(.inode))
    }

    if(!all(unlist(lapply(.inode, is.hnode)))){
      error("all inode functions must be of class 'hnode'")
    }

    if(npositional(.fun) != length(.inode)){
      warn(
        "found %d arguments in .inode, expected %d (from .fun)",
        npositional(.fun),
        length(.inode)
      )
    }

    a <- lapply(.inode, execute) 

    funlist <- append(.fun, append(a, args))

    if(do.call(.val, a)){
      b <- do.call(.pass, funlist)
    } else {
      b <- do.call(.fail, funlist)
    }

    runall(.effect, b=b, b=b)
    .cacher('put', b)
    b
  }

  htype(fun) <- c(unlist(lapply(inode, op)), otype)

  fun <- add_class(fun, 'hnode')

  fun
}

#' @rdname node
#' @export
hnode <- function(f, ...){
  if(npositional(f) == 0){
    fun <- hsource_(f, ...)
  } else {
    fun <- hpipe_(f, ...)
  }
  fun
}
