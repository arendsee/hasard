#' Build function
#'
#' @section Arguments:
#'
#' \itemize{
#'   \item .itype input type
#'   \item .otype output type
#'   \item .inode input nodes
#'   \item .val (a -> Bool) - determines in input is correct
#'   \item .iunwrap input unwrapper
#'   \item .owrap output wrapper
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
  owrap  = id,
  effect = nothing,
  cacher = nocache,
  args   = list()
){

  fun <- function(
    .f      = f,
    .owrap  = owrap,
    .effect = effect,
    .cacher = cacher,
    .delete = FALSE,
    .args   = args
  ) {
    b = NULL
    if(.delete){ .cacher('del') }
    if(!.cacher('chk')){
      b <- do.call(.f, .args) 
      B <- owrap(b)
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
  itype   = '*',
  otype   = '*',
  inode   = NULL,
  val     = true,
  iunwrap = id,
  owrap   = id,
  pass    = execute,
  fail    = nothing,
  effect  = nothing,
  cacher  = nocache,
  args    = list()
){

  if(is.typed(val) && !((itype == "*" || ip(val) == itype) && (op(val) == "Bool")))
  {
    msg <- "expect val :: (%s -> Bool), got (%s -> %s)"
    warn(msg, itype, ip(val), op(val))
  }

  fun <- function(
    x,
    .f       = f,
    .val     = val,
    .inode   = inode,
    .iunwrap = iunwrap,
    .owrap   = owrap,
    .pass    = pass,
    .fail    = fail,
    .effect  = effect,
    .cacher  = cacher,
    .delete  = FALSE,
    .args    = args
  ) {
    if(.delete){ .cacher('del') }

    if(missing(x)){
      if(.cacher('chk')){
        return(.cacher('get'))
      }
      x <- .inode()
    }

    a <- .iunwrap(x)
    if(.val(a)){
      b <- do.call(.pass, append(list(.f, a), .args)) 
    } else {
      b <- do.call(.fail, append(list(.f, a), .args)) 
    }
    B <- .owrap(b, A=x)

    runall(.effect, A=x, B=B)
    .cacher('put', B)
    B
  }

  htype(fun) <- c(itype, otype)

  fun <- add_class(fun, 'hnode', 'unary')

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
