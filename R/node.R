#' Build function
#'
#' @section Arguments:
#'
#' \itemize{
#'   \item itype input type
#'   \item otype output type
#'   \item inode input nodes
#'   \item val (a -> Bool) - determines in input is correct
#'   \item pass function called if val returns TRUE
#'   \item fail function called if val returns FALSE
#'   \item effect a function of a and b
#'   \item cacher the caching function
#' }
#'
#' @param f pure function
#' @param h hnode function
#' @param value right hand value for assignment
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
      runall(.effect, b)
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

    if(all(class(.inode) != 'list')){
      .inode <- list(.inode)
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

    runall(.effect, b, h_input=a)
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


#' @rdname node
#' @export
h_fun    <- function(h) { formals(h)$.fun    }

#' @rdname node
#' @export
h_inode  <- function(h) { formals(h)$.inode  }

#' @rdname node
#' @export
h_itype  <- function(h) { formals(h)$.itype  }

#' @rdname node
#' @export
h_otype  <- function(h) { formals(h)$.otype  }

#' @rdname node
#' @export
h_val    <- function(h) { formals(h)$.val    }

#' @rdname node
#' @export
h_pass   <- function(h) { formals(h)$.pass   }

#' @rdname node
#' @export
h_fail   <- function(h) { formals(h)$.fail   }

#' @rdname node
#' @export
h_effect <- function(h) { formals(h)$.effect }

#' @rdname node
#' @export
h_cacher <- function(h) { formals(h)$.cacher }

#' @rdname node
#' @export
h_args   <- function(h) { formals(h)$.args   }

#' @rdname node
#' @export
`h_fun<-`    <- function(h, value) { formals(h)$.fun    <- value; h }

#' @rdname node
#' @export
`h_inode<-`  <- function(h, value) { formals(h)$.inode  <- value; h }

#' @rdname node
#' @export
`h_itype<-`  <- function(h, value) { formals(h)$.itype  <- value; h }

#' @rdname node
#' @export
`h_otype<-`  <- function(h, value) { formals(h)$.otype  <- value; h }

#' @rdname node
#' @export
`h_val<-`    <- function(h, value) { formals(h)$.val    <- value; h }

#' @rdname node
#' @export
`h_pass<-`   <- function(h, value) { formals(h)$.pass   <- value; h }

#' @rdname node
#' @export
`h_fail<-`   <- function(h, value) { formals(h)$.fail   <- value; h }

#' @rdname node
#' @export
`h_effect<-` <- function(h, value) { formals(h)$.effect <- value; h }

#' @rdname node
#' @export
`h_cacher<-` <- function(h, value) { formals(h)$.cacher <- value; h }

#' @rdname node
#' @export
`h_args<-`   <- function(h, value) { formals(h)$.args   <- value; h }
