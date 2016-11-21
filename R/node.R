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
#' @param h hnode function
#' @param value right hand value for assignment
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

  fun <- function(.fun, .effect, .delete, .cacher, .args){

    .fun    = eval(.fun)
    .effect = eval(.effect)
    .cacher = eval(.cacher)
    .args   = eval(.args)

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

  formals(fun)$.fun    = substitute(f)
  formals(fun)$.effect = substitute(effect)
  formals(fun)$.cacher = substitute(cacher)
  formals(fun)$.delete = FALSE
  formals(fun)$.args   = substitute(args)

  htype(fun) <- type
  fun <- add_class(fun, 'hnode', 'unary', 'source')

  fun
}

hpipe_ <- function(
  type,
  f       = nothing,
  inode   = nothing,
  val     = true,
  pass    = execute,
  fail    = nothing,
  effect  = nothing,
  cacher  = nocache,
  args    = list()
){

  fun <- function(.fun, .inode, .val, .pass, .fail, .effect, .delete, .cacher, .args){

    .fun    <- eval(.fun)
    .inode  <- eval(.inode)
    .val    <- eval(.val)
    .pass   <- eval(.pass)
    .fail   <- eval(.fail)
    .effect <- eval(.effect)
    .cacher <- eval(.cacher)
    .args   <- eval(.args)

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

  formals(fun)$.fun    <- substitute(f)
  formals(fun)$.inode  <- substitute(inode)
  formals(fun)$.val    <- substitute(val)
  formals(fun)$.pass   <- substitute(pass)
  formals(fun)$.fail   <- substitute(fail)
  formals(fun)$.effect <- substitute(effect)
  formals(fun)$.cacher <- substitute(cacher)
  formals(fun)$.args   <- substitute(args)
  formals(fun)$.delete <- FALSE

  htype(fun) <- type
  fun <- add_class(fun, 'hnode')

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
  fun
}

#' @rdname node
#' @export
h_fun <- function(h) { formals(h)$.fun }

#' @rdname node
#' @export
h_inode <- function(h) { formals(h)$.inode }

#' @rdname node
#' @export
h_val <- function(h) { formals(h)$.val }

#' @rdname node
#' @export
h_pass <- function(h) { formals(h)$.pass }

#' @rdname node
#' @export
h_fail <- function(h) { formals(h)$.fail }

#' @rdname node
#' @export
h_effect <- function(h) { formals(h)$.effect }

#' @rdname node
#' @export
h_cacher <- function(h) { formals(h)$.cacher }

#' @rdname node
#' @export
h_args <- function(h) { formals(h)$.args }

#' @rdname node
#' @export
h_delete <- function(h) { formals(h)$.delete }

# set_ <- function(field, check=true) {
#   function(h, value){
#     if(is.name(value)){
#       v <- eval(value)
#       k <- value
#     } else {
#       v <- value
#       k <- substitute(value)
#     }
#     if(check(h, v)){
#       a <- attributes(h)
#       formals(h)[[field]] <- k
#       attributes(h) <- a
#     }
#     h
#   }
# }

#' @rdname node
#' @export
`h_fun<-` <- function(h, value){
  # if(is.name(value)){
  #   v <- eval(value)
  #   k <- value
  # } else {
  #   v <- value
  #   k <- substitute(value)
  # }
  # if(!is.function(v)){
  #   error("Expected function, got '%s'", paste0(class(v), collapse="', '"))
  # }
  # if(npositional(v) != nhargs(h)){
  #   warn(
  #     "found %d positional arguments in value, but this node requires a function of type %s",
  #     npositional(v),
  #     type_str(h)
  #   )
  # }
  # a <- attributes(h)
  # formals(h)$.fun <- k
  # attributes(h) <- a
  # h
  a <- attributes(h)
  formals(h)$.fun <- substitute(value)
  attributes(h) <- a
  h
}

#' @rdname node
#' @export
`h_inode<-` <- function(h, value) {
  # if(all(class(value) != 'list')){
  #   value <- list(value)
  # }
  # if(!all(unlist(lapply(value, is.hnode)))){
  #   warn("this function is not of class 'hnode'")
  # }
  # if(length(value) != nhargs(h)){
  #   warn(
  #     "found %d arguments in .inode, expected %d for function of type %s",
  #     length(value),
  #     nhargs(h),
  #     type_str(h)
  #   )
  # }
  a <- attributes(h)
  formals(h)$.inode <- substitute(value)
  attributes(h) <- a
  h
}

#' @rdname node
#' @export
`h_val<-` <- function(h, value) {
  a <- attributes(h)
  formals(h)$.val <- substitute(value)
  attributes(h) <- a
  h
}

#' @rdname node
#' @export
`h_pass<-` <- function(h, value) {
  a <- attributes(h)
  formals(h)$.pass <- substitute(value)
  attributes(h) <- a
  h
}

#' @rdname node
#' @export
`h_fail<-` <- function(h, value) {
  a <- attributes(h)
  formals(h)$.fail <- substitute(value)
  attributes(h) <- a
  h
}

#' @rdname node
#' @export
`h_effect<-` <- function(h, value) {
  a <- attributes(h)
  formals(h)$.effect <- substitute(value)
  attributes(h) <- a
  h
}

#' @rdname node
#' @export
`h_cacher<-` <- function(h, value) {
  a <- attributes(h)
  formals(h)$.cacher <- substitute(value)
  attributes(h) <- a
  h
}

#' @rdname node
#' @export
`h_args<-` <- function(h, value) {
  a <- attributes(h)
  formals(h)$.args <- substitute(value)
  attributes(h) <- a
  h
}

#' @rdname node
#' @export
`h_delete<-` <- function(h, value) {
  a <- attributes(h)
  formals(h)$.delete <- as.logical(value)
  attributes(h) <- a
  h
}
