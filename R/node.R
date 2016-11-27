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
#' @name node
NULL

#' @rdname node
#' @export
default_fun <- function(type){
  type <- parse_type(type)
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
  fun
}

#' @rdname node
#' @export
default_inode <- function(type){
  type <- parse_type(type)
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

#' @rdname node
#' @export
hwell <- function(type){
  type <- parse_type(type, role='well')

  h <- function(
    .fun    = nothing,
    .effect = nothing,
    .cacher = nocache,
    .delete = FALSE,
    .args   = list()
  ){
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

  h <- add_class(h, 'hnode', 'well')
  htype(h) <- type
  parent.env(environment(h)) <- parent.frame()
  h
}

#' @rdname node
#' @export
hpipe <- function(type){
  type <- parse_type(type)

  h <- function(
    type,
    .fun    = default_fun(type),
    .inode  = default_inode(type),
    .val    = true,
    .pass   = execute,
    .fail   = nothing,
    .effect = nothing,
    .cacher = nocache,
    .delete = FALSE,
    .args   = list()
  ){
    if(.delete) .cacher('del')
    if(.cacher('chk')) return(.cacher('get'))
    a <- runall(.inode)
    funlist <- append(.fun, append(a, .args))
    if(do.call(.val, a)){
      b <- do.call(.pass, funlist)
    } else {
      b <- do.call(.fail, funlist)
    }
    runall(.effect, b, h_input=a)
    .cacher('put', b)
    b
  }

  h <- add_class(h, 'hnode', 'pipe')
  htype(h) <- type
  parent.env(environment(h)) <- parent.frame()
  h
}

#' @rdname node
#' @export
hsink <- function(type){
  type <- parse_type(type, 'sink')
  h <- hpipe(type)
  h <- add_class(h, 'hnode', 'sink')
  parent.env(environment(h)) <- parent.frame()
  h
}
