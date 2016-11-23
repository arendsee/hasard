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

#' Get and set elements of an hnode
#'
#' @param h a function of the 'hnode' class
#' @param value right hand value for assignment
#' @name hnode_setters
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
  htype(fun) <- type
  fun
}

default_inode <- function(type){
  N <- nhargs(type)
  if(N == 0){
    inode <- nothing
    formals(inode) <- c()
  } else if(N == 1) {
    inode <- nothing
    formals(inode) <- alist(x=)
  } else {
    inode <- list()
    for(i in 1:N){
      fun <- nothing
      formals(fun) <- eval(parse(text=sprintf('alist(%s =)', letters[i])))
      inode[[1]] <- fun
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

  # h_fun(fun)    <- substitute(f)
  # h_inode(fun)  <- substitute(inode)
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

#' @rdname hnode_setters
#' @export
h_fun <- function(h) { formals(h)$.fun }

#' @rdname hnode_setters
#' @export
h_inode <- function(h) { formals(h)$.inode }

#' @rdname hnode_setters
#' @export
h_val <- function(h) { formals(h)$.val }

#' @rdname hnode_setters
#' @export
h_pass <- function(h) { formals(h)$.pass }

#' @rdname hnode_setters
#' @export
h_fail <- function(h) { formals(h)$.fail }

#' @rdname hnode_setters
#' @export
h_effect <- function(h) { formals(h)$.effect }

#' @rdname hnode_setters
#' @export
h_cacher <- function(h) { formals(h)$.cacher }

#' @rdname hnode_setters
#' @export
h_args <- function(h) { formals(h)$.args }

#' @rdname hnode_setters
#' @export
h_delete <- function(h) { formals(h)$.delete }

set_ <- function(field, check=true) {
  fun <- function(h, value){}
  body(fun) <- substitute(
    {
      if(is.name(value)){
        k <- value
        v <- dynGet(deparse(value), inherits=TRUE)
      } else if(is.call(value)){
        k <- value
        v <- eval(value)
      } else {
        k <- substitute(value)
        v <- value
      }
      if(check(h, v)){
        a <- attributes(h)
        formals(h)$field <- k
        attributes(h) <- a
      } else {
        stop("Assignment to '%s' failed", deparse(field))
      }
      h
    }
  )
  environment(fun) <- parent.frame()
  fun
}

check_fun_ <- function(h, value){
  success <- TRUE
  if(!is.function(value)){
    warning(sprintf("Expected function, got '%s'", paste0(class(value), collapse="', '")))
    success <- FALSE
  }
  if(npositional(value) != nhargs(h)){
    warning(sprintf(
      "found %d positional arguments in value, but this node requires a function of type %s",
      npositional(value),
      type_str(h)
    ))
    success <- FALSE
  }
  success
}

check_inode_ <- function(h, value){
  success <- TRUE
  if(class(value)[1] != 'list'){
    value <- list(value)
  }
  # if(!all(sapply(value, is.hnode))){
  #   warning("input node is not of class 'hnode', this may be OK")
  # }
  if(length(value) != nhargs(h)){
    warning(sprintf(
      "found %d arguments in .inode, expected %d for function of type %s",
      length(value),
      nhargs(h),
      type_str(h)
    ))
    success <- FALSE
  }
  success
}

#' @export
#' @rdname hnode_setters
`h_fun<-` <- set_(.fun, check=check_fun_)

#' @export
#' @rdname hnode_setters
`h_inode<-` <- set_(.inode, check=check_inode_)

#' @export
#' @rdname hnode_setters
`h_val<-` <- set_(.val)

#' @export
#' @rdname hnode_setters
`h_pass<-` <- set_(.pass)

#' @export
#' @rdname hnode_setters
`h_fail<-` <- set_(.fail)

#' @export
#' @rdname hnode_setters
`h_effect<-` <- set_(.effect)

#' @export
#' @rdname hnode_setters
`h_cacher<-` <- set_(.cacher)

#' @export
#' @rdname hnode_setters
`h_fail<-` <- set_(.fail)

#' @export
#' @rdname hnode_setters
`h_args<-` <- set_(.args)

#' @export
#' @rdname hnode_setters
`h_delete<-` <- set_(.delete)
