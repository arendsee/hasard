#' Get and set elements of an hnode
#'
#' The `h_*` functions return the object after evaluating it within the hnode
#' environment. For example,
#'
#' ```
#' h_fun <- function(h) { eval(formals(h)$.fun, environment(h)) }
#' ```
#'
#' The  `h_*_ne` functions return the object directly (they will normally be
#' expressions). The `ne` stands for 'no evaluation'.
#'
#' ```
#' h_fun_ne <- function(h) { formals(h)$.fun }
#' ```
#'
#' The `h_*_ne` family is mostly useless. I keep it around for testing.
#'
#' @param h a function of the 'hnode' class
#' @param value right hand value for assignment
#' @name hnode_setters
NULL



#' @rdname hnode_setters
#' @export
h_fun <- function(h) { eval(formals(h)$.fun, environment(h)) }

#' @rdname hnode_setters
#' @export
h_inode <- function(h) { eval(formals(h)$.inode, environment(h)) }

#' @rdname hnode_setters
#' @export
h_val <- function(h) { eval(formals(h)$.val, environment(h)) }

#' @rdname hnode_setters
#' @export
h_pass <- function(h) { eval(formals(h)$.pass, environment(h)) }

#' @rdname hnode_setters
#' @export
h_fail <- function(h) { eval(formals(h)$.fail, environment(h)) }

#' @rdname hnode_setters
#' @export
h_effect <- function(h) { eval(formals(h)$.effect, environment(h)) }

#' @rdname hnode_setters
#' @export
h_cacher <- function(h) { eval(formals(h)$.cacher, environment(h)) }

#' @rdname hnode_setters
#' @export
h_fun_ne <- function(h) { formals(h)$.fun }

#' @rdname hnode_setters
#' @export
h_inode_ne <- function(h) { formals(h)$.inode }

#' @rdname hnode_setters
#' @export
h_val_ne <- function(h) { formals(h)$.val }

#' @rdname hnode_setters
#' @export
h_pass_ne <- function(h) { formals(h)$.pass }

#' @rdname hnode_setters
#' @export
h_fail_ne <- function(h) { formals(h)$.fail }

#' @rdname hnode_setters
#' @export
h_effect_ne <- function(h) { formals(h)$.effect }

#' @rdname hnode_setters
#' @export
h_cacher_ne <- function(h) { formals(h)$.cacher }

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
      if(is.name(value) || is.call(value)){
        k <- value
        v <- eval(value, parent.frame(3))
      } else {
        k <- substitute(value)
        v <- value
      }
      if(check(h, v)){
        a <- attributes(h)
        formals(h)[[field]] <- k
        attributes(h) <- a
        attributes(h)$srcref <- NULL
      } else {
        stop(sprintf("Assignment to '%s' failed", field))
      }
      h
    }
  )
  fun
}

check_fun_ <- function(h, value){
  success <- TRUE
  if(!is.function(value)){
    message(sprintf(
      "Expected function, got '%s'",
      paste0(class(value), collapse="', '")
    ))
    success <- FALSE
  }
  if(npositional(value) != nhargs(h)){
    message(sprintf(
      "found %d positional arguments in %s, but this node requires a function of type %s",
      npositional(value),
      deparse(substitute(value)),
      type_str(h)
    ))
    success <- FALSE
  }
  success
}

check_inode_ <- function(h, value){
  success <- TRUE
  if(is.function(value)){
    value <- list(value)
  } else if(is.list(value)){
    if(!all(sapply(value, is.function))){
      message('all elements of an inode list must be functions')
      success <- FALSE
    }
  } else {
    message("inode must be a function or list of functions")
    success <- FALSE
  }
  if(length(value) != nhargs(h)){
    message(sprintf(
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
`h_inode<-` <- set_(".inode", check=check_inode_)

#' @export
#' @rdname hnode_setters
`h_fun<-` <- set_(".fun", check=check_fun_)

#' @export
#' @rdname hnode_setters
`h_val<-` <- set_(".val")

#' @export
#' @rdname hnode_setters
`h_pass<-` <- set_(".pass")

#' @export
#' @rdname hnode_setters
`h_fail<-` <- set_(".fail")

#' @export
#' @rdname hnode_setters
`h_effect<-` <- set_(".effect")

#' @export
#' @rdname hnode_setters
`h_cacher<-` <- set_(".cacher")

#' @export
#' @rdname hnode_setters
`h_fail<-` <- set_(".fail")

#' @export
#' @rdname hnode_setters
`h_args<-` <- set_(".args")

#' @export
#' @rdname hnode_setters
`h_delete<-` <- set_(".delete")
