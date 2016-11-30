#' Create a new validator
#' 
#' @param type the type of the function
#' @param f function that should be of the specified input type
#' @return a validator function
#' @family makers
#' @export
make_validator <- function(type, f=true){
  type <- parse_type(type)
  if(rev(type)[1] != 'Bool'){
    warning("Expected output type to be 'Bool' for a validator")
  }

  fun <- function(...) {}
  body(fun) <- substitute( { all( f(...) ) } )
  environment(fun) <- parent.frame()

  fun <- add_class(fun, 'validator', 'typed')
  htype(fun) <- type

  fun
}

#' Create a caching function
#'
#' @param fchk determines if a cached value exists
#' @param fdel deletes any cached value
#' @param fput caches the input
#' @param fget returns the cached value
#' @return a caching function
#' @family makers
#' @export
make_cacher <- function(fchk=false, fdel=nothing, fput=nothing, fget=nothing){
  fun <- function(op, ...){}
  body(fun) <- substitute(
    {
      switch(
        op,
        chk = fchk(...),
        del = fdel(...),
        put = fput(...),
        get = fget(...)
      )
    }
  )
  environment(fun) <- parent.frame()
  fun <- add_class(fun, 'cacher')
  fun
}

#' Create a new effector function
#' 
#' @param type the type of the function
#' @param f function that should be of the specified input type
#' @return an effector function
#' @family makers
#' @export
make_effector <- function(type, f=nothing) {
  type <- parse_type(type)
  fun <- function(...) {}
  body(fun) <- substitute( { f(...) } )
  environment(fun) <- parent.frame()

  fun <- add_class(fun, 'effector', 'typed')
  htype(fun) <- type

  fun
}

#' Create a default (NULL returning) function for a given type
#'
#' @param type functional type
#' @seealso default_inode
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

#' Create a default inode object for a given type
#'
#' @param type functional type
#' @seealso default_fun
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
