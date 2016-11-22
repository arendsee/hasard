#' Create a new validator
#' 
#' @param type the type of the function
#' @param f function that should be of the specified input type
#' @return a validator function
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
