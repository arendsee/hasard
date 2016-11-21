#' Create a new validator
#' 
#' @param type the type of the function
#' @param f function that should be of the specified input type
#' @return a validator function
#' @export
make_validator <- function(type, f=true){
  fun <- function(...) {
    all( f(...) )
  }
  fun <- add_class(fun, 'validator', 'typed')
  type <- parse_type(type)
  htype(fun) <- type
  if(rev(type)[1] != 'Bool'){
    warning("Expected output type to be 'Bool' for a validator")
  }
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
make_cacher <- function(fchk, fdel, fput, fget){
  function(op, ...){
    switch(
      op,
      chk = fchk(...),
      del = fdel(...),
      put = fput(...),
      get = fget(...)
    )
  }
}

#' Create a new effector function
#' 
#' @param type the type of the function
#' @param f function that should be of the specified input type
#' @return an effector function
#' @export
make_effector <- function(type, f) {
  fun <- function(...) {
    f(...)
  }
  fun <- add_class(fun, 'effector', 'typed')
  type <- parse_type(type)
  htype(fun) <- type
  fun
}
