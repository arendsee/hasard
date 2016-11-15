#' Create a new validator
#' 
#' @param f function with type: f :: vclass -> logical
#' @param vclass the input class
#' @return a unary, validator function
#' @export
make_validator <- function(f, vclass){
  fun <- function(x) {
    f(x)
  }
  fun <- add_class(fun, 'validator', 'unary')
  htype(fun) <- c(vclass, 'Bool')
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

make_effector <- function(...) {
  nothing
}

make_wrapper <- function(...) {
  id
}
