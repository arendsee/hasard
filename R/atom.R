#' Basic functions
#'
#' \itemize{
#'   \item true - ignore all input and return TRUE
#'   \item false - ignore all input and return FALSE
#'   \item nothing - ignore all input and do nothing
#'   \item execute - pass ... to FUN and return the results
#' }
#'
#' @param FUN any function
#' @param ... anything
#' @name basic_functions

#' @rdname basic_functions
#' @export
true <- function(...) {
  TRUE
}

#' @rdname basic_functions
#' @export
false <- function(...) {
  FALSE
}

#' @rdname basic_functions
#' @export
nothing <- function(...){
  NULL
}

#' @rdname basic_functions
#' @export
execute <- function(FUN, ...) {
  if(!classcheck('function', FUN)){
    error("the first argument in `execute` must be a function")
  }
  FUN(...)
}
