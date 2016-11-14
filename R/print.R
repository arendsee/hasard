#' Hasard print functions
#' 
#' @param x input function
#' @param ... additional arguments, will be ignored
#' @name print

#' @rdname print
#' @export
print.unary <- function(x, ...) {
  print(sprintf("%s :: %s -> %s", deparse(substitute(x)), ip(x), op(x)))
}

#' @rdname print
#' @export
print.typed <- function(x, ...) {
  print(sprintf(
    "%s :: %s",
    deparse(substitute(x)),
    paste0(htype(x), collapse=" -> ")
  ))
}
