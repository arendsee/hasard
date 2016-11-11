#' Print unary function
#' 
#' @param x input function
#' @param ... additional arguments, will be ignored
#' @export
print.unary <- function(x, ...) {
  print(sprintf(
    "%s :: %s -> %s",
    deparse(substitute(x)),
    attributes(x)$.itype,
    attributes(x)$.otype
  ))
}

#' Print wired_node 
#' 
#' @param x input function
#' @param ... additional arguments, will be ignored
#' @export
print.wired_node <- function(x, ...) {
  print.unary(x, ...)
}

#' Print active_node 
#' 
#' @param x input function
#' @param ... additional arguments, will be ignored
#' @export
print.active_node <- function(x, ...) {
  print.unary(x, ...)
}
