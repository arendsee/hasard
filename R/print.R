#' Print unary function
#' 
#' @param x input function
#' @param ... additional arguments, will be ignored
#' @export
print.unary <- function(x, ...) {
  print(sprintf(
    "%s :: %s -> %s",
    deparse(substitute(x)),
    htype(x)[1],
    htype(x)[2]
  ))
}
