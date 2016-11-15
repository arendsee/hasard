#' Getters and setters
#' 
#' \itemize{
#'   \item htype - Get or set types for unary function
#'   \item nhtypes - The number of types a function has (2 for unary functions)
#'   \item ip,op - get input and output types
#'   \item inode - get/set input nodes
#' }
#' 
#' @param f any function
#' @param value character string
#' @name access
NULL

#' @rdname access
#' @export
htype <- function(f){
  attr(f, 'htype')
}

#' @rdname access
#' @export
`htype<-` <- function(f, value){
  if(npositional(f) != (length(value) - 1)){
    warn("Expected %d types, got %d", npositional(f)+1, length(value))
  }
  attr(f, 'htype') <- value
  f <- add_class(f, 'typed')
  f
}

#' @rdname access
#' @export
nhtypes <- function(f){
  length(htype(f))
}

#' @rdname access
#' @export
inode <- function(f){
  formals(f)$inode
}

#' @rdname access
#' @export
`inode<-` <- function(f, value){
  formals(f)$inode <- value
  f
}

#' @rdname access
#' @export
ip <- function(f) {
  htype(f)[1]
}

#' @rdname access
#' @export
`ip<-` <- function(f, value){
  if(is.null(htype(f))){
    htype(f) <- c('*', '*')
    warn("Initializing output to '*'")
  }
  htype(f)[1] <- value
  f
}

#' @rdname access
#' @export
op <- function(f) {
  htype(f)[2]
}

#' @rdname access
#' @export
`op<-` <- function(f, value){
  if(is.null(htype(f))){
    htype(f) <- c('*', '*')
    warn("Initializing input to '*'")
  }
  htype(f)[2] <- value
  f
}
