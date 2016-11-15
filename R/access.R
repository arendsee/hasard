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
  htype(f)[1:(nhtypes(f)-1)]
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
  htype(f)[nhtypes(f)]
}

#' @rdname access
#' @export
`op<-` <- function(f, value){
  if(is.null(htype(f))){
    warn("f does not have a type, initializing input to '*'")
    htype(f) <- c('*', '*')
  }
  htype(f)[nhtypes(f)] <- value
  f
}
