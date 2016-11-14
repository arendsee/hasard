#' Getters and setters
#' 
#' \itemize{
#'   \item htype - Get or set types for unary function
#'   \item nhtypes - The number of types a function has (2 for unary functions)
#'   \item ip,op - get input and output types
#'   \item inode,onode - get/set input and output nodes
#' }
#' 
#' @param f any function
#' @param value character string
#' @name assignment

#' @rdname assignment
#' @export
htype <- function(f){
  attr(f, 'htype')
}

#' @rdname assignment
#' @export
`htype<-` <- function(f, value){
  if(npositional(f) != (length(value) - 1)){
    warn("Expected %d types, got %d", npositional(f)+1, length(value))
  }
  attr(f, 'htype') <- value
  f <- add_class(f, 'typed')
  f
}

#' @rdname assignment
#' @export
nhtypes <- function(f){
  length(htype(f))
}

#' @rdname assignment
#' @export
inode <- function(f){
  attr(f, 'inode')
}

#' @rdname assignment
#' @export
`inode<-` <- function(f, value){
  attr(f, 'inode') <- value
  f
}

#' @rdname assignment
#' @export
onode <- function(f){
  attr(f, 'onode')
}

#' @rdname assignment
#' @export
`onode<-` <- function(f, value){
  attr(f, 'onode') <- value
  f
}

#' @rdname assignment
#' @export
ip <- function(f) {
  htype(f)[1]
}

#' @rdname assignment
#' @export
`ip<-` <- function(f, value){
  if(is.null(htype(f))){
    htype(f) <- c('*', '*')
    warn("Initializing output to '*'")
  }
  htype(f)[1] <- value
  f
}

#' @rdname assignment
#' @export
op <- function(f) {
  htype(f)[2]
}

#' @rdname assignment
#' @export
`op<-` <- function(f, value){
  if(is.null(htype(f))){
    htype(f) <- c('*', '*')
    warn("Initializing input to '*'")
  }
  htype(f)[2] <- value
  f
}
