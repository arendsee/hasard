#' Access, set, and print function types
#' 
#' @section Details:
#' 
#' Functional types are based on Haskell types. For example, a function that sums
#' two integers, might have the type, 'Int -> Int -> Int'. The first 2 `Int`s are
#' the input integers, that last is the output. Every hnode has a type attribute,
#' which contains $n+1$ elements in a character vector, where $n$ is the number of
#' inputs.
#' 
#' *htype* prints the type character vector, *ip* prints the input types (all
#' but the last elements), and *op* prints the output type (the last element of
#' the type vector). *nhtypes* counts the number of inputs. *type_str* converts
#' from a character vector to a Haskell syntax function definition.
#'
#' *parse_type* attempts to extract a type (as a character vector) from
#' whatever input it is given. Legal input is 1) Haskell style type (e.g. 'a ->
#' b') or 2) a character vector or 3) an object with a non-NULL htype
#' attribute. Anything else will result in an error.
#' 
#' @param f any function
#' @param value character string
#' @param x type string
#' @name type
NULL

#' @rdname type
#' @export
type_str <- function(f){
  sprintf("(%s)", paste0(htype(f), collapse=" -> "))
}

#' @rdname type
#' @export
parse_type <- function(x){
  # If the input is an object with a defined htype, use this type
  if(!is.null(htype(x))){
    x <- htype(x)
  }
  # If the type appears to be Haskell style, convert to vector
  if(length(x) == 1 && grepl('->', x)){
    x <- unlist(strsplit(x, '\\s*->\\s*', perl=TRUE))
    x <- gsub('\\(|\\)', '', x, perl=TRUE)
    x <- ifelse(x == 'NA', NA, x)
  }
  # If the type is not a character vector, die with great angst
  if(!is.character(x)){
    error("Input type must be a character vector")
  }
  x
}

#' @rdname type
#' @export
htype <- function(f){
  attr(f, 'htype')
}

#' @rdname type
#' @export
ip <- function(f) {
  rev(rev(htype(f))[-1])
}

#' @rdname type
#' @export
op <- function(f) {
  rev(htype(f))[1]
}

#' @rdname type
#' @export
`htype<-` <- function(f, value){
  attr(f, 'htype') <- parse_type(value)
  f <- add_class(f, 'typed')
  f
}

#' @rdname type
#' @export
`ip<-` <- function(f, value){
  if(is.null(htype(f))){
    htype(f) <- c('*', '*')
    warn("Initializing output to '*'")
  }
  htype(f)[1] <- value
  f
}

#' @rdname type
#' @export
`op<-` <- function(f, value){
  if(is.null(htype(f))){
    warn("f does not have a type, initializing input to '*'")
    htype(f) <- c('*', '*')
  }
  htype(f)[length(htype(f))] <- value
  f
}

#' @rdname type
#' @export
nhargs <- function(f){
  if(is.function(f)){
    types <- htype(f)
  } else if(is.character(f)) {
    types <- f
  } else {
    error("Cannot count arguments of class '%s'", paste0(class(f), collpase="', '"))
  }
  if(!is.null(types)){
    types <- rev(types)[-1]
    n <- length(types[!is.na(types)])
  } else {
    warn("This function is not typed, returning NULL")
    n <- NULL
  }
  n
}
