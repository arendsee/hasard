#' Collapse all arguments into unary closure
#'
#' @param f arbitrary function
#' @param ... additional arguments
#' @export
unify <- function(f, ...){
  function(x){
    f(x, ...)
  }
}

#' Get number of formal positional arguments
#'
#' @param f arbitrary function
#' @export
npositional <- function(f){
  formals(f)                  %>%
    lapply(deparse)           %>%
    lapply(nchar)             %>%
    unlist                    %>%
    { .[!names(.) == '...'] } %>%
    { sum(. == 0) }
}

#' Get number of formal arguments
#'
#' @param f arbitrary function
#' @export
nformals <- function(f){
  length(methods::formalArgs(f))
}

#' Run functions for their side effects
#' 
#' @param f a function or a list of functions
#' @param ... arguments sent to f
#' @export
runall <- function(f, ...){
  if(!all(class(f) == 'list')){
    f <- list(f)
  }
  if(all(sapply(f, is.function))){
    lapply(f, execute, ...)
  } else {
    stop("f must be a function or list of functions")
  }
}

#' Check the htype
#' 
#' @param CLASS expected class
#' @param ... any number of objects
#' @return TRUE if all elements in ... have class CLASS
#' @export
classcheck <- function(CLASS, ...){
  if(!is.character(CLASS)){
    stop("CLASS must be a character vector")
  }
  all(unlist(lapply(list(...), function(x) CLASS %in% class(x))))
}

#' Prepend a class to the class vector
#' 
#' @param x anything
#' @param ... classes to add
#' @return x with the new class prepended
#' @export
add_class <- function(x, ...){
  add_class_ <- function(x, cls) {
    if(!classcheck(cls, x)){
      class(x) <- c(cls, class(x))
    }
    x
  }
  Reduce(add_class_, unlist(list(...)), init=x)
}

# #' Insert elements between every member of a list
# #'
# #' @param x any list or vector
# #' @return s list or vector to be intercalated
# #' @export
# #' @examples
# #' intercalate(letters, ',')
# intercalate <- function(x, s){
#   lapply(x, c, s) %>% unlist
# }

#' Like splits on a pattern but keeps the pattern
#' 
#' @param x a character vector
#' @return p a pattern (can be a regular expression)
#' @export
#' @examples
#' keep_split(c('a,b', ',acat,,eh'), ',')
#' keep_split('(a){strangely(punctuated..*sentence', '[^\\w]')
keep_split <- function(x, p, dummy='@@'){
  pattern <- paste0('(', p, ')')
  replacement <- paste0(dummy, '\\1', dummy)
  strsplit(gsub(pattern, replacement, x, perl=TRUE), dummy)
}

#' Create a counter
#'
#' @export
make_get_uid <- function(initial=1) {
  i = initial
  function(){
    i <<- i + 1
    i
  }
}

#' Unique id generator (1,2, ..., n)
#'
#' @export
get_uid <- make_get_uid(1)
