typecheck <- function(x, type){
  good = TRUE
  if(is.null(x)){
    good = FALSE 
  }
  if(! type %in% class(x)){
    good = FALSE
  }
  return(good)
}

ltypecheck <- function(x, type){
  good = TRUE
  if(is.null(x)){
    good = FALSE 
  }
  good = all(unlist(lapply(x, typecheck)))
  return(good)
}

composition_test <- function(f, g){
  typecheck(f, 'unary')
  typecheck(g, 'unary')
  if( are_composable(f, g) ){
    stop("Composition g . f is illegal")
  }
}

vlift <- function(value, state=list(ok=TRUE)) {
  val <- list(value=value, state=state)
  class(val) <- c('monad', class(val))
}

flift <- function(f) {
  function(x) {
    val <- f(x$value)
    vlift(val, x$state)
  }
}

