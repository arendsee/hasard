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
  good = lapply(x, typecheck) %>% unlist %>% all
  return(good)
}

composition_test <- function(f, g){
  typecheck(f, 'unary')
  typecheck(g, 'unary')
  if( are_composable(f, g) ){
    stop("Composition g . f is illegal")
  }
}

vlift <- function(x, state=list(ok=TRUE)) {
  val <- list(val=x, state=state)
  class(val) <- c('monad', class(val))
}

flift <- function(f) {
  function(x) {
    val <- f(x$val)
    vlift(val, x$state)
  }
}

