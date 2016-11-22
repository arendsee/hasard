#' Check if two functions are composable
#'
#' Inputs are considered to be composable if they are functions and are either
#' untyped or have composable types. Typed functions (f,g) are composable if 1)
#' f's output is not NA (i.e. is not a sink), 2) g's input is not NA (i.e. is
#' not a source), and 3) f's output equals g's input.
#' 
#' @param f,g unary class functions
#' @return logical
#' @export
are_composable <- function(f, g){
  are_functions <- is.function(f) && is.function(g)

  if(!are_functions){
    return(FALSE)
  }

  untyped <- !classcheck('typed', f, g)

  if(untyped){
    return(TRUE) 
  }

  f_has_output <- !is.na(op(f))
  f_output_matches_g_input <- (op(f) == ip(g)) || (ip(g) == "*")

  return(f_has_output && f_output_matches_g_input)
}

#' Make composition of functions
#'
#' Input functions should be ordered from innermost to outtermost. All
#' arguments are passed to the innermost function, whose formal arguments are
#' transferred exactly to the composed function.
#'
#' The composed function is a member of the environment in which `compose` was
#' called. The composition does not copy the constituent functions, but rather
#' searches for them in the parent frame. So, given `foo <- compose(f,g,h)`
#' calling `foo()` is exactly equivalent to calling `h(g(f))()` in foo's
#' parental environment.
#'
#' @param ... two or more functions
#' @return A function that is a composition of the input functions
#' @examples
#' compose(runif, mean, abs, log)
#' @export
compose <- function(...){
  fnames <- sapply(match.call(expand.dots=TRUE)[-1], deparse)
  funcs <- lapply(fnames, function(s) eval(parse(text=s)))

  if(length(funcs) == 0){
    return(NULL)
  }

  if(length(funcs) == 1){
    return(funcs[[1]])
  }

  for(i in 1:(length(funcs)-1)){
    if(!are_composable(funcs[[i]], funcs[[i+1]])){
      stop(sprintf("Arguments %d and %d are not composable", i, i+1))
    }
  }

  inner <- funcs[[1]]
  outer <- rev(funcs)[[1]]

  fpass <- formalArgs(inner) %>% paste0(collapse=", ")

  fun <- blank
  formals(fun) <- formals(inner)

  compose_ <- function(a, b){
    sprintf('%s(%s)', a, b) 
  }

  body(fun) <- Reduce(f=compose_, x=rev(fnames), init=fpass, right=TRUE) %>%
    {parse(text=sprintf("{%s}", .))}

  environment(fun) <- parent.frame()

  if(is.typed(inner) && is.typed(outer)){
    htype(fun) <- c(ip(inner), op(outer))
  }

  fun
}
