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
#' library(magrittr)
#' compose(runif, mean, abs, log)
#' @export

compose <- function(...){
  
  funcs <- list(...)

  N <- length(funcs)

  if(N == 0) return(NULL)

  inner <- funcs[[1]]
  outer <- rev(funcs)[[1]]

  if(N == 1) return(inner)

  for(i in 2:N){
    if(!are_composable(funcs[[i-1]], funcs[[i]])){
      stop(sprintf("Arguments %d and %d are not composable", i-1, i))
    }
  }

  # names of all functions, innermost to outermost
  fun_names  <- sapply(match.call(expand.dots=TRUE)[-1], deparse)
  # paramters for innermost function as a string
  inner_args <- methods::formalArgs(inner) %>% paste0(collapse=', ')
  # function for recursive wrapping of calls
  compose_   <- function(f, g){ sprintf("%s(%s)", g, f) }
  # functional body as an expression
  body_expr <- Reduce(fun_names, f=compose_, init=inner_args) %>%
    sprintf(fmt="{%s}") %>%
    {parse(text=.)}

  fun              <- blank
  body(fun)        <- body_expr
  formals(fun)     <- formals(inner)
  environment(fun) <- parent.frame()

  if(is.typed(inner) && is.typed(outer)){
    htype(fun) <- c(ip(inner), op(outer))
  }

  fun
}
