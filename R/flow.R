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

#' Connect nodes into a pipeline
#'
#' Pathways are expressed using a simple language. Given nodes A, B, C, and D:
#'
#'  * 'A' - performs no operations
#'  * 'A --> B' := `h_inode(B) <- A` in the parent frame
#'
#' Multiple inputs are space delimited
#'
#'  * 'A B --> C' := `h_inode(C) <- list(A, B)`
#'
#' Any number of nodes can be chained
#'
#'  * 'A --> B --> C' := `h_inode(B) <- A; h_inode(C) <- B`
#'
#' Parentheses can be used to denote branching pathways
#'
#'  * '(A --> B) (C --> D) E'
#'    1. `h_inode(B) <- A`
#'    2. `h_inode(D) <- C`
#'    3. `h_inode(E) <- list(B, D)`
#'
#' @param x string describing functional pathway
#' @examples
#' library(pied)
#' library(magrittr)
#' A <- hwell('a')
#' B <- hpipe('a -> b')
#' C <- hpipe('b -> c')
#' D <- hpipe('b -> c -> d')
#'
#' h_fun(A) <- function() 'a'
#' h_fun(B) <- function(x) paste0(x, 'b')
#' h_fun(C) <- function(x) paste0(x, 'c')
#' h_fun(D) <- function(x,y) sprintf('(%s)(%s)d', x, y)
#'
#' connect('(A --> B) (A --> B --> C) --> D')
#' @export

connect <- function(x){

  parent <- parent.frame()
  child <- new.env()
  parent.env(e) <- parent

  NAME <- '[\\w.]+'

  terms <- keep_split(x, NAME)[[1]]
  name_ids <- stringr::str_detect(terms, NAME)

  with( child,
    {
      nodes <- lapply(terms[name_ids], function(x) get(x, envir=parent)())
    }
  )

  node_names <- paste0(terms[name_ids], '__', sapply(child$nodes, attr, 'id'))
  terms[name_ids] <- node_names

  with( child,
    {
      names(nodes) <- node_names
      for(i in 1:length(nodes)){
        assign(names(nodes)[i], nodes[[i]])
      }
      rm(nodes, i)
    }
  )

  x <- paste(terms, collapse='')
  while(!stringr::str_detect(x, '^[\\w.]+$')){
    original <- x
    link <- stringr::str_extract(x, '[\\w. ]+\\s*-->\\s*[\\w.]+')[1]
    x <- stringr::str_replace(x, '[\\w. ]+\\s*-->\\s*([\\w.]+)', '\\1')
    x <- stringr::str_replace(x, '\\(\\s*([\\w.]+)\\s*\\)', '\\1')
    if(original == x || link == "" ){
      stop('Malformed expression, could not parse')
    }
    p <- stringr::str_replace(link, '-->', '')
    p <- stringr::str_split(p, '\\s+')[[1]]
    N <- length(p)
    o <- p[N]
    i <- p[-N]
    if(N >= 2){
      cmd_str <- 'h_inode(%s) <- list(%s)'
      cmd_str <- sprintf(cmd_str, o, paste(i, collapse=', '))
      with(child, eval(parse(text=cmd_str)))
    } else {
      stop(sprintf("Expected N >= 2, but found N=%s for p='%s'", N, p))
    }
  }

  child
}
