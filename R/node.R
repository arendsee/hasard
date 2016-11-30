#' Build function
#'
#' @section Elements:
#'
#' \itemize{
#'   \item type function type
#'   \item f function
#'   \item inode input nodes
#'   \item val (a -> Bool) - determines in input is correct
#'   \item pass function called if val returns TRUE
#'   \item fail function called if val returns FALSE
#'   \item effect a function of a and b
#'   \item cacher the caching function
#' }
#'
#' @param type a functional type ala Haskell
#' @name node
NULL

# TODO: I need a formal method for handling loops, I've started implementing
# some junk below, but I don't like it. My basic approach is to define a well
# and sink, then for each well variant, recall the sink. Finally merge the list
# of sink outputs. The overall approach seems fine (it is a little tricky
# running loops across subgraphs), but there are recalcitrant details. How
# should I handle the cache? The effectors? Also, what about nodes that branch
# off nodes within the loop?

#' @rdname node
#' @export
hwell <- function(type, envir=parent.frame()){
  type <- parse_type(type, role='well')

  h <- function(
    .fun    = nothing,
    .effect = nothing,
    .cacher = nocache,
    .args   = list(),
    .id     = get_uid(),
    .clear  = TRUE,
    .envir  = envir
  ){

    # Get the concrete entities
    fun    <- eval(.fun,    .envir)
    effect <- eval(.effect, .envir)
    args   <- eval(.args,   .envir)
    cacher <- eval(.cacher, .envir)
    id     <- eval(.id,     .envir)
    cacher <- cacher(id)

    # Clear cache if needed
    if(.clear){
      cacher('del')
    }

    function(){
      if(!cacher('chk')){
        b <- do.call(fun, args)
        runall(effect, b)
        cacher('put', b)
      } else {
        b <- cacher('get')
      }
      b
    }
  }

  h <- add_class(h, 'hnode', 'well')
  htype(h) <- type
  h
}

#' @rdname node
#' @export
hpipe <- function(type, envir=parent.frame()){
  type <- parse_type(type)

  h <- function(
    .fun    = default_fun(type),
    .val    = true,
    .pass   = execute,
    .fail   = nothing,
    .effect = nothing,
    .cacher = nocache,
    .clear  = FALSE,
    .args   = list(),
    .id     = get_uid(),
    .envir  = envir
  ){

    # Get the concrete entities
    fun    <- eval(.fun,    .envir)
    val    <- eval(.val,    .envir)
    pass   <- eval(.pass,   .envir)
    fail   <- eval(.fail,   .envir)
    effect <- eval(.effect, .envir)
    cacher <- eval(.cacher, .envir)
    args   <- eval(.args,   .envir)
    id     <- eval(.id,     .envir)
    cacher <- cacher(id)

    # Clear cache if needed
    if(.clear){
      cacher('del')
    }

    function(.inode){
      if(cacher('chk')) return(cacher('get'))
      a <- runall(.inode)
      funlist <- append(fun, append(a, args))
      if(do.call(val, a)){
        b <- do.call(pass, funlist)
      } else {
        b <- do.call(fail, funlist)
      }
      runall(effect, b, h_input=a)
      cacher('put', b)
      b
    }
  }

  h <- add_class(h, 'hnode', 'pipe')
  htype(h) <- type
  h
}

#' @rdname node
#' @export
hsink <- function(type, envir=parent.frame()){
  type <- parse_type(type, 'sink')
  h <- hpipe(type, envir)
  h <- add_class(h, 'hnode', 'sink')
  h
}
