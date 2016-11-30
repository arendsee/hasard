#' Build function
#'
#' @section Arguments:
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

#' @rdname node
#' @export
hwell <- function(type){
  type <- parse_type(type, role='well')

  h <- function(
    .fun    = nothing,
    .effect = nothing,
    .cacher = nocache,
    .delete = FALSE,
    .args   = list()
  ){
    if(.delete){ .cacher('del') }
    if(!.cacher('chk')){
      b <- do.call(.fun, .args)
      runall(.effect, b)
      .cacher('put', b)
    } else {
      b <- .cacher('get')
    }
    b
  }

  h <- add_class(h, 'hnode', 'well')
  htype(h) <- type
  parent.env(environment(h)) <- parent.frame()
  h
}

#' @rdname node
#' @export
hpipe <- function(type){
  type <- parse_type(type)

  h <- function(
    .fun    = default_fun(type),
    .inode  = default_inode(type),
    .val    = true,
    .pass   = execute,
    .fail   = nothing,
    .effect = nothing,
    .cacher = nocache,
    .delete = FALSE,
    .args   = list()
  ){
    if(.delete) .cacher('del')
    if(.cacher('chk')) return(.cacher('get'))
    a <- runall(.inode)
    funlist <- append(.fun, append(a, .args))
    if(do.call(.val, a)){
      b <- do.call(.pass, funlist)
    } else {
      b <- do.call(.fail, funlist)
    }
    runall(.effect, b, h_input=a)
    .cacher('put', b)
    b
  }

  h <- add_class(h, 'hnode', 'pipe')
  htype(h) <- type
  parent.env(environment(h)) <- parent.frame()
  h
}

#' @rdname node
#' @export
hsink <- function(type){
  type <- parse_type(type, 'sink')
  h <- hpipe(type)
  h <- add_class(h, 'hnode', 'sink')
  parent.env(environment(h)) <- parent.frame()
  h
}


# TODO: I need a formal method for handling loops, I've started implementing
# some junk below, but I don't like it. My basic approach is to define a well
# and sink, then for each well variant, recall the sink. Finally merge the list
# of sink outputs. The overall approach seems fine (it is a little tricky
# running loops across subgraphs), but there are recalcitrant details. How
# should I handle the cache? The effectors? Also, what about nodes that branch
# off nodes within the loop? Anyway, below are the bones of my first attack:
#
# make_split_fun <- function(variant, field, values, sink){
#   fun <- function(){}
#   body(fun) <- substitute(
#     {
#       i <- 1
#       run_one <- function(v) {
#         h_args(variant)[[field]] <- v
#         h_cachid(variant) <- i
#         i <- i + 1
#         sink()
#       }
#       lapply(values, run_one)
#     }
#   )
#   parent.env(environment(fun)) <- parent.frame()
#   fun
# }
#
# hloop <- function(flow, split_fun, ..., merge_fun=id){
#
#   flow <- parse_flow(flow)
#
#   # assert variant is a well in flow
#   # assert there is only one sink
#
#   h <- function(){
#
#   }
#
# }
