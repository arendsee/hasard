context("core.R")

test_that(
  "active node building functions work",
  {
    f <- mean
    fm <- monify(mean)
    fmt <- typify(fm, "[Num]", "Num")
    fmtv <- valify(fmt)
    fmtve <- effify(fmtv, function(a, b) {message("hi")})

    expect_equal(class(fm), c('unary', 'function'))
    expect_equal(class(fmt), c('typed', 'unary', 'function'))
    expect_equal(class(fmtv), c('validated', 'typed', 'unary', 'function'))
    expect_equal(class(fmtve), c('effectual', 'validated', 'typed', 'unary', 'function'))
  }
)

# test_that(
#   "compose works",
#   {
#     expect_equal(compose(mean, log2, round, as.character)(1:100), 40)
#   }
# )


# #' Create a new validator
# #'
# #' @param f function with type: f :: vclass -> logical
# #' @param vclass the input class
# #' @return a unary, validator function
# #' @export
# make_validator <- function(f, vclass){
#   fun <- function(x) {
#     f(x)
#   }
#   fun <- add_class(fun, 'validator', 'unary')
#   htype(fun) <- c(vclass, 'Bool')
#   fun
# }
#
#
# #' Make composition of functions
# #'
# #' @param ... two or more functions
# #' @return A function that is a composition of the input functions
# compose <- function(...){
#   if(classcheck('typed', ...)){
#     compose_ <- function(f, g) {
#       if(! are_composable(f, g) ){
#         msg <- "Illegal composition of (%s -> %s) and (%s -> %s)"
#         error(msg, htype(f)[1], htype(f)[2], htype(g)[1], htype(g)[2],)
#       }
#       fun <- function(x) {
#         g(f(x))
#       }
#       htype(fun) <- c(htype(f)[1], htype(g)[2])
#       if(!classcheck('validated', f)){
#         fun$ival <- true
#       } else {
#         fun$ival <- f$ival
#       }
#       if(!classcheck('validated', g)){
#         fun$oval <- true
#       } else {
#         fun$oval <- g$oval
#       }
#       class(fun) <- c("typed", "unary", "validated", "function")
#       fun
#     }
#   } else {
#     compose_ <- function(f, g) {
#       function(.) { g(f(.)) }
#     }
#   }
#   Reduce(compose_, list(...))
# }
#
# #' A function that recusively determines class from an object
# #'
# #' @param . any thing
# #' @return character a class definition
# rclass <- compose(
#   monify(lapply, function(x) ifelse(is.list(x), list_class(x), class(x)[1])),
#   monify(unlist),
#   monify(paste0, collapse=", "),
#   monify(sub, pattern="(.*,.*)", replace="(\\1)")
# )
#
# #' Build a tuple (based on a list) from arguments
# #'
# #' @param ... data to include in the output tuple
# #' @return a list with 'tuple' class
# #' @export
# tuplify <- function(...) {
#   tuple <- list(...)
#   htype(tuple) <- rclass(tuple)
#   tuple <- add_class(tuple, 'tuple')
#   tuple
# }
#
# #' Extract a subset from a tuple
# #'
# #' @param x a tuple (or list)
# #' @param i indices to extract from x
# #' @return a subset of x
# #' @export
# parsubset <- function(x, i){
#   if(! all(i %in% 1:length(x))){
#     stop(sprintf("Invalid indices, select from 1-%s\n", length(x)))
#   }
#   tuple <- x[i]
#   htype(tuple) <- rclass(tuple)
#   tuple <- add_class(tuple, 'tuple')
#   tuple
# }
