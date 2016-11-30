#' pied: A typed monadic pipeline
#'
#' \code{pied} formalizes and simplifies workflow design by adding types to
#' functions and separating pure code from validation, caching, logging, error
#' handling.
#'
#' A large research project often includes finding and cleaning many types of
#' data; merging and transforming them, along highly branched pipelines, to get
#' final results; validating input data at each step; printing and plotting
#' intermediate data throughout the pipeline; caching of intermediate and final
#' results; gracefully handling errors; and presenting the final results.  Even
#' a simple pipeline, where the pure data transformations require only a few
#' dozen lines, quickly bloats to pages when the above conditions are met.
#'
#' \code{pied} is designed to simplify this process by cleanly separating pure
#' data transformations from validation, caching, effects, and error handling.
#' The flow of data between functions is formalized with flow language.
#' Initalizing each function with a type signature (an idea borrowed from
#' Haskell), allows reasoning about the pipeline, and indeed type checking it,
#' before writing the first line of code.
#'
#' @section Main functions:
#'
#' Functions for creating new functions
#' \itemize{
#'   \item hwell
#'   \item hpipe
#'   \item hsink
#' }
#'
#' @docType package
#' @name pied
NULL

#' @include atom.R
#' @include is.R
#' @include util.R
#' @include type.R 
#' @include node_access.R
#' @include node.R
#' @include make.R 
#' @include flow.R
#' importFrom methods "formalArgs"
#' importFrom magrittr "%>%"
NULL

# so R doesn't complain about magrittr symbols
utils::globalVariables(c("%>%", "."))
NULL
