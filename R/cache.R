#' Cache functions
#'
#' @param op command for cache function
#' @param filename filename for cachers that store data in files
#' @param ... arguments sent to del, chk, put, and get within the cacher
#' @name cache_functions

#' @rdname cache_functions
#' @export
nocache <- function(op, ...) {
  switch(
    op,
    del = nothing(...),
    chk = false(...),
    put = nothing(...),
    get = nothing(...)
  )
}

#' @rdname cache_functions
#' @export
make_memcache <- function(){
  d <- NULL
  del_ <- function( ) d <<- NULL
  chk_ <- function( ) !is.null(d)
  put_ <- function(x) d <<- x
  get_ <- function( ) d
  function(op, ...){
    switch(
      op,
      del = del_(   ),
      chk = chk_(   ),
      put = put_(...),
      get = get_(   )
    )
  }
}

#' @rdname cache_functions
#' @export
make_datcache <- function(filename){
  del_ <- function( ) file.remove(filename)
  chk_ <- function( ) file.exists(filename)
  put_ <- function(x) save(x, file=filename)
  get_ <- function( ) {local({load(filename); get('x')})}
  function(op, ...){
    switch(
      op,
      del = del_(   ),
      chk = chk_(   ),
      put = put_(...),
      get = get_(   )
    )
  }
}
