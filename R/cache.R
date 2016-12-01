#' Cache functions
#'
#' @param id unique id for the cache, if it will be the filename
#' @param directory directory where caches files are kept
#' @name cache_functions

#' @rdname cache_functions
#' @export
nocache <- function(id=NA) {
  del_ <- nothing
  chk_ <- false
  put_ <- nothing
  get_ <- nothing
  function(op, ...){
    switch(
      op,
      del = del_(),
      chk = chk_(),
      put = put_(),
      get = get_()
    )
  }
}

#' @rdname cache_functions
#' @export
memcache <- function(id=NA){
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
make_datcache <- function(directory){
  cacher <- function(id=NA){
    f <- file.path(directory, paste0(id, '.rdat'))
    del_ <- function(){
      if(file.exists(f)) file.remove(f)
    }
    chk_ <- function(){
      file.exists(f)
    }
    put_ <- function(x) {
      if(!dir.exists(directory)){
        dir.create(directory)
      }
      save(x, file=f)
    }
    get_ <- function(){
      local({load(f); get('x')})
    }
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
}
