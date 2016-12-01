context("cache.R")

test_that(
  "test basic cache functions;",
  {
    f_maker <- function() { x=1; function() {y<-x; x <<- x+1; y} }
    f1 <- f_maker()

    a <- hwell('a')
    h_fun(a) <- f1
    a1 <- a()
    
    expect_equal(a1(), 1)
    expect_equal(a1(), 2)

    h_cacher(a) <- memcache
    a2 <- a()

    expect_equal(a2(), 3)
    expect_equal(a2(), 3)

    datcache <- make_datcache('zzz')
    h_cacher(a) <- datcache
    a3 <- a()
    expect_equal(a3(), 4)
    expect_equal(a3(), 4)

    expected_file <- file.path('zzz', paste0(attributes(a3)$id, '.rdat'))
    expect_true(file.exists(expected_file))

    # clean up cached files
    file.remove(list.files('zzz', full.names=TRUE))
    file.remove('zzz')
  }
)

test_that(
  "nocache",
  {
    cacher <- nocache()
    expect_null(cacher('del'))
    expect_null(cacher('put'))
    expect_null(cacher('get'))
    expect_false(cacher('chk'))
  }
)

test_that(
  "issue #4",
  {
    f_maker <- function() { x=1; function() {y<-x; x <<- x+1; y} }
    foo <- f_maker()
    a <- hwell('a')
    h_fun(a) <- foo
    h_cacher(a) <- memcache
    a1 <- a()

    expect_equal(a1(), 1)
    expect_equal(a1(), 1)
  }
)
