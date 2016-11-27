context("cache.R")

test_that(
  "test basic cache functions;",
  {
    c_mem <- make_memcache()
    c_dat <- make_datcache('test.Rdat')
    f_maker <- function() { x=1; function() {y<-x; x <<- x+1; y} }
    f1 <- f_maker()

    a <- hnode('NA->a', f=f1)
    expect_equal(a(), 1)
    expect_equal(a(), 2)

    h_cacher(a) <- c_mem
    expect_equal(a(), 3)
    expect_equal(a(), 3)

    h_cacher(a) <- c_dat
    expect_equal(a(), 4)
    expect_equal(a(), 4)

    expect_true(file.exists('test.Rdat'))
  }
)
