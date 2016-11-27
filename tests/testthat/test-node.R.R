context("node.R")

test_that(
  "hnode creates named defaults",
  {
    foo <- hpipe('a -> b')
    expect_equal(htype(foo), c('a', 'b'))
    
    expect_equal(deparse(h_fun_ne(foo)),    "default_fun(type)")
    expect_equal(deparse(h_inode_ne(foo)),  "default_inode(type)")
    expect_equal(deparse(h_val_ne(foo)),    "true")
    expect_equal(deparse(h_pass_ne(foo)),   "execute")
    expect_equal(deparse(h_fail_ne(foo)),   "nothing")
    expect_equal(deparse(h_effect_ne(foo)), "nothing")
    expect_equal(deparse(h_cacher_ne(foo)), "nocache")
    expect_equal(eval(h_args(foo)), list())
    expect_equal(h_delete(foo), FALSE)

    bar <- hwell('b')
    expect_equal(htype(bar), c(NA, 'b'))

    expect_equal(deparse(h_fun_ne(bar)),    "nothing")
    expect_equal(deparse(h_effect_ne(bar)), "nothing")
    expect_equal(deparse(h_cacher_ne(bar)), "nocache")
    expect_equal(eval(h_args(bar)), list())
    expect_equal(h_delete(bar), FALSE)
  }
)

test_that(
  "hnode work",
  {

    one <- function(){1}
    dbl <- function(x){2*x}

    foo <- hwell('a')
    h_fun(foo) <- one

    bar <- hpipe('a -> b')
    h_fun(bar) <- dbl
    h_inode(bar) <- foo

    #delete
    bar()

    expect_equal(bar(), 2)

    # Below I use warning to see if right function was run

    h_val(bar)  <- false
    h_fail(bar) <- warn
    expect_warning(bar(), "'warn' function was called")

    h_val(bar) <- true
    h_effect(bar) <- warn
    expect_warning(bar(), "'warn' function was called")

    h_pass(bar) <- warn
    expect_warning(bar(), "'warn' function was called")
  }
)

test_that(
  "test linear pipeline",
  {

    f0 <- function(){'a'}
    f1 <- function(x){ paste0(x, 'b') }
    f2 <- function(x){ paste0(x, 'c') }

    h0 <- hwell('a')
    h1 <- hpipe('a->b')
    h2 <- hsink('b')

    h_fun(h0) <- f0
    h_fun(h1) <- f1
    h_fun(h2) <- f2

    h_inode(h1) <- h0
    h_inode(h2) <- h1

    expect_equal(h2(), 'abc')

  }
)

test_that(
  "test branching pipeline",
  {

    f0 <- function(){'a'}
    f1 <- function(){'b'}
    f2 <- function(){'c'}
    f3 <- function(a,b,c){ paste0(a,b,c) }

    h0 <- hwell('a')
    h1 <- hwell('b')
    h2 <- hwell('c')
    h3 <- hpipe('a->b->c->abc')

    h_fun(h0) <- f0
    h_fun(h1) <- f1
    h_fun(h2) <- f2
    h_fun(h3) <- f3

    h_inode(h3) <- list(h0,h1,h2)

    expect_equal(h3(), 'abc')
  }
)

test_that(
  "issue #2",
  {
    foo <- hpipe('a->b->c')
    expect_null(h_fun(foo)())
  }
)
