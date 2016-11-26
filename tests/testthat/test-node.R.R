context("node.R")

test_that(
  "hnode creates named defaults",
  {
    foo <- hnode('a -> b')
    expect_equal(htype(foo), c('a', 'b'))
    
    expect_equal(deparse(h_fun_ne(foo)),    "do_nothing")
    expect_equal(deparse(h_inode_ne(foo)),  "input_nothing")
    expect_equal(deparse(h_val_ne(foo)),    "true")
    expect_equal(deparse(h_pass_ne(foo)),   "execute")
    expect_equal(deparse(h_fail_ne(foo)),   "nothing")
    expect_equal(deparse(h_effect_ne(foo)), "nothing")
    expect_equal(deparse(h_cacher_ne(foo)), "nocache")
    expect_equal(eval(h_args(foo)), list())
    expect_equal(h_delete(foo), FALSE)

    bar <- hnode('NA -> b')
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

    foo <- hnode('NA -> a', f=one)
    bar <- hnode('a -> b', f=dbl, inode=foo)

    expect_equal(bar(), 2)

    # Below I use warning to see if right function was run

    h_val(bar)  <- false
    h_fail(bar) <- warn
    expect_warning(bar(), "'warn' function was called")

    bar <- hnode('a -> b', f=dbl, inode=foo)
    h_effect(bar) <- warn
    expect_warning(bar(), "'warn' function was called")

    bar <- hnode('a -> b', f=dbl, inode=foo)
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

    h0 <- hnode('NA->a',  f=f0)
    h1 <- hnode('a->b',   f=f1, inode=h0)
    h2 <- hnode('b->NA',  f=f2, inode=h1)

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

    h0 <- hnode('NA->a',        f=f0)
    h1 <- hnode('NA->b',        f=f1)
    h2 <- hnode('NA->c',        f=f2)
    h3 <- hnode('a->b->c->abc', f=f3, inode=list(h0,h1,h2))

    expect_equal(h3(), 'abc')
  }
)

test_that(
  "issue #2",
  {
    foo <- hnode('a->b->c')
    expect_null(h_fun(foo)())
  }
)
