context("node.R")

test_that(
  "Test hnode formals access",
  {
    foo <- function(
      .fun    = nothing,
      .inode  = nothing,
      .val    = true,
      .pass   = execute,
      .fail   = nothing,
      .effect = nothing,
      .delete = FALSE,
      .cacher = nocache,
      .args   = list(a=42)
    ){}

    expect_equal(deparse(h_fun(foo)),    "nothing")
    expect_equal(deparse(h_inode(foo)),  "nothing")
    expect_equal(deparse(h_val(foo)),    "true")
    expect_equal(deparse(h_pass(foo)),   "execute")
    expect_equal(deparse(h_fail(foo)),   "nothing")
    expect_equal(deparse(h_effect(foo)), "nothing")
    expect_equal(deparse(h_cacher(foo)), "nocache")
    expect_equal(eval(h_args(foo)), list(a = 42))
    expect_equal(h_delete(foo), FALSE)
  }
)

test_that(
  "hnode creates named defaults",
  {
    foo <- hnode('a -> b')
    expect_equal(htype(foo), c('a', 'b'))
    
    expect_equal(deparse(h_fun(foo)),    "do_nothing")
    expect_equal(deparse(h_inode(foo)),  "input_nothing")
    expect_equal(deparse(h_val(foo)),    "true")
    expect_equal(deparse(h_pass(foo)),   "execute")
    expect_equal(deparse(h_fail(foo)),   "nothing")
    expect_equal(deparse(h_effect(foo)), "nothing")
    expect_equal(deparse(h_cacher(foo)), "nocache")
    expect_equal(eval(h_args(foo)), list())
    expect_equal(h_delete(foo), FALSE)

    bar <- hnode('NA -> b')
    expect_equal(htype(bar), c(NA, 'b'))
    
    expect_equal(deparse(h_fun(bar)),    "nothing")
    expect_equal(deparse(h_effect(bar)), "nothing")
    expect_equal(deparse(h_cacher(bar)), "nocache")
    expect_equal(eval(h_args(bar)), list())
    expect_equal(h_delete(bar), FALSE)
  }
)

test_that(
  "Test hnode assignments work",
  {
    one <- function(){ 1 }
    bar <- function(x){}

    foo <- hnode('a -> b')
    # baz <- hnode('NA -> a', f=one)
    baz <- hnode('NA -> a')
    h_fun(baz) <- one

    expect_equal({h_fun(foo)    <- bar;   deparse(h_fun(foo))},    "bar")
    expect_equal({h_inode(foo)  <- baz;   deparse(h_inode(foo))},  "baz")
    expect_equal({h_val(foo)    <- false; deparse(h_val(foo))},    "false")
    expect_equal({h_pass(foo)   <- baz;   deparse(h_pass(foo))},   "baz")
    expect_equal({h_fail(foo)   <- baz;   deparse(h_fail(foo))},   "baz")
    expect_equal({h_effect(foo) <- baz;   deparse(h_effect(foo))}, "baz")
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
  "issue #1",
  {
    foo <- hnode('a->b')
    e <- environment(foo)
    expect_null(eval(h_fun(foo), e)())
  }
)

test_that(
  "issue #2",
  {
    foo <- hnode('a->b->c')
    e <- environment(foo)
    expect_null(eval(h_fun(foo), e)())
  }
)
