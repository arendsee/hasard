context("node_access.R")

test_that(
  "Test hnode formals access",
  {
    foo <- function(
      .fun    = nothing,
      .val    = true,
      .pass   = execute,
      .fail   = nothing,
      .effect = nothing,
      .delete = FALSE,
      .cacher = nocache,
      .args   = list(a=42)
    ){}

    expect_equal(deparse(h_fun_ne(foo)),    "nothing")
    expect_equal(deparse(h_val_ne(foo)),    "true")
    expect_equal(deparse(h_pass_ne(foo)),   "execute")
    expect_equal(deparse(h_fail_ne(foo)),   "nothing")
    expect_equal(deparse(h_effect_ne(foo)), "nothing")
    expect_equal(deparse(h_cacher_ne(foo)), "nocache")
    expect_equal(eval(h_args(foo)), list(a = 42))
    expect_equal(h_delete(foo), FALSE)
  }
)

test_that(
  "Test hnode assignments work",
  {
    one <- function(){ 1 }
    bar <- function(x){}

    foo <- hpipe('a -> b')

    baz <- hwell('a')
    h_fun(baz) <- one

    expect_equal({h_fun(foo)    <- bar;   deparse(h_fun_ne(foo))},    "bar")
    expect_equal({h_val(foo)    <- false; deparse(h_val_ne(foo))},    "false")
    expect_equal({h_pass(foo)   <- baz;   deparse(h_pass_ne(foo))},   "baz")
    expect_equal({h_fail(foo)   <- baz;   deparse(h_fail_ne(foo))},   "baz")
    expect_equal({h_effect(foo) <- baz;   deparse(h_effect_ne(foo))}, "baz")
  }
)

test_that(
  "check checkers",
  {
    a <- hwell('a')
    b <- hpipe('a->b')
    c <- hpipe('a->b->c')

    no_arg <- function(){}
    one_arg <- function(x){}
    two_arg <- function(x,y){}

    expect_error(h_fun(a) <- one_arg)
    expect_error(h_fun(b) <- no_arg)
    expect_error(h_fun(b) <- two_arg)
    expect_error(h_fun(b) <- 'hi')

    c1 <- c()
    expect_error(h_inode(c1) <- 1)
    expect_error(h_inode(c1) <- a)
    expect_error(h_inode(c1) <- list(a, 1))
    expect_error(h_inode(c1) <- list(a,a,a))
  }
)

test_that(
  "issue #1 - check all defaults",
  {
    foo <- hpipe('a->b')
    expect_null(h_fun(foo)())
    expect_true(h_val(foo)())
    expect_null(h_pass(foo)(h_fun(foo), 1))
    expect_null(h_fail(foo)())
    expect_null(h_effect(foo)())
  }
)

test_that(
  "issue #3 - assignment of anonymous functions",
  {
    foo <- hpipe('a->b')
    expect_equal({ h_fun(foo) <- function(x){1}; h_fun(foo)() }, 1)

    foo_name <- h_fun_ne(foo) %>%
      deparse %>% paste0(collapse="") %>%
      gsub(pattern=" ", replacement="")

    expect_equal(foo_name, 'function(x){1}')
  }
)
