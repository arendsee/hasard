context("is.R")

test_that(
  "is functions work",
  {
    foo <- function(){}
    class(foo) <- c('unary', 'hnode', 'validated', 'effectual', 'typed', class(foo))
    expect_true(is.hnode(foo))
    expect_true(is.unary(foo))
    expect_true(is.typed(foo))
    expect_true(is.validated(foo))
    expect_true(is.effectual(foo))

    bar <- function(){}
    expect_false(is.unary(bar))
    expect_false(is.hnode(bar))
    expect_false(is.validated(bar))
    expect_false(is.effectual(bar))
  }
)
