context("util.R")
context("access.R")

test_that(
  "Type and class functions work",
  {
    foo <- function(x) { }
    expect_warning(ip(foo) <- 'a')
    foo <- function(x) { }
    expect_warning(op(foo) <- 'a')
    ip(foo) <- 'b'
    expect_equal(ip(foo), 'b')
    op(foo) <- 'c'
    expect_equal(op(foo), 'c')

    htype(foo) <- c('*', 'jes')
    class(foo) <- c('unary', 'function')

    bar <- function(x) {  }
    htype(bar) <- c('jes', 'jak')
    class(bar) <- c('unary', 'function')


    baz <- function(x) { }
    htype(baz) <- c('jak', 'jen')
    class(baz) <- c('unary', 'function')

    bad <- function(x, y) { }
    htype(bad) <- c('jak', 'jen', 'gracy')
    class(bad) <- c('multi', 'function')

    expect_equal(htype(foo), c('*', 'jes'))
    expect_equal(ip(foo), '*')
    expect_equal(op(foo), 'jes')


    expect_equal(npositional(foo), 1)
    expect_equal(npositional(bad), 2)
    expect_equal(npositional(mean), 1)

    expect_warning(htype(bad) <- c('jak', 'jen', 'gracy', 'dave'))

    expect_equal(
      class(add_class(mean, "a")),
      c("a", "function"),
      "expect_equal - base case"
    )
    expect_equal(
      class(add_class(mean, "a", "b")),
      c("b", "a", "function"),
      "expect_equal - add multiple classes"
    )
    expect_equal(
      class(add_class(baz, "unary")),
      c("unary", "function"),
      "expect_equal - don't repeat classes"
    )

    expect_equal(nhtypes(baz), 2)

    expect_equal(htype(baz), attr(baz, "htype"))

    expect_true(classcheck('unary', foo, bar, baz))
    expect_false(classcheck('unary', foo, bar, baz, mean))

    expect_true(are_composable(foo, bar), "are_composable works for positive case")
    expect_true(are_composable(bar, foo), "are_composable works for '*'")
    expect_false(are_composable(baz, bar), "are_composable non-comutable if assymetric")
  }
)

test_that(
  "Warn and error work",
  {
    expect_warning(warn('ladida'))
    expect_error(error('ladida'))
  }
)
