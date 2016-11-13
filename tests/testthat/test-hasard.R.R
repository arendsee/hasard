context("hasard.R")

test_that("Testing type checkers", {

  foo <- function(x) { }
  htype(foo) <- c('*', 'jes')
  class(foo) <- c('unary', 'function')

  bar <- function(x) {  }
  htype(bar) <- c('jes', 'jak')
  class(bar) <- c('unary', 'function')

  baz <- function(x) { }
  htype(baz) <- c('jak', 'jen')
  class(baz) <- c('unary', 'function')

  expect(are_composable(foo, bar), "are_composable works for positive case")
  expect(are_composable(bar, foo), "are_composable works for '*'")
  expect(!are_composable(baz, bar), "are_composable non-comutable if assymetric")
})
