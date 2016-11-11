context("hasard.R")

test_that("Testing type checkers", {
  d <- list(
    jes=1,
    jak=2,
    jen=3
  )
  class(d$jes) <- 'jes'
  class(d$jak) <- 'jak'
  class(d$jen) <- 'jen'

  foo <- function(x) { class(x) <- 'jes'; x}
  ftype(foo) <- c('*', 'jes')

  bar <- function(x) { class(x) <- 'jak'; x }
  ftype(bar) <- c('jes', 'jak')

  baz <- function(x) { class(x) <- 'jen'; x }
  ftype(baz) <- c('jak', 'jen')

  expect(are_composable(foo, bar), "are_composable works for positive case")
  expect(are_composable(bar, foo), "are_composable works for '*'")
  expect(!are_composable(baz, bar), "are_composable non-comutable if assymetric")
})
