context("type.R")

test_that(
  "Type parsing and stringification works",
  {
    foo <- function(){}
    attributes(foo)$htype <- c('a', 'b', 'c')
    bar <- function(){}
    attributes(bar)$htype <- c(NA, 'd')

    expect_equal(type_str(foo), '(a -> b -> c)')
    expect_equal(type_str(bar), '(NA -> d)')

    expect_equal(parse_type(c('a', 'b')), c('a', 'b'))
    expect_equal(parse_type(c(NA, 'b')), c(NA, 'b'))
    expect_equal(parse_type('a -> b'), c('a', 'b'))
    expect_equal(parse_type('NA -> b'), c(NA, 'b'))
  }
)

test_that(
  "Type assignment works",
  {
    foo <- function(){}
    htype(foo) <- c('a', 'b', 'c')

    expect_equal(attributes(foo)$htype, c('a', 'b', 'c'))
    expect_equal(ip(foo), c('a', 'b'))
    expect_equal(op(foo), 'c')
    expect_equal(nhargs(foo), 2)

    bar <- function(){}
    htype(bar) <- c(NA, 'd')
    expect_true(is.na(ip(bar)))
    expect_equal(op(bar), 'd')
    expect_equal(nhargs(bar), 0)

    baz <- function(){}
    expect_null(htype(baz))
    expect_null(ip(baz))
    expect_null(op(baz))
    expect_warning(nhargs(baz))

    expect_equal(nhargs(c('a', 'b', 'c')), 2)
    expect_equal(nhargs(c(NA, 'c')), 0)
    expect_error(nhargs(1))
  }
)
