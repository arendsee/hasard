context("type.R")

test_that(
  "assignments work",
  {
    foo <- function(){}
    expect_equal({htype(foo) <- 'NA->a'; htype(foo)}, c(NA,'a'))
    expect_true({htype(foo) <- 'NA->a'; is.na(ip(foo))})
    expect_equal({htype(foo) <- 'NA->a'; op(foo)}, 'a')

    htype(foo) <- 'a->b'
    expect_equal({ip(foo) <- 'c'; htype(foo)}, c('c', 'b'))
    expect_equal({op(foo) <- 'd'; htype(foo)}, c('c', 'd'))

    expect_null({htype(foo) <- NULL; htype(foo)})

    htype(foo) <- NULL
    expect_warning(ip(foo) <- 'c')
    htype(foo) <- NULL
    expect_warning(op(foo) <- 'd')
  }
)

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
    expect_equal(parse_type(foo), c('a', 'b', 'c'))
    expect_error(parse_type(1))
  }
)

test_that(
  "NA is created for wells and sinks",
  {
    expect_equal(parse_type('a', role='well'), c(NA, 'a'))
    expect_equal(parse_type('a', role='sink'), c('a', NA))
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

    expect_warning(nhargs(baz), NULL)
    expect_equal(nhargs(c('a', 'b', 'c')), 2)
    expect_equal(nhargs(c(NA, 'c')), 0)
    expect_equal(nhargs(bar), 0)
    expect_error(nhargs(1))
  }
)
