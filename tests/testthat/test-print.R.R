context("print.R")

test_that(
  "Typed printing works", {

    foo <- function(x) { x }
    htype(foo) <- c('Foo', 'Bar')
    
    expect_equal(capture.output(print(foo)), '[1] "foo :: Foo -> Bar"')
  }
)
