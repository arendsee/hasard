context("print.R")

test_that(
  "Unary printing works", {
    foo <- monify(mean) 
    foo <- typify(foo, 'Foo', 'Bar')
    
    expect_equal(capture.output(print(foo)), '[1] "foo :: Foo -> Bar"')
  }
)
