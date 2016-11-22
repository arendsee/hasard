context("flow.R")

test_that(
  "Compositions",
  {
    f <- function( ) 'f'          
    g <- function(x) paste(x,'g') 
    h <- function(x) paste(x,'h') 

    expect_equal(compose(f,g,h)(), 'f g h')

    htype(f) <- 'NA -> a'
    htype(g) <- 'a  -> b'
    htype(h) <- 'b  -> NA'

    expect_true(are_composable(f,g))
    expect_true(are_composable(g,h))
    expect_false(are_composable(f,h))
    expect_false(are_composable(h,f))

    expect_null(compose())
    expect_true(is.function(compose(f)))
    expect_equal(htype(compose(f,g)), c(NA, 'b'))
    expect_true(all(is.na(htype(compose(f,g,h)))))

    expect_error(compose(h,f))
    expect_error(compose(f,f))
    expect_error(compose(g,f))
  }
)
