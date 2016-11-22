context("util.R")

test_that(
  "Warn and error work",
  {
    foo <- function(x, y, z=1){ 1 }
    bar <- function(x, y=1, z){ 2 }

    expect_equal(npositional(foo), 2)
    expect_equal(nformals(foo), 3)
    expect_equal(npositional(bar), 2)
    expect_equal(nformals(bar), 3)

    expect_equal(npositional(unify(foo, y=1, z=2)), 1) 
    expect_equal(nformals(unify(foo, y=1, z=2)), 1) 

    # runall should return a list with the output of each function
    expect_equal(unlist(runall(list(foo, bar))), c(1, 2)) 
    # if there is only one effect, and it is not a list, run it directly
    expect_equal(unlist(runall(bar)), 2) 
    # error if input is not a function
    expect_error(runall(1)) 

    # can check class of multiple objects
    expect_true(classcheck('function', foo, bar))

    # no worries if the class is not the first
    a = 1
    expect_error(classcheck(42, a))
    class(a) <- c('a', 'b', 'c')
    expect_true(classcheck('c', a))

    a <- add_class(a, 'd', 'e')
    expect_true(classcheck('e', a))

    expect_true(classcheck('olga', add_class(a, 'olga')))
    # each class must appear only once
    expect_true(sum('e' %in% class(add_class(a, 'e', 'e'))) == 1)
  }
)
