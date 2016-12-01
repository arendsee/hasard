context("atom.R")

test_that(
  "Basic functions work",
  {
    expect_true(true(4))
    expect_true(true(4, "u"))
    expect_true(true(FALSE))

    expect_false(false(4))
    expect_false(false(4, "u"))
    expect_false(false(TRUE))

    expect_null(nothing())
    expect_null(nothing(4))
    expect_null(nothing(NULL))
    expect_null(nothing(TRUE))
    expect_null(nothing(1,1,NULL))

    expect_error(fail())
    expect_error(fail(1))
    expect_error(fail(1,1))

    expect_warning(warn())
    expect_warning(warn(1))
    expect_warning(warn(1,1))

    expect_null(blank())
    expect_error(blank(1))

    expect_equal(id('a'), 'a')
    expect_equal(id('a', 'b'), 'a')

    expect_equal(execute(paste, "a", "b"), "a b") 
    expect_error(execute('a')) 
  }
)
