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

    expect_null(nothing(4))
    expect_null(nothing(NULL))
    expect_null(nothing(TRUE))
    expect_null(nothing(1,1,NULL))

    expect_equal(execute(paste, "a", "b"), "a b") 
  }
)
