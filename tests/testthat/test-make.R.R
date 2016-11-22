context("make.R")

test_that(
  "Check validator",
  {
    expect_true(is.function(make_validator('a->Bool')))
    expect_true(is.function(make_effector('a->b')))
    expect_true(is.function(make_cacher()))

    expect_warning(make_validator('a->b'))
  }
)
