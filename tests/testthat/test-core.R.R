context("core.R")

# test_that(
#   "active node building functions work",
#   {
#     f <- mean
#     fm <- monify(mean)
#     fmt <- typify(fm, "[Num]", "Num")
#     fmtv <- valify(fmt)
#     fmtve <- effify(fmtv, function(a, b) {message("hi")})
#
#     expect_equal(class(fm), c('unary', 'function'))
#     expect_equal(class(fmt), c('typed', 'unary', 'function'))
#     expect_equal(class(fmtv), c('validated', 'typed', 'unary', 'function'))
#     expect_equal(class(fmtve), c('effectual', 'validated', 'typed', 'unary', 'function'))
#   }
# )

# test_that(
#   "compose works",
#   {
#     expect_equal(compose(mean, log2, round, as.character)(1:100), 40)
#   }
# )
