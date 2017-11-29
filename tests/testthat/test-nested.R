
context("nested")

test_that("'dataset' can be nested", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(dim(x), c(5, 2))
    expect_equal(names(x), c("a", "b"))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(dim(y), c(5, 3))
    expect_equal(names(y), c("a", "b", "c"))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(dim(z), c(5, 2))
    expect_equal(names(z), c("a", "b"))
})
