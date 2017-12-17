context("lookup")

test_that("'lookup' with scalar key works", {
    x <- framed(mtcars)
    i <- lookup(x, c("Valiant", "Fiat 128", "Delorean"))
    j <- match(c("Valiant", "Fiat 128", "Delorean"), rownames(x))
    expect_equal(i, j)              
})


test_that("'lookup' with NULL works", {
    x <- framed(mtcars)
    expect_equal(lookup(x, NULL), NULL)
})


test_that("'lookup' with single multi-component works", {
    x <- framed(mtcars)
    keys(x) <- dataset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))

    expect_equal(lookup(x, list("A", 5)), 5)
})


test_that("'lookup' with default works", {
    x <- framed(mtcars)
    keys(x) <- dataset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))

    expect_equal(lookup(x, list("ZZ", 5), default = 7), 7)
})


test_that("'lookup' with many multi-component works", {
    x <- framed(mtcars)
    keys(x) <- dataset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))

    i <- lookup(x, list(c("A", "A", "B", "Z"), c(  5, 100,   1,   3)))
    expect_equal(i, c(5, NA, 9, NA))
})


test_that("'lookup' with many wrong type works", {
    x <- framed(mtcars)
    keys(x) <- dataset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))

    i <- lookup(x, list(c("A", "A", "B", "Z"), c("5", "100", "1", "3")))
    expect_equal(i, c(5, NA, 9, NA))
})


test_that("'lookup' errors for invalid key", {
    x <- framed(mtcars)
    expect_error(lookup(x, NULL, c(-1, 0)), "'default' must have length 1")
    expect_error(lookup(x, NULL, NaN), "'default' cannot be NaN or Inf")
})
