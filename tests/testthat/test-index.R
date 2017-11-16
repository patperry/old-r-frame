
context("index")

test_that("indexing with column number works", {
    x <- as_dataset(mtcars)
    j <- c(5, 3, 0, 7)
    expect_equal(x[j], as_dataset(mtcars[j]))
})


test_that("indexing with negative column number works", {
    x <- as_dataset(mtcars)
    j <- -c(5, 3, 0, 7)
    expect_equal(x[j], as_dataset(mtcars[j]))
})


test_that("indexing with mixed sign fails", {
    x <- as_dataset(mtcars)
    j <- c(-5, -3, 0, 7)
    expect_error(x[j],
                 "subscript contains both positive and with negative values")
})


test_that("indexing with out of bounds positive fails", {
    x <- as_dataset(mtcars)
    j <- c(5, 3, 2, 100, 1)
    expect_error(x[j], "subscript entry 4 is out of bounds")
})


test_that("indexing with out of bounds negative fails", {
    x <- as_dataset(mtcars)
    j <- c(-5, -3, -100, -2)
    expect_error(x[j], "subscript entry 3 is out of bounds")
})


test_that("indexing with NA fails", {
    x <- as_dataset(mtcars)
    j <- c(seq_along(mtcars)[-1], NA)
    expect_error(x[j], "subscript entry 11 is NA")
})


test_that("indexing with column logical works", {
    x <- as_dataset(mtcars)
    j <- rep(c(TRUE, FALSE), length(mtcars))[seq_along(mtcars)]
    expect_equal(x[j], as_dataset(mtcars[j]))
})


test_that("indexing with NA column logical errors", {
    x <- as_dataset(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1), NA)
    expect_error(x[j], "logical subscript entry 11 is NA")
})


test_that("indexing with wrong number of logical errors", {
    x <- as_dataset(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1))
    expect_error(x[j],
        "logical subscript length \\(10\\) must equal number of column \\(11\\)")
})


test_that("indexing with row number works", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], as_dataset(mtcars[i,]))
})


test_that("indexing with duplicates errors if key is set", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19, 5, 7)
    expect_error(x[i,],
                 "subset contains duplicate row \\(5\\) but key is non-NULL")
})


test_that("indexing with row names works", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], x[rownames(x)[i],])
})


test_that("indexing with column number works", {
    x <- as_dataset(mtcars)
    j <- c(5, 3, 7)
    expect_equal(x[,j], as_dataset(mtcars[,j]))
})


test_that("'length' does not include key columns", {
    expect_equal(length(as_dataset(mtcars)), length(mtcars))
})
