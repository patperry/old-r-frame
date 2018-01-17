
context("index-select")

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
    expect_error(x[j], "only 0's may be mixed with negative subscripts")
})


test_that("indexing with out of bounds positive fails", {
    x <- as_dataset(mtcars)
    j <- c(5, 3, 2, 100, 1)
    expect_error(x[j], "column selection entry 4 is out of bounds")
})


test_that("indexing with NA fails", {
    x <- as_dataset(mtcars)
    j <- c(seq_along(mtcars)[-1], NA)
    expect_error(x[j], "column selection entry 11 is NA")
})


test_that("indexing with column logical works", {
    x <- as_dataset(mtcars)
    j <- rep(c(TRUE, FALSE), length(mtcars))[seq_along(mtcars)]
    expect_equal(x[j], as_dataset(mtcars[j]))
})


test_that("indexing with NA column logical errors", {
    x <- as_dataset(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1), NA)
    expect_error(x[j], "column selection entry 11 is NA")
})


test_that("indexing with wrong number of logical errors", {
    x <- as_dataset(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1))
    expect_error(x[j],
        "selection mask length \\(10\\) must equal number of columns \\(11\\)")
})
