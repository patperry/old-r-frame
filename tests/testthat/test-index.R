
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


test_that("indexing with row number works", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], as_dataset(mtcars[i,]))
})


test_that("indexing with row number and 'drop' works", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i, , drop = FALSE], as_dataset(mtcars[i,]))
})


test_that("indexing with duplicates should not error if key is set", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19, 5, 7)
    y <- x[i,]
    z <- as_dataset(mtcars[i,])
    keys(z) <- as_dataset(list(name = rownames(mtcars)[i],
                               c(1, 1, 1, 1, 2, 1)))
    expect_equal(y, z)
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


test_that("indexing without keys works", {
    x <- as_dataset(list(a = letters))
    expect_equal(as_dataset(x[1:5,,drop = FALSE]),
                 as_dataset(list(a = letters[1:5])))
})
