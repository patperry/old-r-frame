
context("index")


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


