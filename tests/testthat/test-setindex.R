context("setindex")

test_that("deleting all columns", {
    x <- as_dataset(mtcars)
    y <- mtcars
    x[] <- NULL
    y[] <- NULL
    expect_equal(x, as_dataset(y))
})


test_that("deleting columns with single index", {
    x <- as_dataset(mtcars)
    y <- mtcars
    x[c(4, 2, 4)] <- NULL
    y[c(4, 2)] <- NULL # errors on duplicates
    expect_equal(x, as_dataset(y))
})


test_that("deleting columns with single index and comma", {
    x <- as_dataset(mtcars)
    y <- mtcars
    x[,c(1, 1, 5)] <- NULL
    y[,c(1, 5)] <- NULL # errors on duplicates
    expect_equal(x, as_dataset(y))
})


test_that("setting single column scalar", {
    x <- as_dataset(mtcars)
    y <- mtcars
})
