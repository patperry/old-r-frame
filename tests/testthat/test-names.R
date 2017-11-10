
context("names")

test_that("'rownames' returns NULL when absent", {
    x <- as_dataset(mtcars)
    expect_equal(rownames(x), NULL)
})


test_that("'rownames<-' errors", {
    x <- as_dataset(mtcars)
    expect_error(rownames(x) <- seq_len(nrow(x)),
                 "setting row names is not allowed for dataset objects")
})


test_that("'rownames<- NULL' is allowed", {
    x <- as_dataset(mtcars)
    rownames(x) <- NULL
    expect_equal(x, as_dataset(mtcars))
})


test_that("'dimnames[[1]]<-' errors", {
    x <- as_dataset(mtcars)
    expect_error(dimnames(x) <- list(seq_len(nrow(x)), names(x)),
                 "setting row names is not allowed for dataset objects")
})


test_that("'dimnames[[1]]<-' NULL is allowed", {
    x <- as_dataset(mtcars)
    dimnames(x) <- list(NULL, names(x))
    expect_equal(x, as_dataset(mtcars))
})
