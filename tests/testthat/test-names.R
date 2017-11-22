
context("names")

test_that("'rownames' returns NULL when absent", {
    x <- as_dataset(mtcars, rownames = NULL)
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


test_that("'dimnames<-' fails for incorrect number of dimensions", {
    x <- as_dataset(mtcars)
    expect_error(dimnames(x) <- NULL,
                 "setting 'dimnames' to NULL is not allowed")
    expect_error(dimnames(x) <- list(NULL, names(x), NULL),
                 "'dimnames' length \\(3\\) must be 2")
})


test_that("'names' does not include key", {
    x <- as_dataset(mtcars)
    expect_equal(names(x), names(mtcars))
})


test_that("'names<-' does not error if key already has name", {
    x <- as_dataset(mtcars)
    nm <- c("name", names(x)[-1])
    names(x) <- nm
    expect_equal(names(x), nm)
})
