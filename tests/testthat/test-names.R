
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


test_that("'names<-' NULL errors", {
    x <- as_dataset(mtcars)
    expect_error(names(x) <- NULL, "setting 'names' to NULL is not allowed")
})


test_that("'names<-' with too many errors", {
    x <- as_dataset(mtcars)
    expect_error(names(x) <- c(names(x), "foo"),
        "'names' length \\(13\\) must match number of columns \\(12\\)")
})


test_that("'names<-' renames key", {
    x <- as_dataset(mtcars)
    nm <- names(x)
    nm[[1]] <- "newkey"
    names(x) <- nm
    expect_equal(names(x), nm)
    expect_equal(key(x), "newkey")
    expect_equal(rownames(x), rownames(mtcars))
})
