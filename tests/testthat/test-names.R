
context("names")

test_that("'rownames' returns NULL when absent", {
    x <- framed(mtcars, keys = integer())
    expect_equal(rownames(x), NULL)
})


test_that("'rownames<-' is allowd", {
    x <- framed(mtcars)
    rownames(x) <- seq_len(nrow(x))
    expect_equal(rownames(x), as.character(seq_len(nrow(x))))
    expect_equal(keys(x)[[1]], rownames(x))
})


test_that("'rownames<- NULL' is allowed", {
    x <- framed(mtcars)
    rownames(x) <- NULL
    expect_equal(x, framed(mtcars, key = character()))
})


test_that("'dimnames[[1]]<-' is allowed", {
    x <- framed(mtcars)

    rn <- seq_len(nrow(x))
    cn <- names(x)
    dimnames(x) <- list(rn, cn)
    expect_equal(rownames(x), as.character(rn))
    expect_equal(colnames(x), as.character(cn))
})


test_that("'dimnames[[1]]<-' NULL is allowed", {
    x <- framed(mtcars)
    dimnames(x) <- list(NULL, names(x))
    expect_equal(x, framed(mtcars, key = character()))
})

test_that("'dimnames<-' NULL is allowed", {
    x <- framed(mtcars)
    dimnames(x) <- NULL
    expect_equal(rownames(x), NULL)
    expect_equal(colnames(x), NULL)
})

test_that("'dimnames<-' fails for incorrect number of dimensions", {
    x <- framed(mtcars)
    expect_error(dimnames(x) <- list(NULL, names(x), NULL),
                 "'dimnames' length \\(3\\) must be 2")
})


test_that("'names' does not include key", {
    x <- framed(mtcars)
    expect_equal(names(x), names(mtcars))
})


test_that("'names<-' does not error if key already has name", {
    x <- framed(mtcars)
    nm <- c("name", names(x)[-1])
    names(x) <- nm
    expect_equal(names(x), nm)
})
