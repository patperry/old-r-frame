
context("index-entry")

test_that("indexing with matrix pairs works", {
    ds <- as_dataset(iris)

    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)

    x <- ds[cbind(i,j)]
    y <- mapply(function(i, j) ds[i,j,drop=TRUE], i, j, SIMPLIFY = FALSE)

    expect_equal(x, y)
})


test_that("indexing with matrix indices works", {
    ds <- as_dataset(iris)

    i <- c(47, 5, 132, NA, 10, 142, 143, 123)
    j <- c(1, 3, 3, NA, 1, 2, 2, 3)
    index <- cbind(i + nrow(ds) * (j - 1))

    x <- ds[index]
    y <- ds[cbind(i,j)]

    expect_equal(x, y)
})


test_that("indexing with logical matrix works", {
    ds <- as_dataset(iris)

    index <- matrix(FALSE, nrow(ds), ncol(ds))
    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
    index[cbind(i, j)] <- TRUE

    x <- ds[index]
    y <- ds[cbind(which(as.logical(index)))]
    expect_equal(x, y)
})


test_that("indexing with logical mask works", {
    ds <- as_dataset(iris)

    index <- matrix(FALSE, nrow(ds), ncol(ds))
    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
    index[cbind(i, j)] <- TRUE

    x <- ds[cbind(as.logical(index))]
    y <- ds[index]
    expect_equal(x, y)
})
