
context("index")

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
    z <- framed(mtcars[i,],
                as_keyset(list(name = rownames(mtcars)[i],
                               "#" = c(1, 1, 1, 1, 2, 1))))
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


test_that("indexing without keys works", {
    x <- as_dataset(list(a = letters))
    expect_equal(as_dataset(x[1:5, , drop = FALSE]),
                 as_dataset(list(a = letters[1:5])))
})


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
