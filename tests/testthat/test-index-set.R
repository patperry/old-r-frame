context("index-set")

test_that("deleting all columns", {
    x <- as_dataset(mtcars)
    y <- mtcars
    x[] <- NULL
    y[] <- NULL
    expect_equal(x, as_dataset(y))
})


test_that("setting array errors", {
    x <- as_dataset(mtcars)
    col <- array(1, dim = c(1, 1, nrow(x)))
    expect_error(x[[1]] <- col,
                 "replacement is not a vector or matrix")
})


test_that("setting too short", {
    x <- as_dataset(mtcars)
    col <- c(1, 2)
    expect_error(x[[1]] <- col, "replacement has 2 rows, data has 32")
})


test_that("setting scalar", {
    x <- as_dataset(mtcars)
    x[[1]] <- 1.20

    y <- mtcars
    y[[1]] <- 1.20
    y <- as_dataset(y)

    expect_equal(x, y)
})


test_that("appending column", {
    x <- as_dataset(mtcars)
    col <- rnorm(nrow(x))
    x$foo <- col

    mtcars$foo <- col
    y <- as_dataset(mtcars)
    expect_equal(x, y)
})


test_that("appending column by index", {
    ds <- mtcars
    n <- length(ds)
    col <- rnorm(nrow(ds))

    x <- as_dataset(ds)
    x[[n + 1L]] <- col
    y <- cbind.dataset(ds, col)

    expect_equal(x, y)
})


test_that("appending column by index, too far", {
    ds <- mtcars
    n <- length(ds)
    col <- rnorm(nrow(ds))

    x <- as_dataset(ds)
    x[[n + 3L]] <- col
    y <- cbind.dataset(ds, as_dataset(list(NULL, NULL, col)))

    expect_equal(x, y)
})


test_that("appending column with no names", {
    x <- as_dataset(mtcars)
    names(x) <- NULL
    col <- rnorm(nrow(x))
    x$foo <- col

    mtcars$foo <- col
    y <- as_dataset(mtcars)
    names(y) <- c(character(length(y) - 1), "foo")
    expect_equal(x, y)
})


test_that("replace column with scalar", {
    x <- as_dataset(mtcars)
    x[[5]] <- 17
    y <- mtcars
    y[[5]] <- 17
    y <- as_dataset(y)
    expect_equal(x, y)
})


test_that("replace column with scalar, double index", {
    x <- as_dataset(mtcars)
    x[, 5] <- 17
    y <- as_dataset(mtcars)
    y[[5]] <- 17
    expect_equal(x, y)
})


test_that("deleting columns with single index", {
    x <- as_dataset(mtcars)
    y <- mtcars
    x[c(4, 2, 4)] <- NULL
    y[c(4, 2)] <- NULL # errors on duplicates
    expect_equal(x, as_dataset(y))
})


test_that("deleting columns with name", {
    x <- as_dataset(mtcars)
    y <- mtcars
    i <- names(x)[c(5, 3, 8, 3)]
    x[i] <- NULL
    y[c(5, 3, 8)] <- NULL # errors on duplicates
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
    x[,3] <- 7
    y[,3] <- 7

    expect_equal(x, as_dataset(y))
})


test_that("index setting with matrix pairs works", {
    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
    val <- runif(length(i))

    x <- as_dataset(iris)
    x[cbind(i, j)] <- val

    y <- iris
    y[cbind(i, j)] <- val
    y <- as_dataset(y)

    expect_equal(x, y)
})


test_that("index setting with matrix pairs recycles", {
    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
    val <- 1.1

    x <- as_dataset(iris)
    x[cbind(i, j)] <- val

    y <- iris
    y[cbind(i, j)] <- val
    y <- as_dataset(y)

    expect_equal(x, y)
})


test_that("index setting with matrix and NA index errors", {
    i <- c(47, 5, 132, NA, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, NA, 3)
    val <- runif(length(i))

    x <- as_dataset(iris)
    expect_error(x[cbind(i, j)] <- val,
                 "NAs are not allowed in subscripted assignments")
})


test_that("matrix entry", {
    x <- dataset(x = 1:3,
                 foo = data.frame(bar = 1:3, baz = c("a", "b", "c"),
                                  stringsAsFactors = FALSE))
    x[rbind(c(2, 1),
            c(3, 2))] <- list(100, list(99, 88))

    y <- dataset(x = c(1, 100, 3),
                 foo = data.frame(bar = c(1, 2, 99),
                                  baz = c("a", "b", "88"),
                                  stringsAsFactors = FALSE))
    expect_equal(x, y)
})


test_that("errors for invalid single", {
    x <- dataset(x = 1:3, y = 4:6)
    expect_error(x[rbind(1, 4, 7, 3)], "index 3 \\(7\\) is out of bounds")
})


test_that("errors for invalid pair", {
    x <- dataset(x = 1:3,
                 foo = data.frame(bar = 1:3, baz = c("a", "b", "c"),
                                  stringsAsFactors = FALSE))
    expect_error(x[cbind(c(1, 2), c(2, 3))] <- list(100, list(99, 88)),
                 "index 2 \\(2, 3\\) is out of bounds")
})


test_that("setting invalid errors", {
    x <- as_dataset(mtcars)
    expect_error(x[[c(17, 1)]] <- 10, "non-scalar index \\(length 2\\)")
    expect_error(x[[17]] <- array(1, c(1,1,1)),
                 "replacement is not a vector or matrix")
})
