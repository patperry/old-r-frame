context("as_matrix")

test_that("basic operation with row names", {
    x1 <- as.matrix(as_dataset(mtcars), NA)
    x2 <- as.matrix(mtcars, NA)
    expect_equal(x1, x2)

    x1 <- as.matrix(as_dataset(mtcars), TRUE)
    x2 <- as.matrix(mtcars, TRUE)
    expect_equal(x1, x2)

    x1 <- as.matrix(as_dataset(mtcars), FALSE)
    x2 <- as.matrix(mtcars, FALSE)
    expect_equal(x1, x2)
})


test_that("basic operation without row names", {
    df <- iris
    rownames(df) <- NULL
    x1 <- as.matrix(as_dataset(df), NA)
    x2 <- as.matrix(df, NA)
    expect_equal(x1, x2)

    x1 <- as.matrix(as_dataset(df), TRUE)
    x2 <- as.matrix(df, TRUE)
    expect_equal(x1, x2)

    x1 <- as.matrix(as_dataset(df), FALSE)
    x2 <- as.matrix(df, FALSE)
    expect_equal(x1, x2)
})


test_that("converting 0-column named works", {
    df <- mtcars[,FALSE]
    x1 <- as.matrix(as_dataset(df), NA)
    x2 <- as.matrix(df, NA)
    expect_equal(x1, x2)

    x1 <- as.matrix(as_dataset(df), TRUE)
    x2 <- as.matrix(df, TRUE)
    expect_equal(x1, x2)

    x1 <- as.matrix(as_dataset(df), FALSE)
    x2 <- as.matrix(df, FALSE)
    expect_equal(x1, x2)
})


test_that("converting 0-column unnamed works", {
    df <- iris[,FALSE]
    x1 <- as.matrix(as_dataset(df), NA)
    x2 <- as.matrix(df, NA)
    expect_equal(x1, x2)

    x1 <- as.matrix(as_dataset(df), TRUE)
    x2 <- as.matrix(df, TRUE)
    expect_equal(x1, x2)

    x1 <- as.matrix(as_dataset(df), FALSE)
    x2 <- as.matrix(df, FALSE)
    expect_equal(x1, x2)
})
