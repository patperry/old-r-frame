
context("index")


test_that("duplicates should not error if key is set", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19, 5, 7)
    y <- x[i,]
    z <- framed(mtcars[i,],
                as_keyset(list(name = rownames(mtcars)[i],
                               "#" = c(1, 1, 1, 1, 2, 1))))
    expect_equal(y, z)
})


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

