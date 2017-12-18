
context("index")

test_that("indexing with column number works", {
    x <- framed(mtcars)
    j <- c(5, 3, 0, 7)
    expect_equal(x[j], framed(mtcars[j]))
})


test_that("indexing with negative column number works", {
    x <- framed(mtcars)
    j <- -c(5, 3, 0, 7)
    expect_equal(x[j], framed(mtcars[j]))
})


test_that("indexing with mixed sign fails", {
    x <- framed(mtcars)
    j <- c(-5, -3, 0, 7)
    expect_error(x[j], "only 0's may be mixed with negative subscripts")
})


test_that("indexing with out of bounds positive fails", {
    x <- framed(mtcars)
    j <- c(5, 3, 2, 100, 1)
    expect_error(x[j], "column selection entry 4 is out of bounds")
})


test_that("indexing with NA fails", {
    x <- framed(mtcars)
    j <- c(seq_along(mtcars)[-1], NA)
    expect_error(x[j], "column selection entry 11 is NA")
})


test_that("indexing with column logical works", {
    x <- framed(mtcars)
    j <- rep(c(TRUE, FALSE), length(mtcars))[seq_along(mtcars)]
    expect_equal(x[j], framed(mtcars[j]))
})


test_that("indexing with NA column logical errors", {
    x <- framed(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1), NA)
    expect_error(x[j], "column selection entry 11 is NA")
})


test_that("indexing with wrong number of logical errors", {
    x <- framed(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1))
    expect_error(x[j],
        "selection mask length \\(10\\) must equal number of columns \\(11\\)")
})


test_that("indexing with row number works", {
    x <- framed(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], framed(mtcars[i,]))
})


test_that("indexing with row number and 'drop' works", {
    x <- framed(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i, , drop = FALSE], framed(mtcars[i,]))
})


test_that("indexing with duplicates should not error if key is set", {
    x <- framed(mtcars)
    i <- c(13, 5, 20, 19, 5, 7)
    y <- x[i,]
    z <- framed(mtcars[i,])
    keys(z) <- framed(list(name = rownames(mtcars)[i],
                               c(1, 1, 1, 1, 2, 1)))
    expect_equal(y, z)
})


test_that("indexing with row names works", {
    x <- framed(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], x[rownames(x)[i],])
})


test_that("indexing with column number works", {
    x <- framed(mtcars)
    j <- c(5, 3, 7)
    expect_equal(x[,j], framed(mtcars[,j]))
})


test_that("'length' does not include key columns", {
    expect_equal(length(framed(mtcars)), length(mtcars))
})


test_that("indexing without keys works", {
    x <- framed(list(a = letters))
    expect_equal(framed(x[1:5,,drop = FALSE]),
                 framed(list(a = letters[1:5])))
})


test_that("indexing with named first key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[row = "a", drop = FALSE], x[keys(x)$row == "a",])
    expect_equal(x[row = "b", drop = FALSE], x[keys(x)$row == "b",])
    expect_equal(x[row = "d", drop = FALSE], x[keys(x)$row == "d",])
})


test_that("indexing with list first key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[list("a", NULL), , drop = FALSE], x[keys(x)$row == "a",])
    expect_equal(x[list("b", NULL), , drop = FALSE], x[keys(x)$row == "b",])
    expect_equal(x[list("d", NULL), , drop = FALSE], x[keys(x)$row == "d",])
})


test_that("indexing with named two first keys works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[row = c("a","c"),], x[keys(x)$row %in% c("a", "c"),])
    expect_equal(x[row = c("c","b"),], x[keys(x)$row %in% c("c", "b"),])
})


test_that("indexing with list two first keys works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[list(c("a","c"), NULL), ], x[keys(x)$row %in% c("a", "c"),])
    expect_equal(x[list(c("c","b"), NULL), ], x[keys(x)$row %in% c("c", "b"),])
})


test_that("indexing with named second key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[col = "x",,drop = FALSE], x[keys(x)$col == "x",])
    expect_equal(x[col = "y",,drop = FALSE], x[keys(x)$col == "y",])
    expect_equal(x[col = "z",,drop = FALSE], x[keys(x)$col == "z",])
})


test_that("indexing with list second key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[list(NULL,"x"),,drop = FALSE], x[keys(x)$col == "x",])
    expect_equal(x[list(NULL,"y"),,drop = FALSE], x[keys(x)$col == "y",])
    expect_equal(x[list(NULL,"z"),,drop = FALSE], x[keys(x)$col == "z",])
})


test_that("indexing with named two second keys works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[col = c("y","x"),], x[keys(x)$col %in% c("y", "x"),])
    expect_equal(x[col = c("x","z"),], x[keys(x)$col %in% c("x", "z"),])
})


test_that("indexing with list two second keys works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[list(NULL, c("y","x")),], x[keys(x)$col %in% c("y", "x"),])
    expect_equal(x[list(NULL, c("x","z")),], x[keys(x)$col %in% c("x", "z"),])
})


test_that("indexing with named key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[row = "b", drop = FALSE], x[keys(x)$row == "b",])
    expect_equal(x[row = c("a","c")], x[keys(x)$row %in% c("a", "c"),])
    expect_equal(x[col = c("y","x"),], x[keys(x)$col %in% c("y", "x"),])
    expect_equal(x[col = "x",,drop = FALSE], x[keys(x)$col == "x",])
    expect_equal(x[col = "y", row = "c", drop = FALSE],
                 x[with(keys(x), col == "y" & row == "c"),])
})


test_that("indexing with name 'x' works", {
    x <- framed(list(x=2:6, y = 6:10), keys = "x")
    expect_equal(x[x = c(3, 5),], x[c(2, 4),])
})
