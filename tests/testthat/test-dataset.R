
context("dataset")


test_that("'dataset' allows NULL names", {
    x <- as_dataset(list(1, 2, 3))
    expect_equal(names(x), NULL)
})


test_that("'as_dataset.list' allows duplicate column names", {
    expect_equal(dataset(x = 1:10, y = 1:10, x = 1:10),
                 as_dataset(list(x = 1:10, y = 1:10, x = 1:10)))
})


test_that("'as_dataset' does not error for invalid column name", {
    nm <- "fa\xE7ile"; Encoding(nm) <- "UTF-8" # latin-1, declared UTF-8
    x <- list(1:10)
    names(x)[[1]] <- nm
    expect_equal(names(as_dataset(x)), nm)
})


test_that("'as_dataset' allows duplicate names after normalization", {
    x <- list(1, 2)
    names(x) <- c("\u00c5", "\u212b")
    expect_equal(names(as_dataset(x)), names(x))
})


test_that("'as_dataset' fixes NA names", {
    x <- structure(list(4, 7), names = c("a", NA))
    y <- structure(list(4, 7), names = c("a", ""))

    expect_equal(as_dataset(x), as_dataset(y))
})


test_that("'as_dataset' allows empty names", {
    x <- structure(list(4, 7, 2, -9, 1), names = c("a", "b", "", "d", ""))

    expect_equal(names(as_dataset(x)), names(x))
})


test_that("'as_dataset.list' works for missing names", {
    x <- as_dataset(list(a = 1, -5, 2.3))
    expect_equal(names(x), c("a", "", ""))
})


test_that("'as_dataset.list' allows duplicated names", {
    l <- list(a = 1, a = -5, a = 2.3)
    x <- as_dataset(l)
    expect_equal(names(x), names(l))
})


test_that("'dataset' errors for unequal length columns", {
    expect_error(dataset(x = 1:10, y = 1:10, z = c(1, 2, 3)),
                 "columns 1 and 3 \\(\"x\" and \"z\"\\) have differing numbers of rows: 10 and 3")

    expect_error(dataset(x = 1:10, y = matrix(1:10, 2, 5)),
                 "columns 1 and 2 \\(\"x\" and \"y\"\\) have differing numbers of rows: 10 and 2")
})


test_that("'dataset' errors for rank-3 array columns", {
    expect_error(dataset(x = array(1:24, c(2,3,4))),
                 "column 1 \\(\"x\"\\) has more than 2 dimensions")
})


test_that("'dataset' errors for NULL columns", {
    expect_error(dataset(x = 1, y = NULL), "column 2 \\(\"y\"\\) is NULL")
})


test_that("'dataset' allows scalar columns", {
    expect_equal(dataset(x = 1:10, y = 10:1, z = 1),
                 dataset(x = 1:10, y = 10:1, z = rep(1, 10)))
})


test_that("'dataset' drops vector names", {
    x1 <- 1:26
    x2 <- x1; names(x2) <- letters
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'dataset' drops 1-d array names", {
    x1 <- array(1:26)
    x2 <- array(1:26, dimnames = list(letters))
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'dataset' drops matrix row names", {
    x1 <- matrix(1:20, 4, 5, dimnames = list(NULL, LETTERS[1:5]))
    x2 <- matrix(1:20, 4, 5, dimnames = list(letters[1:4], LETTERS[1:5]))
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'as_dataset' works for nested", {
    l <- list(zz = dataset(a = 1:4))
    x <- as_dataset(l)
    expect_equal(x, dataset(zz = dataset(a = 1:4)))
})


test_that("'as_dataset' works with no columns", {
    x <- mtcars
    x[] <- NULL
    x <- as_dataset(x)

    expect_equal(nrow(x), nrow(mtcars))
    expect_equal(names(x), character())
    expect_equal(length(x), 0)
    expect_equal(rownames(x), rownames(mtcars))
    expect_true(is_dataset(x))
})


test_that("'as_dataset' can convert data.frame row names", {
    x1 <- data.frame(x = 1:26, row.names = letters)
    x2 <- framed(dataset(x = 1:26), keyset(name = letters))
    expect_equal(as_dataset(x1), x2)
})


test_that("'as_dataset' does not require names", {
    expect_equal(names(as_dataset(list("hello"))), NULL)
    expect_equal(names(as_dataset(list("hello"), simple = TRUE)), NULL)
})


test_that("'dataset' can be nested", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(dim(x), c(5, 2))
    expect_equal(names(x), c("a", "b"))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(dim(y), c(5, 3))
    expect_equal(names(y), c("a", "b", "c"))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(dim(z), c(5, 2))
    expect_equal(names(z), c("a", "b"))
})


test_that("as_dataset(list()) has 1 row", {
    x <- as_dataset(list())
    y <- as_dataset(structure(list(), row.names = .set_row_names(1),
                              class = "data.frame"))
    expect_equal(x, y)
})


test_that("handles list matrix", {
    mat <- rbind(list("D", 3),
                 list("A", 1),
                 list("C", 1))
    x <- as_dataset(mat)
    y <- as_dataset(list(mat[, 1], mat[, 2]))
    expect_equal(x, y)
})


test_that("errors for list array", {
    arr <- array(as.list(1:6), c(1, 2, 3))
    expect_error(as_dataset(arr), "cannot convert rank-3 array to dataset")
})
