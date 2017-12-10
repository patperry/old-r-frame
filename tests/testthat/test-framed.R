
context("dataset")

test_that("'dataset' allows NULL names", {
    x <- framed(list(1, 2, 3))
    expect_equal(names(x), NULL)
})

# Removed; this is no longer the default behavior
#test_that("'dataset' evaluates columns in order", {
#    x <- dataset(x = 1:10, y = 2 * x, z = 3 * x, y = x)
#    l <- list(x = 1:10, y = 2 * (1:10), z = 3 * (1:10), y = 1:10)
#    expect_equal(x, framed(l))
#})


test_that("'framed.list' errors for non-list inputs", {
    expect_error(framed.list(NULL),
                 "argument is not a list")
})


test_that("'framed.list' allows duplicate column names", {
    expect_equal(dataset(x = 1:10, y = 1:10, x = 1:10),
                 framed(list(x = 1:10, y = 1:10, x = 1:10)))
})


test_that("'framed' does not error for invalid column name", {
    nm <- "fa\xE7ile"; Encoding(nm) <- "UTF-8" # latin-1, declared UTF-8
    x <- list(1:10)
    names(x)[[1]] <- nm
    expect_equal(names(framed(x)), nm)
})


test_that("'framed allows duplicate names after normalization", {
    x <- list(1, 2)
    names(x) <- c("\u00c5", "\u212b")
    expect_equal(names(framed(x)), names(x))
})


test_that("'framed fixes NA names", {
    x <- structure(list(4, 7), names = c("a", NA))
    y <- structure(list(4, 7), names = c("a", ""))

    expect_equal(framed(x), framed(y))
})


test_that("'framed allows empty names", {
    x <- structure(list(4, 7, 2, -9, 1), names = c("a", "b", "", "d", ""))

    expect_equal(names(framed(x)), names(x))
})


test_that("'framed.list' works for missing names", {
    x <- framed(list(a = 1, -5, 2.3))
    expect_equal(names(x), c("a", "", ""))
})


test_that("'framed.list' allows duplicated names", {
    l <- list(a = 1, a = -5, a = 2.3)
    x <- framed(l)
    expect_equal(names(x), names(l))
})


test_that("'framed' errors for unequal length columns", {
    expect_error(dataset(x = 1:10, y = 1:10, z = c(1, 2, 3)),
                 "columns 1 and 3 \\(\"x\" and \"z\"\\) have differing numbers of rows: 10 and 3")

    expect_error(dataset(x = 1:10, y = matrix(1:10, 2, 5)),
                 "columns 1 and 2 \\(\"x\" and \"y\"\\) have differing numbers of rows: 10 and 2")
})


test_that("'framed' errors for rank-3 array columns", {
    expect_error(dataset(x = array(1:24, c(2,3,4))),
                 "column 1 \\(\"x\"\\) has more than 2 dimensions")
})


test_that("'framed' errors for NULL columns", {
    expect_error(dataset(x = 1, y = NULL), "column 2 \\(\"y\"\\) is NULL")
})


test_that("'framed' errors for too many columns", {
    # use S3this rather than allocate a 2GB list to test
    x <- structure(list(), class = c("frame_longlist", "list"))
    length.frame_longlist <<- function(x) .Machine$integer.max + 1

    expect_error(framed(x),
                 "number of columns \\(2147483648\\) exceeds maximum \\(2147483647\\)")

    remove("length.frame_longlist", envir = .GlobalEnv)
})


test_that("'framed' allows scalar columns", {
    expect_equal(dataset(x = 1:10, y = 10:1, z = 1),
                 dataset(x = 1:10, y = 10:1, z = rep(1, 10)))
})


test_that("'framed' drops vector names", {
    x1 <- 1:26
    x2 <- x1; names(x2) <- letters
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'framed' drops 1-d array names", {
    x1 <- array(1:26)
    x2 <- array(1:26, dimnames = list(letters))
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'framed' drops matrix row names", {
    x1 <- matrix(1:20, 4, 5, dimnames = list(NULL, LETTERS[1:5]))
    x2 <- matrix(1:20, 4, 5, dimnames = list(letters[1:4], LETTERS[1:5]))
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'framed' can convert data.frame row names", {
    x1 <- data.frame(x = 1:26, row.names = letters)
    x2 <- data.frame(name = letters, x = 1:26, stringsAsFactors = FALSE)
    expect_equal(framed(x1), framed(x2, key = "name"))
})


test_that("'framed' errors for duplicated key", {
    expect_error(framed(mtcars, key = c("cyl", "cyl")),
                 "key contains duplicates")
})


test_that("'framed' errors for NA key", {
    expect_error(framed(mtcars, key = c("cyl", NA)),
                 "key contains NA")
})


test_that("'framed' errors for missing key", {
    expect_error(framed(mtcars, key = c("cyl", "zzz")),
                 "key refers to unknown column \"zzz\"")
})


test_that("'framed' works for nested", {
    l <- list(zz = dataset(a = 1:4))
    x <- framed(l)
    expect_equal(x, dataset(zz = dataset(a = 1:4)))
})