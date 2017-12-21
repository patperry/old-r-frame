
context("key")


test_that("'as_dataset' on data.frame uses row names as key", {
    x <- as_dataset(mtcars)
    expect_equal(keys(x)[[1]], rownames(mtcars))
})


test_that("'as_dataset' on data.frame uses row names special column as key", {
    x <- as_dataset(mtcars)
    expect_equal(keys(x)[["name"]], rownames(mtcars))
})


test_that("'key' errors for vector columns", {
    expect_error(framed(list(x = matrix(1:4, 4, 1)), keys = "x"),
                 "argument column 1 \\(\"x\"\\) is not a vector")
})


test_that("'key' errors for invalid UTF-8", {
    x <- "fa\xE7ile"; Encoding(x) <- "UTF-8"
    expect_error(framed(list(x = x), keys = "x"),
                 "argument column 1 \\(\"x\"\\) cannot be encoded in valid UTF-8 \\(entry 1 is invalid\\)")
})


test_that("'key' errors for single if not unique", {
    expect_error(framed(list(x = c(1, 2, 3, 2)), keys = "x"),
                 "argument has a duplicate row \\(4\\)")
})


test_that("'key' errors for multiple if not unique", {
    expect_error(framed(list(x = c(1, 1, 2, 2), y = c(1, 2, 1, 1)),
                            keys = c("x", "y")),
                 "argument has a duplicate row \\(4\\)")
})


test_that("'key<-' NULL works ", {
    ds <- framed(list(x = letters), keys = "x")
    expect_equal(keys(ds)[[1]], letters)
    keys(ds) <- NULL
    expect_equal(keys(ds), NULL)
    keys(ds) <- NULL
    expect_equal(keys(ds), NULL)
})


test_that("'key<-' errors for invalid key", {
    expect_error(framed(list(x = c(1, 1, 2, 2), y = c(1, 2, 1, 1)),
                        keys = c("x", "y")),
                 "argument has a duplicate row \\(4\\)")
})


test_that("'key<-' errors for key with duplicates", {
    expect_error(framed(mtcars, keys = "vs"),
                 "argument has a duplicate row \\(2\\)")

    x <- as_dataset(mtcars)
    expect_error(keys(x) <- mtcars[,"vs",drop=FALSE],
                 "argument has a duplicate row \\(2\\)")
})


test_that("'key<-' moves columns to beginning", {
    ds1 <- framed(list(z = letters[1:4], x = c(1, 1, 2, 2),
                           y = c(1, 2, 1, 2)),
                      keys = c("x", "y"))
    ds2 <- framed(list(x = c(1, 1, 2, 2), z = letters[1:4],
                           y = c(1, 2, 1, 2)),
                      keys = c("x", "y"))
    ds3 <- framed(list(y = c(1, 2, 1, 2), x = c(1, 1, 2, 2),
                           z = letters[1:4]),
                      keys = c("x", "y"))
    expect_equal(ds1, ds2)
    expect_equal(ds1, ds3)
})


test_that("'keylevels' is NULL if key is NULL", {
    ds <- dataset(x = letters)
    expect_equal(keylevels(ds), NULL)
})


test_that("'keylevels' gives unique values for length-1 key", {
    ds <- framed(list(x = LETTERS[1:20], y = rep(1:10, each = 2)),
                 keys = "x")
    expect_equal(keylevels(ds), list(x = LETTERS[1:20]))
})


test_that("'keylevels' gives unique values for length-2 key", {
    ds <- framed(list(x = c(letters, letters), y = rep(1:13, each = 4)),
                 keys = c("x", "y"))
    expect_equal(keylevels(ds), list(x = letters, y = 1:13))
})


test_that("'rownames' works if key is set", {
    x <- as_dataset(mtcars)
    expect_equal(rownames(x), rownames(mtcars))
})


test_that("'row.names' works if key is set", {
    x <- as_dataset(mtcars)
    expect_equal(row.names(x), row.names(mtcars))
})


test_that("'rownames' gives NULL for multiple keys", {
    ds <- framed(list(name = c(1, 1, 2, 2), y = c(1, 2, 1, 2)),
                      keys = c("name", "y"))
    expect_equal(rownames(ds), NULL)
    expect_equal(row.names(ds), NULL)
})


test_that("'keys' gives converted row names", {
    x <- as_dataset(mtcars)
    expect_equal(keys(x)[[1]], rownames(mtcars))
})


test_that("'keys<-' works with scalar", {
    x <- y <- dataset(a = 1:26)
    keys(x) <- letters
    keys(y) <- framed(letters)
    expect_equal(x, y)
})
