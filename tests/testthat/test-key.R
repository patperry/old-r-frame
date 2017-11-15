
context("key")

test_that("'as_dataset' on data.frame uses row names as key", {
    x <- as_dataset(mtcars)
    expect_equal(key(x), "name")
})


test_that("'as_dataset' on data.frame uses row names special column as key", {
    x <- as_dataset(mtcars, rownames = "foo")
    expect_equal(key(x), "foo")
})


test_that("'key' errors for vector columns", {
    expect_error(dataset(x = matrix(1:4, 4, 1), key = "x"),
                 "key column \"x\" is not a vector")
})


test_that("'key' errors for NA", {
    expect_error(dataset(x = c(1:4, NA), key = "x"),
                 "key column \"x\" has a missing value \\(entry 5\\)")
})


test_that("'key' errors for invalid UTF-8", {
    x <- "fa\xE7ile"; Encoding(x) <- "UTF-8"
    expect_error(dataset(x, key = "x"),
                 "key column \"x\" cannot be converted to UTF-8 \\(entry 1 is invalid\\)")
})


test_that("'key' errors for single if not unique", {
    expect_error(dataset(x = c(1, 2, 3, 2), key = "x"),
                 "key column \"x\" has duplicate entries \\(2 and 4\\)")
})


test_that("'key' errors for multiple if not unique", {
    expect_error(dataset(x = c(1, 1, 2, 2),
                         y = c(1, 2, 1, 1),
                         key = c("x", "y")),
                 "key column set \\(\"x\", \"y\"\\) has duplicate rows \\(3 and 4\\)")
})


test_that("'key<-' NULL works ", {
    ds <-dataset(x = letters, key = "x")
    expect_equal(key(ds), "x")
    key(ds) <- NULL
    expect_equal(key(ds), NULL)
    key(ds) <- NULL
    expect_equal(key(ds), NULL)
})


test_that("'key<-' errors for invalid key", {
    ds <- dataset(x = c(1, 1, 2, 2), y = c(1, 2, 1, 1))
    expect_error(key(ds) <- c("x", "y"),
                 "key column set \\(\"x\", \"y\"\\) has duplicate rows \\(3 and 4\\)")
})


test_that("'key<-' errors for key with duplicates", {
    expect_error(as_dataset(mtcars, key = "vs"),
                 "key column \"vs\" has duplicate entries \\(1 and 2\\)")

    x <- as_dataset(mtcars)
    expect_error(key(x) <- "vs",
                 "key column \"vs\" has duplicate entries \\(1 and 2\\)")
})


test_that("'key<-' moves columns to beginning", {
    ds1 <- dataset(z = letters[1:4], x = c(1, 1, 2, 2), y = c(1, 2, 1, 2),
                   key = c("x", "y"))
    ds2 <- dataset(x = c(1, 1, 2, 2), z = letters[1:4], y = c(1, 2, 1, 2),
                   key = c("x", "y"))
    ds3 <- dataset(y = c(1, 2, 1, 2), x = c(1, 1, 2, 2), z = letters[1:4],
                   key = c("x", "y"))
    expect_equal(ds1, ds2)
    expect_equal(ds1, ds3)
})


test_that("'keyvals' is NULL if key is NULL", {
    ds <- dataset(x = letters)
    expect_equal(keyvals(ds), NULL)
})


test_that("'keyvals' gives unique values for length-1 key", {
    ds <- dataset(x = LETTERS[1:20], y = rep(1:10, each = 2), key = "x")
    expect_equal(keyvals(ds), list(x = LETTERS[1:20]))
})


test_that("'keyvals' gives unique values for length-2 key", {
    ds <- dataset(x = c(letters, letters), y = rep(1:13, each = 4),
                  key = c("x", "y"))
    expect_equal(keyvals(ds), list(x = letters, y = as.character(1:13)))
})


test_that("'rownames' works if key is set", {
    x <- as_dataset(mtcars)
    expect_equal(rownames(x), rownames(mtcars))
})


test_that("'row.names' works if key is set", {
    x <- as_dataset(mtcars)
    expect_equal(row.names(x), row.names(mtcars))
})


test_that("'rownames' works for multiple keys", {
    ds <- dataset(x = c(1, 1, 2, 2), y = c(1, 2, 1, 2),
                  key = c("x", "y"))
    expect_equal(rownames(ds), c("1:1", "1:2", "2:1", "2:2"))
    expect_equal(row.names(ds), c("1:1", "1:2", "2:1", "2:2"))
})
