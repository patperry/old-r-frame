
context("key")

test_that("'key_escape' handles \\ and ' '", {
    expect_equal(key_escape(c("Jones, Henrietta", "\\/", ",,,", "\\,\\", "")),
                 c("Jones\\, Henrietta", "\\\\/", "\\,\\,\\,", "\\\\\\,\\\\",
                   ""))
})


test_that("'key_unescape' reverses `key_escape", {
    expect_equal(key_unescape(c("Jones\\, Mx.", "\\\\/", "\\,\\,\\,",
                                "\\\\\\,\\\\", "")),
                c("Jones, Mx.", "\\/", ",,,", "\\,\\", ""))
})


test_that("'key_encode' gives NULL for NULL or length 0", {
    expect_equal(key_encode(NULL), NULL)
    expect_equal(key_encode(list()), NULL)
})


test_that("'key_decode' gives NULL for NULL", {
    expect_equal(key_decode(NULL), NULL)
})


test_that("'key_encode' leaves scalar alone", {
    k <- c("Jones, Henrieta", "X, Mx.", "Box, George")
    expect_equal(key_encode(list(k)), k)
})


test_that("'key_decode' puts no-comma in list", {
    k <- c("Jones, Henrieta", "X, Mx.", "Box, George")
    expect_equal(key_decode(k, composite = FALSE), list(k))
})


test_that("'key_encode' escapes and combines others", {
    k <- list(c("Jones, Henrieta", "X, Mx.", "Box, George"),
              c("\\/", "", "2\\b"),
              c("92", "2", "17"))
    e <- lapply(k, key_escape)
    s <- paste0(e[[1]], ",", e[[2]], ",", e[[3]])
    expect_equal(key_encode(k), s)
})


test_that("'key_decode' splits and unescapes composites ", {
    k <- list(c("Jones, Henrieta", "X, Mx.", "Box, George"),
              c("\\/", "", "2\\b"),
              c("92", "2", "17"))
    e <- lapply(k, key_escape)
    s <- paste0(e[[1]], ",", e[[2]], ",", e[[3]])
    x <- key_encode(k)
    expect_equal(key_decode(x), k)
})


test_that("'as_dataset' on data.frame uses row names as key", {
    x <- as_dataset(mtcars)
    expect_equal(keys(x)[[1]], rownames(mtcars))
})


test_that("'as_dataset' on data.frame uses row names special column as key", {
    x <- as_dataset(mtcars, rownames = "foo")
    expect_equal(keys(x)[["foo"]], rownames(mtcars))
})


test_that("'key' errors for vector columns", {
    expect_error(dataset(x = matrix(1:4, 4, 1), key = "x"),
                 "key column 1 \\(\"x\"\\) is not a vector")
})


test_that("'key' errors for NA", {
    expect_error(dataset(x = c(1:4, NA), key = "x"),
                 "key column 1 \\(\"x\"\\) has a missing value \\(entry 5\\)")
})


test_that("'key' errors for invalid UTF-8", {
    x <- "fa\xE7ile"; Encoding(x) <- "UTF-8"
    expect_error(dataset(x, key = "x"),
                 "key column 1 \\(\"x\"\\) cannot be converted to UTF-8 \\(entry 1 is invalid\\)")
})


test_that("'key' errors for single if not unique", {
    expect_error(dataset(x = c(1, 2, 3, 2), key = "x"),
                 "key set has duplicate entries \\(2 and 4\\)")
})


test_that("'key' errors for multiple if not unique", {
    expect_error(dataset(x = c(1, 1, 2, 2),
                         y = c(1, 2, 1, 1),
                         key = c("x", "y")),
                 "key set has duplicate rows \\(3 and 4\\)")
})


test_that("'key<-' NULL works ", {
    ds <- dataset(x = letters, key = "x")
    expect_equal(keys(ds)[[1]], letters)
    keys(ds) <- NULL
    expect_equal(keys(ds), NULL)
    keys(ds) <- NULL
    expect_equal(keys(ds), NULL)
})


test_that("'key<-' errors for invalid key", {
    expect_error(dataset(x = c(1, 1, 2, 2), y = c(1, 2, 1, 1),
                         key = c("x", "y")),
                 "key set has duplicate rows \\(3 and 4\\)")
})


test_that("'key<-' errors for key with duplicates", {
    expect_error(as_dataset(mtcars, key = "vs"),
                 "key set has duplicate entries \\(1 and 2\\)")

    x <- as_dataset(mtcars)
    expect_error(keys(x) <- mtcars[,"vs",drop=FALSE],
                 "key set has duplicate entries \\(1 and 2\\)")
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
    expect_equal(rownames(ds), c("1,1", "1,2", "2,1", "2,2"))
    expect_equal(row.names(ds), c("1,1", "1,2", "2,1", "2,2"))
})
