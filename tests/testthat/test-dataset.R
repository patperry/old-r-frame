
context("dataset")

test_that("'dataset' errors for duplicate columns", {
    expect_error(dataset(x = 1:10, y = 2 * x, z = 3 * x, y = x),
                 "duplicate column name: \"y\"")
})


test_that("'as_dataset.list' errors for non-list inputs", {
    expect_error(as_dataset.list(NULL),
                 "argument is not a list")
})


test_that("'as_dataset.list' errors for duplicate columns", {
    expect_error(as_dataset(list(x = 1:10, y = 1:10, x = 1:10)),
                 "duplicate column name: \"x\"")
})


test_that("'as_dataset' errors for invalid column name", {
    nm <- "fa\xE7ile"; Encoding(nm) <- "UTF-8" # latin-1, declared UTF-8
    x <- list(1:10)
    names(x)[[1]] <- nm
    expect_error(as_dataset(x), "column name 1 has wrong declared Encoding")
})


test_that("'as_dataset errors for duplicate name after normalization", {
    x <- list(1, 2)
    names(x) <- c("\u00c5", "\u212b")
    expect_error(as_dataset(x),
                 "duplicate \\(normalized\\) column name: \"\u00c5\"")
})


test_that("'as_dataset fixes NA names", {
    x <- structure(list(4, 7), names = c("a", NA))
    y <- structure(list(4, 7), names = c("a", "V2"))

    expect_equal(as_dataset(x), as_dataset(y))
})


test_that("'as_dataset fixes empty names", {
    x <- structure(list(4, 7, 2, -9, 1), names = c("a", "b", "", "d", ""))
    y <- structure(list(4, 7, 2, -9, 1), names = c("a", "b", "V3", "d", "V5"))

    expect_equal(as_dataset(x), as_dataset(y))
})


test_that("'as_dataset.list' works for missing names", {
    x <- as_dataset(list(1, -5, 2.3))
    y <- as_dataset(list(V1 = 1, V2 = -5, V3 = 2.3))
    expect_equal(x, y)
})


test_that("'as_dataset.list' errors for duplicated names", {
    x <- list(V2 = 1, -5, 2.3)
    expect_error(as_dataset(x),
                 "cannot create column name 2; name \"V2\" already exists")
})


test_that("'as_dataset.data.frame' errors if name already exists", {
    expect_error(as_dataset(mtcars, rownames = "cyl"),
                 "cannot create column for row names; name \"cyl\" already exists")
})


test_that("'as_dataset' errors for unequal length columns", {
    expect_error(dataset(x = 1:10, y = 1:10, z = c(1, 2, 3)),
                 "columns 1 and 3 have differing numbers of rows: 10 and 3")

    expect_error(dataset(x = 1:10, y = matrix(1:10, 2, 5)),
                 "columns 1 and 2 have differing numbers of rows: 10 and 2")
})


test_that("'as_dataset' errors for NULL columns", {
    expect_error(dataset(x = 1, y = NULL),
                 "columns 1 and 2 have differing numbers of rows: 1 and 0")
})


test_that("'as_dataset' errors for too many columns", {
    # use S3this rather than allocate a 2GB list to test
    x <- structure(list(), class = c("frame_longlist", "list"))
    length.frame_longlist <<- function(x) .Machine$integer.max + 1

    expect_error(as_dataset(x),
                 "number of columns \\(2147483648\\) exceeds maximum \\(2147483647\\)")

    remove("length.frame_longlist", envir = .GlobalEnv)
})


test_that("'as_dataset' allows scalar columns", {
    expect_equal(dataset(x = 1:10, y = 10:1, z = 1),
                 dataset(x = 1:10, y = 10:1, z = rep(1, 10)))
})


test_that("'as_dataset' drops vector names", {
    x1 <- 1:26
    x2 <- x1; names(x2) <- letters
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'as_dataset' drops 1-d array names", {
    x1 <- array(1:26)
    x2 <- array(1:26, dimnames = list(letters))
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'as_dataset' drops matrix row names", {
    x1 <- matrix(1:20, 4, 5, dimnames = list(NULL, LETTERS[1:5]))
    x2 <- matrix(1:20, 4, 5, dimnames = list(letters[1:4], LETTERS[1:5]))
    expect_equal(dataset(x = x1), dataset(x = x2))
})


test_that("'as_dataset' can convert data.frame row names", {
    x1 <- data.frame(x = 1:26, row.names = letters)
    x2 <- data.frame(name = letters, x = 1:26, stringsAsFactors = FALSE)
    expect_equal(as_dataset(x1), as_dataset(x2, key = "name"))
})


test_that("'as_dataset' errors for duplicated key", {
    expect_error(as_dataset(mtcars, key = c("cyl", "cyl")),
                 "key contains duplicates")
})


test_that("'as_dataset' errors for NA key", {
    expect_error(as_dataset(mtcars, key = c("cyl", NA)),
                 "key contains NA")
})


test_that("'as_dataset' errors for missing key", {
    expect_error(as_dataset(mtcars, key = c("cyl", "zzz")),
                 "key refers to unknown column \"zzz\"")
})
