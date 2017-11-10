
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


test_that("'as_dataset' errors for unequal length columns", {
    expect_error(dataset(x = 1:10, y = 1:10, z = 1),
                 "columns 1 and 3 have differing numbers of rows: 10 and 1")

    expect_error(dataset(x = 1:10, y = matrix(1:10, 2, 5)),
                 "columns 1 and 2 have differing numbers of rows: 10 and 2")
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
