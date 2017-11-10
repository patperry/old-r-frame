
context("dataset")

test_that("'dataset' errors for duplicate columns", {
    expect_error(dataset(x = 1:10, y = 2 * x, z = 3 * x, y = x),
                 "duplicate column name: 'y'")
})


test_that("'as_dataset.list' errors for non-list inputs", {
    expect_error(as_dataset.list(NULL),
                 "argument is not a list")
})


test_that("'as_dataset.list' errors for duplicate columns", {
    expect_error(as_dataset(list(x = 1:10, y = 1:10, x = 1:10)),
                 "duplicate column name: 'x'")
})


test_that("'as_dataset' errors for unequal length columns", {
    expect_error(dataset(x = 1:10, y = 1:10, z = 1),
                 "columns 1 and 3 have differing numbers of rows: 10 and 1")

    expect_error(dataset(x = 1:10, y = matrix(1:10, 2, 5)),
                 "columns 1 and 2 have differing numbers of rows: 10 and 2")
})


test_that("'as_dataset.list' works for missing names", {
    x <- as_dataset(list(1, -5, 2.3))
    y <- as_dataset(list(V1 = 1, V2 = -5, V3 = 2.3))
    expect_equal(x, y)
})
