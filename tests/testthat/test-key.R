
context("key")

test_that("'as_dataset' on data.frame uses row names as key", {
    x <- as_dataset(mtcars)
    expect_equal(key(x), "name")
})


test_that("'as_dataset' on data.frame uses row names special column as key", {
    x <- as_dataset(mtcars, rownames = "foo")
    expect_equal(key(x), "foo")
})


test_that("'rownames' works if key is set", {
    x <- as_dataset(mtcars)
    expect_equal(rownames(x), rownames(mtcars))
})


test_that("'row.names' works if key is set", {
    x <- as_dataset(mtcars)
    expect_equal(row.names(x), row.names(mtcars))
})
