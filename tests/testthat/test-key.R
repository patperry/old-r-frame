
context("key")

test_that("'as_dataset' on data.frame uses row names as key", {
    x <- as_dataset(mtcars)
    expect_equal(keynames(x), "name")
})


test_that("'as_dataset' on data.frame uses row names special column as key", {
    x <- as_dataset(mtcars, rownames = "foo")
    expect_equal(keynames(x), "foo")
})
