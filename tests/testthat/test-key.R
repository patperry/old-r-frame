
context("key")

test_that("'rownames' returns NULL when absent", {
    x <- as_dataset(mtcars)
    expect_equal(rownames(x), NULL)
})
