
context("index-key")

test_that("indexing with row names works", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], x[rownames(x)[i],])
})
