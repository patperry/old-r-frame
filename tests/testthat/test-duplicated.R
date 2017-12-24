context("duplicated")

test_that("fixes bugs in base R", {
    x <- as_dataset(data.frame(x = c(.15, .1 + .05), y = "a"))
    expect_equal(unique(x), as_keyset(x))
})
