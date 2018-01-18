
context("index-key")

test_that("row names", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], x[rownames(x)[i],])
})


test_that("list", {
    x <- cbind.dataset(name = rownames(mtcars),
                       mtcars[1:3],
                       dataset(group = mtcars[4:5]))
    keys(x) <- dataset(k1 = rep(LETTERS[1:4], 8),
                       k2 = rep(1:8, each = 4))

    i <- list("D", 3)
    expect_equal(x[i, ], x[rowid(x, list("D", 3)), ])
    expect_equal(x[i, ], x[as_dataset(i), ])
})
