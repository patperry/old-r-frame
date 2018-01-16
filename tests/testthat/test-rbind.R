context("rbind")

test_that("'rbind' concatenates rows", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4, , drop = FALSE]
    x3 <- mtcars[5:8, ]
    y <- rbind.dataset(x1, x2, x3)
    expect_equal(y, as_dataset(mtcars[1:8, ]))
})


test_that("'rbind' NULL works", {
    expect_equal(rbind.dataset(), NULL)
    expect_equal(rbind.dataset(NULL), NULL)
    expect_equal(rbind.dataset(NULL, NULL), NULL)
})


test_that("'rbind' ignores NULL", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:8, ]
    y <- rbind.dataset(x1, NULL, NULL, x2, NULL)
    expect_equal(y, rbind.dataset(x1, x2))
})


test_that("'rbind' errors if columns do not match", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, 1:4]
    expect_error(rbind.dataset(x1, x1, x2),
                 "arguments 1 and 3 have different numbers of columns")
})


test_that("'rbind' errors if columns do not match, with NULL in between", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, 1:4]
    expect_error(rbind.dataset(NULL, x1, x1, NULL, x2),
                 "arguments 2 and 5 have different numbers of columns")
})


test_that("'rbind' errors if names do not match", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, rev(seq_len(ncol(x1)))]
    expect_error(rbind.dataset(x1, x2),
                 "arguments 1 and 2 have different names")
})


test_that("'rbind' allows NULL names on second", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, ]
    y <- rbind.dataset(x1, unname(x2))
    expect_equal(y, rbind.dataset(x1, x2))
})


test_that("'rbind' allows NULL names on first", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, ]
    y <- rbind.dataset(unname(x1), x2)
    expect_equal(y, rbind.dataset(x1, x2))
})


test_that("'rbind' allows NULL names on all", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, ]
    y <- rbind.dataset(unname(x1), unname(x2))
    expect_equal(y, unname(rbind.dataset(x1, x2)))
})


test_that("'rbind' errors for mismatched key names", {
    x1 <- as_dataset(mtcars[1:3, ])
    x2 <- as_dataset(mtcars[4:5, ])
    keys(x2) <- dataset(foo = keys(x2)[[1]])
    expect_error(rbind.dataset(x1, x2),
                 "arguments 1 and 2 have different key names")
})


test_that("'rbind' errors for mismatched number of keys", {
    x1 <- as_dataset(mtcars[1:3, ])
    x2 <- as_dataset(mtcars[4:5, ])
    keys(x2) <- dataset(foo = keys(x2)[[1]], bar = 4:5)
    expect_error(rbind.dataset(x1, x2),
                 "arguments 1 and 2 have different numbers of keys")
})


test_that("'rbind' can handle named vector arguments", {
    x <- rbind.dataset(first = mtcars[1, , drop = TRUE], mtcars)
    y <- as_dataset(rbind(first = mtcars[1, , drop = TRUE], mtcars))
    expect_equal(x, y)
})


test_that("'rbind' can handle named vector arguments with unnamed matrix", {
    x <- rbind.dataset(first = mtcars[1, , drop = TRUE], unname(mtcars))
    y <- as_dataset(rbind(first = mtcars[1, , drop = TRUE], mtcars))
    expect_equal(x, y)
})


test_that("'rbind' can handle unnamed vector arguments", {
    z <- mtcars
    rownames(z) <- NULL
    x <- rbind.dataset(mtcars[1, , drop = TRUE], z)
    y <- framed(rbind(mtcars[1, , drop = TRUE], z), NULL)
    expect_equal(x, y)
})


test_that("'rbind' can handle unnamed vector arguments with unnamed matrix", {
    z <- mtcars
    rownames(z) <- NULL
    x <- rbind.dataset(mtcars[1, , drop = TRUE], unname(z))
    y <- framed(rbind(mtcars[1, , drop = TRUE], z), NULL)
    expect_equal(x, y)
})


test_that("'rbind' can handle named matrix arguments", {
    z <- mtcars
    rownames(z) <- NULL
    x <- rbind.dataset(mtcars[1, , drop = FALSE], nest = z)
    y <- as_dataset(rbind(mtcars[1, , drop = FALSE], nest = z))
    expect_equal(x, y)
})
