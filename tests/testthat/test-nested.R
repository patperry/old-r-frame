
context("nested")

test_that("'dataset' can be nested", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(dim(x), c(5, 2))
    expect_equal(names(x), c("a", "b"))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(dim(y), c(5, 3))
    expect_equal(names(y), c("a", "b", "c"))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(dim(z), c(5, 2))
    expect_equal(names(z), c("a", "b"))
})


test_that("'as.list.dataset' works", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(as.list(x), list(a = x$a, b = x$b))
    expect_equal(as.list(x, flatten = TRUE), list(a = x$a, b = x$b))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(as.list(y), list(a = y$a, b = y$b, c = y$c))
    expect_equal(as.list(y, flatten = TRUE),
                 list(a = y$a,
                      b.a = y$b$a,
                      b.b = y$b$b,
                      c = y$c))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(as.list(z), list(a = z$a, b = z$b))
    expect_equal(as.list(z, flatten = TRUE),
                 list(a.a = z$a$a,
                      a.b.a = z$a$b$a, a.b.b = z$a$b$b,
                      a.c = z$a$c,
                      b = z$b))
})


test_that("'as.list.dataset' path index works", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(attr(as.list(x, path = TRUE), "index"), list(1, 2))
    expect_equal(attr(as.list(x, flatten = TRUE, path = TRUE), "index"),
                 list(1, 2))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(attr(as.list(y, path = TRUE), "index"), list(1, 2, 3))
    expect_equal(attr(as.list(y, flatten = TRUE, path = TRUE), "index"),
                 list(1, c(2, 1), c(2, 2), 3))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(attr(as.list(z, path = TRUE), "index"), list(1, 2))
    expect_equal(attr(as.list(z, flatten = TRUE, path = TRUE), "index"),
                 list(c(1, 1), c(1, 2, 1), c(1, 2, 2), c(1, 3), 2))
})


test_that("'as.list.dataset' path names works", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(attr(as.list(x, path = TRUE), "path"), list("a", "b"))
    expect_equal(attr(as.list(x, flatten = TRUE, path = TRUE), "path"),
                 list("a", "b"))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(attr(as.list(y, path = TRUE), "path"), list("a", "b", "c"))
    expect_equal(attr(as.list(y, flatten = TRUE, path = TRUE), "path"),
                 list("a", c("b", "a"), c("b", "b"), "c"))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(attr(as.list(z, path = TRUE), "path"), list("a", "b"))
    expect_equal(attr(as.list(z, flatten = TRUE, path = TRUE), "path"),
                 list(c("a", "a"), c("a", "b", "a"), c("a", "b", "b"),
                      c("a", "c"), "b"))
})
