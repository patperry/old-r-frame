
context("as_list")


test_that("'as.list.dataset' works", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(as.list(x), list(a = x$a, b = x$b))
    expect_equal(as.list(x, flat = TRUE), list(a = x$a, b = x$b))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(as.list(y), list(a = y$a, b = y$b, c = y$c))
    expect_equal(as.list(y, flat = TRUE),
                 list(a = y$a,
                      b.a = y$b$a,
                      b.b = y$b$b,
                      c = y$c))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(as.list(z), list(a = z$a, b = z$b))
    expect_equal(as.list(z, flat = TRUE),
                 list(a.a = z$a$a,
                      a.b.a = z$a$b$a, a.b.b = z$a$b$b,
                      a.c = z$a$c,
                      b = z$b))
})


test_that("'as.list.dataset' path index works", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(attr(as.list(x, path = TRUE), "index"), list(1, 2))
    expect_equal(attr(as.list(x, flat = TRUE, path = TRUE), "index"),
                 list(1, 2))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(attr(as.list(y, path = TRUE), "index"), list(1, 2, 3))
    expect_equal(attr(as.list(y, flat = TRUE, path = TRUE), "index"),
                 list(1, c(2, 1), c(2, 2), 3))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(attr(as.list(z, path = TRUE), "index"), list(1, 2))
    expect_equal(attr(as.list(z, flat = TRUE, path = TRUE), "index"),
                 list(c(1, 1), c(1, 2, 1), c(1, 2, 2), c(1, 3), 2))
})


test_that("'as.list.dataset' path names works", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(attr(as.list(x, path = TRUE), "path"), list("a", "b"))
    expect_equal(attr(as.list(x, flat = TRUE, path = TRUE), "path"),
                 list("a", "b"))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(attr(as.list(y, path = TRUE), "path"), list("a", "b", "c"))
    expect_equal(attr(as.list(y, flat = TRUE, path = TRUE), "path"),
                 list("a", c("b", "a"), c("b", "b"), "c"))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(attr(as.list(z, path = TRUE), "path"), list("a", "b"))
    expect_equal(attr(as.list(z, flat = TRUE, path = TRUE), "path"),
                 list(c("a", "a"), c("a", "b", "a"), c("a", "b", "b"),
                      c("a", "c"), "b"))
})


test_that("'as_list' does not create names if none exist", {
    l <- list(1, 2, 3)
    x <- as_dataset(l)
    expect_equal(as.list(x), l)
    expect_equal(as.list(x, flat = TRUE), l)
})


test_that("0-column matrix column", {
    ds <- dataset(x = 1:5, y = matrix(0, 5, 0))
    x <- as.list(ds, flat = TRUE)
    y <- list(x = 1:5, y = rep(list(numeric(0)), 5))
    expect_equal(x, y)
})


test_that("0-column named, matrix column", {
    ds <- dataset(x = 1:5, y = matrix(0, 5, 0))
    x <- as.list(ds, flat = TRUE)
    y <- list(x = 1:5, y = rep(list(numeric(0)), 5))
    expect_equal(x, y)
})
