
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
