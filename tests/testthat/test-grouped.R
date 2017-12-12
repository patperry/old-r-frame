context("grouped")

test_that("'grouped(,NULL, NULL)' puts in list", {
    x <- mtcars
    y <- grouped(x)
    z <- framed(list(list(framed(x))))
    expect_equal(y, z)
})


test_that("'grouped(,integer,NULL)' splits", {
    set.seed(0)
    x <- mtcars
    g <- sample(1:5, nrow(x), replace = TRUE)
    y <- grouped(x, dataset(group = g))
    l <- lapply(split(x, g), framed)
    z <- framed(list(l), keys = dataset(group = names(l)))
    expect_equal(y, z)
})
