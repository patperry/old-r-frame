context("grouped")

test_that("'grouped(NULL)' works", {
    expect_equal(grouped(NULL), NULL)
})

test_that("'grouped(,NULL, NULL)' puts in list", {
    x <- grouped(mtcars)
    y <- framed(list(list(framed(mtcars))))
    expect_equal(x, y)
})


test_that("'grouped(,integer(), NULL)' works", {
    x <- grouped(mtcars, integer())
    y <- grouped(mtcars)
    expect_equal(x, y)
})


test_that("'grouped(,character(), NULL)' works", {
    x <- grouped(mtcars, character())
    y <- grouped(mtcars)
    expect_equal(x, y)
})


test_that("'grouped(,integer,NULL)' splits", {
    set.seed(0)
    group <- sample(1:5, nrow(mtcars), replace = TRUE)
    x <- grouped(mtcars, dataset(group))
    l <- lapply(split(mtcars, group), framed)
    y <- framed(list(l), keys = dataset(group = as.integer(names(l))))
    expect_equal(x, y)
})


test_that("'grouped(,name,NULL)' removes column", {
    x <- grouped(mtcars, c("gear", "cyl"))
    by <- framed(mtcars)[, c("gear", "cyl")]
    x1 <- framed(mtcars)[, -match(c("gear", "cyl"), names(mtcars))]
    l <- split(x1, by)
    keys <- matrix(as.numeric(unlist(strsplit(names(l), "\\."))),
                   ncol = 2, byrow = 2,
                   dimnames = list(NULL, c("gear", "cyl")))
    y <- framed(list(l), keys = framed(keys))
    expect_equal(x, y[keys(x),])
})


test_that("'grouped(,char,NULL)' removes column", {
    x <- grouped(mtcars, c(5, 1, 2))
    y <- grouped(mtcars, names(mtcars)[c(5, 1, 2)])
    expect_equal(x, y)
})


test_that("'grouped(,integer(0),NULL)' works", {
    x <- grouped(mtcars[0,], dataset(group = integer()))
    expect_equal(x, NULL)
})


test_that("'grouped' works with scalar function on whole", {
    xg <- grouped(mtcars)
    x <- framed(sapply(xg[[1]], nrow), keys(xg))
    y <- grouped(mtcars, do = nrow)
    expect_equal(x, y)
})


test_that("'grouped' works with scalar function on parts", {
    set.seed(0)
    group <- sample(1:5, nrow(mtcars), replace = TRUE)
    xg <- grouped(mtcars, dataset(group = group))
    x <- framed(sapply(xg[[1]], nrow), keys(xg))

    y <- grouped(mtcars, dataset(group = group), nrow)
    expect_equal(x, y)
})
