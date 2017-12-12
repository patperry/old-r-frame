context("grouped")

test_that("'grouped(,NULL, NULL)' puts in list", {
    x <- grouped(mtcars)
    y <- framed(list(list(framed(mtcars))))
    expect_equal(x, y)
})


test_that("'grouped(,integer,NULL)' splits", {
    set.seed(0)
    group <- sample(1:5, nrow(mtcars), replace = TRUE)
    x <- grouped(mtcars, dataset(group = group))
    l <- lapply(split(mtcars, group), framed)
    y <- framed(list(l), keys = dataset(group = names(l)))
    expect_equal(x, y)
})


if (FALSE) {
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
}
