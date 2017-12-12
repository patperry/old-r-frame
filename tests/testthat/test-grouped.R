context("grouped")

test_that("'grouped(,NULL, NULL)' puts in list", {
    y <- grouped(mtcars)
    z <- framed(list(data = list(framed(mtcars))))
    expect_equal(y, z)
})


test_that("'grouped(,integer,NULL)' splits", {
    set.seed(0)
    group <- sample(1:5, nrow(mtcars), replace = TRUE)
    y <- grouped(mtcars, group)
    l <- lapply(split(mtcars, group), framed)
    z <- framed(list(data = l), keys = dataset(group = names(l)))
    expect_equal(y, z)
})
