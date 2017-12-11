context("grouped")

test_that("'grouped(,NULL, NULL)' puts in list", {
    x <- mtcars
    y <- grouped(x)
    z <- framed(list(list(framed(x))))
    expect_equal(y, z)
})
