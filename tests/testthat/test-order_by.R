
context("order_by")

test_that("order_by NULL", {
    expect_equal(order_by(mtcars), as_dataset(mtcars))
})


test_that("order_by asc", {
    o <- order(mtcars$mpg)
    expect_equal(order_by(mtcars, mpg), as_dataset(mtcars)[o,])
    expect_equal(order_by(mtcars, asc = mpg), as_dataset(mtcars)[o,])
})


test_that("order_by desc", {
    o <- order(mtcars$mpg, decreasing = TRUE)
    expect_equal(order_by(mtcars, desc = mpg), as_dataset(mtcars)[o,])
})


test_that("order_by asc asc", {
    o <- order(mtcars$gear, mtcars$cyl)
    expect_equal(order_by(mtcars, gear, cyl), as_dataset(mtcars)[o,])
})


test_that("order_by desc desc", {
    o <- order(mtcars$gear, mtcars$cyl, decreasing = c(TRUE, TRUE),
               method = "radix")
    expect_equal(order_by(mtcars, desc = gear, desc = cyl),
                 as_dataset(mtcars)[o,])
})


test_that("order_by asc desc", {
    o <- order(mtcars$gear, mtcars$cyl, decreasing = c(FALSE, TRUE),
               method = "radix")
    expect_equal(order_by(mtcars, gear, desc = cyl),
                 as_dataset(mtcars)[o,])
})


test_that("order_by desc asc", {
    o <- order(mtcars$gear, mtcars$cyl, decreasing = c(TRUE, FALSE),
               method = "radix")
    expect_equal(order_by(mtcars, desc = gear, cyl),
                 as_dataset(mtcars)[o,])
})


test_that("order_by external", {
    set.seed(0)
    y <- rnorm(nrow(mtcars))
    o <- order(mtcars$gear, y)
    expect_equal(order_by(mtcars, gear, y), as_dataset(mtcars)[o,])
})


test_that("order_by 'x'", {
    set.seed(1)
    x <- rnorm(nrow(mtcars))
    o <- order(mtcars$gear, x)
    expect_equal(order_by(mtcars, gear, x), as_dataset(mtcars)[o,])
})


test_that("order_by invalid", {
    expect_error(order_by(mtcars, decreasing = mpg),
                 "named arguments must be 'asc' or 'desc'")
})
