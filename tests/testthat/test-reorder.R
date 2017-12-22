
context("reorder")

test_that("reorder NULL", {
    expect_equal(reorder(mtcars), as_dataset(mtcars))
})


test_that("reorder asc", {
    o <- order(mtcars$mpg)
    expect_equal(reorder(mtcars, mpg), as_dataset(mtcars)[o,])
    expect_equal(reorder(mtcars, asc = mpg), as_dataset(mtcars)[o,])
})


test_that("reorder desc", {
    o <- order(mtcars$mpg, decreasing = TRUE)
    expect_equal(reorder(mtcars, desc = mpg), as_dataset(mtcars)[o,])
})


test_that("reorder asc asc", {
    o <- order(mtcars$gear, mtcars$cyl)
    expect_equal(reorder(mtcars, gear, cyl), as_dataset(mtcars)[o,])
})


test_that("reorder desc desc", {
    o <- order(mtcars$gear, mtcars$cyl, decreasing = c(TRUE, TRUE),
               method = "radix")
    expect_equal(reorder(mtcars, desc = gear, desc = cyl),
                 as_dataset(mtcars)[o,])
})


test_that("reorder asc desc", {
    o <- order(mtcars$gear, mtcars$cyl, decreasing = c(FALSE, TRUE),
               method = "radix")
    expect_equal(reorder(mtcars, gear, desc = cyl),
                 as_dataset(mtcars)[o,])
})


test_that("reorder desc asc", {
    o <- order(mtcars$gear, mtcars$cyl, decreasing = c(TRUE, FALSE),
               method = "radix")
    expect_equal(reorder(mtcars, desc = gear, cyl),
                 as_dataset(mtcars)[o,])
})


test_that("reorder external", {
    set.seed(0)
    y <- rnorm(nrow(mtcars))
    o <- order(mtcars$gear, y)
    expect_equal(reorder(mtcars, gear, y), as_dataset(mtcars)[o,])
})


test_that("reorder 'x'", {
    set.seed(1)
    x <- rnorm(nrow(mtcars))
    o <- order(mtcars$gear, x)
    expect_equal(reorder(mtcars, gear, x), as_dataset(mtcars)[o,])
})


test_that("reorder invalid", {
    expect_error(reorder(mtcars, decreasing = mpg),
                 "named arguments must be 'asc' or 'desc'")
})
