context("framed")


test_that("'framed' errors for duplicated key", {
    expect_error(framed(mtcars, keys = c("cyl", "cyl")),
                 "`keys` contains duplicates")
})


test_that("'framed' errors for NA key", {
    expect_error(framed(mtcars, keys = c("cyl", NA)),
                 "`keys` contains NA")
})


test_that("'framed' errors for missing key", {
    expect_error(framed(mtcars, keys = c("cyl", "zzz")),
                 "`keys` refers to unknown column \"zzz\"")
})


test_that("'framed' errors for out-of-range", {
    expect_error(framed(mtcars, keys = c(1, -3)),
                 "`keys` refers to column with invalid index \\(-3\\)")
})


test_that("'framed' errors for char key when unnamed", {
    x <- mtcars
    names(x) <- NULL
    expect_error(framed(x, keys = ""),
                 "`keys` refers to named columns but 'names' is NULL")
})


test_that("'framed' allows character() key when unnamed", {
    x <- mtcars
    names(x) <- NULL
    expect_equal(framed(x, keys = character()), framed(x, keys = integer()))
})
