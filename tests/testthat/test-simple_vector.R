context("simple_vector")

test_that("converts numeric as-is", {
    x <- c(1, 1.3, .Machine$integer.max + 1, 23, NaN, Inf)
    expect_true(is_simple_vector(x))
    expect_equal(x, as_simple_vector(x))
})

test_that("converts numeric to integer by truncating", {
    x <- c(1, 1.3, .Machine$integer.max + 1, 23, NaN, Inf)
    y <- as_simple_vector(x, integer())

    i <- c(1L, 1L, NA, 23, NA, NA)
    attr(i, "failed") <- c(3L, 6L)
    expect_equal(y, i)
})

test_that("says factor is not simple", {
    x <- factor(c("5", "4", "3", "2", "1"), c(1, 2, 5, 3, 4))
    expect_false(is_simple_vector(x))
})


test_that("converts factor to character", {
    x <- factor(c("5", "4", "3", "2", "1"), c(1, 2, 5, 3, 4))
    y <- as_simple_vector(x)
    expect_equal(y, as.character(x))
})


test_that("converts factor like integer using labels", {
    x <- factor(c("5", "4", "3", "2", "1"), c(1, 2, 5, 3, 4))
    y <- as_simple_vector(x, integer())
    expect_equal(y, as.integer(as.character(x)))
})


test_that("POSIXct is simple, POSIXlt is not", {
    ct <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    lt <- as.POSIXlt(ct)
    expect_true(is_simple_vector(ct))
    expect_false(is_simple_vector(lt))
})


test_that("times get converted to POSIXct, preserving time zone", {
    ct <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    lt <- as.POSIXlt(ct)
              
    expect_equal(ct, as_simple_vector(ct))
    expect_equal(ct, as_simple_vector(lt))
})


test_that("times get converted to Dates using their own time zone", {
    ct <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    lt <- as.POSIXlt(ct)
              
    expect_equal(as_simple_vector(ct, as.Date(NA)), as.Date("2017-12-25"))
    expect_equal(as_simple_vector(lt, as.Date(NA)), as.Date("2017-12-25"))
})


test_that("datetime without zone gets converted as like", {
    ct <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    x <- as_simple_vector("2017-07-04 15:00:12", ct)
    y <- as.POSIXct("2017-07-04 15:00:12", "America/Los_Angeles")
    expect_equal(x, y)
})


test_that("numeric gets converted to POSIXct assuming origin 1970-01-01 UTC", {
    ct <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    expect_equal(as_simple_vector(as.numeric(ct), ct), ct)
})


test_that("homogeneous list", {
    l <- list(1, 2, 17)
    expect_equal(as_simple_vector(l), as.numeric(l))
})


test_that("homogeneous boxed list", {
    l <- list(list(1), list(2), list(17))
    expect_equal(as_simple_vector(l), c(1, 2, 17))
})


test_that("heterogeneous list", {
    l <- list(1, c(1, 3), 1)
    expect_error(as_simple_vector(l),
                 "cannot convert heterogeneous list to simple vector")
})
