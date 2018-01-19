context("record")

test_that("from list", {
    x <- list(2, "foo", NA)
    expect_equal(as_record(x), structure(x, class = "record"))
})


test_that("from named list", {
    x <- list(a = 2, b = "foo", c = NA)
    expect_equal(as_record(x), structure(x, class = "record"))
})


test_that("from record", {
    x <- as_record(list(a = 2, b = "foo", c = NA))
    expect_equal(as_record(x), x)
})


test_that("from empty", {
    x <- list()
    expect_equal(as_record(x), structure(x, class = "record"))
})


test_that("from named empty", {
    x <- structure(list(), names = character())
    expect_equal(as_record(x), structure(x, class = "record"))
})


test_that("from NULL", {
    expect_equal(as_record(NULL), as_record(list()))
})


test_that("with NSE", {
    a <- 2
    b <- "foo"
    c <- NA
    x <- record(a, b, c)
    y <- as_record(list(a = a, b = b, c = c))
    expect_equal(x, y)
})


test_that("with NSE mixed", {
    a <- 2
    c <- NA
    x <- record(a, b = "foo", c)
    y <- as_record(list(a = a, b = "foo", c = c))
    expect_equal(x, y)
})


test_that("setting names with wrong length", {
    x <- record(a = 1, b = 18, c = "foo")
    expect_error(names(x) <- c("a", "b", "c", "d"),
                 "mismatch: 'value' has length 4, number of fields is 3")
    expect_error(names(x) <- "a",
                 "mismatch: 'value' has length 1, number of fields is 3")
})


test_that("setting names with wrong encoding", {
    names <- c("hello", "fa\xE7ile", "world")
    Encoding(names) <- "UTF-8"

    x <- record(a = 1, b = 18, c = "foo")
    expect_error(names(x) <- names,
                 "'value', entry 2 has invalid character encoding")
})
