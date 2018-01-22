context("record")

test_that("from list", {
    x <- list(2, "foo", NA)
    expect_equal(as.record(x), structure(x, class = "record"))
})


test_that("from named list", {
    x <- list(a = 2, b = "foo", c = NA)
    expect_equal(as.record(x), structure(x, class = "record"))
})


test_that("from record", {
    x <- as.record(list(a = 2, b = "foo", c = NA))
    expect_equal(as.record(x), x)
})


test_that("from empty", {
    x <- list()
    expect_equal(as.record(x), structure(x, class = "record"))
})


test_that("from named empty", {
    x <- structure(list(), names = character())
    y <- as.record(x)
    names(y) <- character()
    expect_equal(as.record(x), y)
})


test_that("from NULL", {
    expect_equal(as.record(NULL), NULL)
})


test_that("with NSE", {
    a <- 2
    b <- "foo"
    c <- NA
    x <- record(a, b, c)
    y <- as.record(list(a = a, b = b, c = c))
    expect_equal(x, y)
})


test_that("with NSE mixed", {
    a <- 2
    c <- NA
    x <- record(a, b = "foo", c)
    y <- as.record(list(a = a, b = "foo", c = c))
    expect_equal(x, y)
})


test_that("setting names with wrong length", {
    x <- record(a = 1, b = 18, c = "foo")
    expect_error(names(x) <- c("a", "b", "c", "d"),
                 "mismatch: `value` length is 4, object length is 3")
    expect_error(names(x) <- "a",
                 "mismatch: `value` length is 1, object length is 3")
})


test_that("setting names with wrong encoding", {
    names <- c("hello", "fa\xE7ile", "world")
    Encoding(names) <- "UTF-8"

    x <- record(a = 1, b = 18, c = "foo")
    expect_error(names(x) <- names,
                 "`value` entry 2 has wrong character encoding")
})


test_that("as.list downcasts", {
    x <- record(a = 1, b = 10)
    expect_equal(as.list(x), x)
})


test_that("as.list converts a non-list", {
    x <- as.record(c(a = 1, b = 10))
    expect_equal(as.record(as.list(x)), record(a = 1, b = 10))
})


test_that("as.vector downcasts", {
    x <- record(a = 1, b = 10)
    expect_equal(as.vector(x, "any"), x)
    expect_equal(as.vector(x, "list"), x)
    expect_equal(as.vector(x, "numeric"), c(1, 10))
})


test_that("as conversion keeps names", {
    x <- record(a = 1, b = 10, c = 0, d = NA)
    expect_equal(as.numeric(x), c(1, 10, 0, NA))
    expect_equal(as.character(x), c("1", "10", "0", NA))
})


test_that("as.data.frame from list", {
    expect_equal(as.data.frame(record(x = 1, b = "hello")),
                 data.frame(x = 1, b = "hello", stringsAsFactors = FALSE))
})


test_that("as.data.frame from atomic", {
    expect_equal(as.data.frame(as.record(c(x = 1, b = 3))),
                 data.frame(x = 1, b = 3))
})


test_that("as.data.frame with invalid names", {
    expect_error(as.data.frame(record(), character(0)),
                 "mismatch: `row.names` has length 0, data has 1 row")
})


test_that("as.data.frame from anonymous", {
    expect_error(as.data.frame(as.record(1:3), character(0)),
                 "mismatch: `row.names` has length 0, data has 1 row")
})


test_that("changing storage mode", {
    x <- record(a = 1, b = 9, c = 2)
    expect_equal(as.vector(x), as.list(x))
    expect_equal(as.vector(x, "list"), as.list(x))
    expect_equal(as.vector(x, "character"), c("1", "9", "2"))
})
