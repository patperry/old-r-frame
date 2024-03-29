context("record subset")

test_that("with positive integers", {
    l <- as.list(letters)
    x <- as.record(l)
    i <- c(1, 9, 3)
    expect_equal(x[i], as.record(l[i]))
})


test_that("with negative integers", {
    l <- as.list(letters)
    x <- as.record(l)
    i <- c(1, 9, 3)
    expect_equal(x[-i], as.record(l[-i]))
})


test_that("with character", {
    l <- record(a = 6, b = 3, c = 10)
    x <- as.record(l)

    expect_equal(x[c("c")], l[c("c")])
    expect_equal(x[c("b", "c")], l[c("b", "c")])
    expect_equal(x[c("b", "c", "c", "d")], l[c("b", "c", "c", "d")])
})


test_that("with logical", {
    x <- record(a = 6, b = 3, c = 10)

    expect_equal(x[c(TRUE, TRUE, FALSE)], x[c(1, 2)])
    expect_equal(x[c(TRUE, FALSE, TRUE)], x[c(1, 3)])
    expect_equal(x[c(FALSE, FALSE, FALSE)], as.record(list(), character()))
    expect_equal(x[c(TRUE, TRUE, TRUE)], x)
})


test_that("with invalid mask", {
    x <- record(a = 6, b = 3, c = 10)
    expect_error(x[TRUE], "mismatch: logical index length is 1, object length is 3")
})


test_that("with NULL, missing", {
    x <- record(a = 6, b = 3, c = 10)
    expect_equal(x[NULL], x)
    expect_equal(x[], x[NULL])
})


test_that("with NA name", {
    x <- as.record(c(4, 3, 2, 1), c("a", NA, "", "z"))
    expect_equal(x[[""]], 3)
    expect_equal(x[[3]], 2)
})


test_that("with empty name", {
    x <- as.record(c(4, 3, 2, 1), c("a", "g", "", "z"))
    expect_equal(x[[""]], 2)
})
