context("lookup")

test_that("'lookup' with scalar key works", {
    i <- lookup(c("Valiant", "Fiat 128", "Delorean"), as_dataset(mtcars))
    j <- match(c("Valiant", "Fiat 128", "Delorean"), rownames(mtcars))
    expect_equal(i, j)              
})


test_that("'lookup' with NULL works", {
    keys <- as_keyset(rownames(mtcars))
    expect_equal(lookup(NULL, keys), NULL)
})


test_that("'lookup' with single multi-component works", {
    keys <- keyset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))
    expect_equal(lookup(list("A", 5), keys), 5)
})


test_that("'lookup' with default works", {
    keys <- keyset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))
    expect_equal(lookup(list("ZZ", 5), keys, default = 7), 7)
})


test_that("'lookup' with many multi-component works", {
    keys <- keyset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))
    i <- lookup(list(c("A", "A", "B", "Z"), c(  5, 100,   1,   3)), keys)
    expect_equal(i, c(5, NA, 9, NA))
})


test_that("'lookup' with many wrong type works", {
    keys <- keyset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))
    i <- lookup(list(c("A", "A", "B", "Z"), c("5", "100", "1", "3")), keys)
    expect_equal(i, c(5, NA, 9, NA))
})


test_that("'lookup' errors for invalid key", {
    keys <- as_keyset(rownames(mtcars))
    expect_error(lookup(NULL, keys, c(-1, 0)), "'default' must have length 1")
    expect_error(lookup(NULL, keys, NaN), "'default' cannot be NaN or Inf")
})


test_that("'lookup' NA string works", {
    keys <- keyset(
        tailnum = c("N172US", "N329JB", "N618JB", "N763JB", "N78511", NA))
    expect_equal(lookup(NA, keys), nrow(keys))
})


test_that("'lookup' NA int works", {
    keys <- keyset(
        tailnum = as.integer(c(172, 329, 618, NA, 763, 78511)))
    expect_equal(lookup(NA, keys), 4)
})


test_that("'lookup' NA double works", {
    keys <- keyset(
        tailnum = c(172, 329.1, 618, NA, 763, 78511))
    expect_equal(lookup(NA, keys), 4)
})


test_that("'lookup' empty works", {
    keys <- as_keyset(list())
    x <- as_dataset(structure(list(), row.names = .set_row_names(5),
                              class = "data.frame"))
    expect_equal(lookup(x, keys), rep(1, nrow(x)))
})
