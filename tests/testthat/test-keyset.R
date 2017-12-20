context("keyset")

test_that("'keyset' with two components works", {
    ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
                 k2 = rep(1:2, 3))
    ds <- dataset(k1 = rep(c("a", "b", "c"), 2),
                  k2 = rep(1:2, 3))
    expect_equal(ks, as_keyset(ds))
})


test_that("'keyset' with one component works", {
    ks <- keyset(name = LETTERS[6:12])
    ds <- dataset(name = LETTERS[6:12])
    expect_equal(ks, as_keyset(ds))
})


test_that("'keyset' with duplicate rows errors", {
    ds <- dataset(name = c("a", "b", "c", "a", "e"))
    expect_error(as_keyset(ds),
                 "argument has duplicate entries \\(1 and 4\\)")
})


test_that("'is_keyset' works", {
    ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
                  k2 = rep(1:2, 3))
    ds <- dataset(k1 = rep(c("a", "b", "c"), 2),
                  k2 = rep(1:2, 3))
    expect_false(is_keyset(ds))
    expect_true(is_keyset(ks))
})
