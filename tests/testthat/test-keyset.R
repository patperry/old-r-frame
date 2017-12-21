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
    expect_error(as_keyset(ds), "argument has a duplicate row \\(4\\)")
})


test_that("'is_keyset' works", {
    ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
                 k2 = rep(1:2, 3))
    ds <- dataset(k1 = rep(c("a", "b", "c"), 2),
                  k2 = rep(1:2, 3))
    expect_false(is_keyset(ds))
    expect_true(is_keyset(ks))
})


test_that("adding a column to keyset downcasts", {
    ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
                 k2 = rep(1:2, 3))
    ds <- as_dataset(ks)
    ks$k3 <- rep(1:3, 2)
    ds$k3 <- rep(1:3, 2)
    expect_equal(ks, ds)
})


test_that("subsetting columns downcasts", {
    ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
                 k2 = rep(1:2, 3))
    ds <- as_dataset(ks)
    ks <- ks[1]
    ds <- ds[1]
    expect_equal(ks, ds)
})


test_that("setting a column downcasts", {
    ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
                 k2 = rep(1:2, 3))
    ds <- as_dataset(ks)
    ks$k1 <- rep(1:3, 2)
    ds$k1 <- rep(1:3, 2)
    expect_equal(ks, ds)
})


test_that("setting a column with [] downcasts", {
    ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
                 k2 = rep(1:2, 3))
    ds <- as_dataset(ks)
    ks[,1] <- rep(1:3, 2)
    ds[,1] <- rep(1:3, 2)
    expect_equal(ks, ds)
})


test_that("setting an entry with [] downcasts", {
    ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
                 k2 = rep(1:2, 3))
    ds <- as_dataset(ks)
    ks[2,1] <- 7
    ds[2,1] <- 7
    expect_equal(ks, ds)
})
