context("restrict")


test_that("named first key", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    for (k in c("a", "b", "c")) {
        x <- restrict(ds, row = I(k))
        y <- ds[keys(ds)$row == k, ]
        expect_equal(x, y)
    }
})


test_that("unnamed first key", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    for (k in c("a", "b", "c")) {
        x <- restrict(ds, I(k))
        y <- restrict(ds, row = I(k))
        expect_equal(x, y)
    }
})


test_that("named two first keys", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    x <- restrict(ds, row = c("a", "c"))
    y <- ds[keys(ds)$row %in% c("a", "c"), ]
    expect_equal(x, y)

    x <- restrict(ds, row = c("c", "b"))
    y <- ds[keys(ds)$row %in% c("c", "b"), ]
    expect_equal(x, y)
})


test_that("unnamed two first keys", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    x <- restrict(ds, c("a", "c"))
    y <- restrict(ds, row = c("a", "c"))
    expect_equal(x, y)

    x <- restrict(ds, c("c", "b"))
    y <- restrict(ds, row = c("c", "b"))
    expect_equal(x, y)
})


test_that("named second key", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    for (k in c("x", "y", "z")) {
        x <- restrict(ds, col = I(k))
        y <- ds[keys(ds)$col == k, ]
        expect_equal(x, y)
    }
})


test_that("unnamed second key", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    for (k in c("x", "y", "z")) {
        x <- restrict(ds, NULL, I(k))
        y <- restrict(ds, col = I(k))
        expect_equal(x, y)
    }
})



test_that("named two second keys", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    x <- restrict(ds, col = c("y","x"))
    y <- ds[keys(ds)$col %in% c("y", "x"), ]
    expect_equal(x, y)

    x <- restrict(ds, col = c("x", "z"))
    y <- ds[keys(ds)$col %in% c("x", "z"), ]
    expect_equal(x, y)
})


test_that("unnamed two second keys", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    x <- restrict(ds, NULL, c("y","x"))
    y <- restrict(ds, col = c("y","x"))
    expect_equal(x, y)

    x <- restrict(ds, NULL, c("x", "z"))
    y <- restrict(ds, col = c("x", "z"))
    expect_equal(x, y)
})



test_that("two named keys", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    ds <- dataset(value = as.vector(a))
    keys(ds) <- dataset(row = rownames(a)[row(a)],
                        col = colnames(a)[col(a)])

    expect_equal(restrict(ds, col = I("y"), row = I("c")),
                 ds[with(keys(ds), col == "y" & row == "c"), ])
})


test_that("name 'x'", {
    ds <- dataset(y = 6:10)
    keys(ds) <- dataset(x = 2:6)

    x <- restrict(ds, x = c(3, 5))
    y <- restrict(ds, c(3, 5))
    expect_equal(x, y)
})
