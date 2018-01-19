context("record entry")

test_that("numeric index", {
    x <- record(a = 1, b = 4, c = 19)

    expect_equal(x[[1]], 1)
    expect_equal(x[[2]], 4)
    expect_equal(x[[3]], 19)
    expect_equal(x[[4]], NULL)
    expect_equal(x[[NA]], NULL)
})

test_that("missing index", {
    x <- record(a = 1, b = 4, c = 19)

    expect_error(x[[NULL]], "missing index")
    expect_error(x[[]], "missing index")
})

test_that("invalid index", {
    skip("Not implemented")
    x <- record(a = 1, b = 4, c = 19)

    expect_error(x[[-1]] <- 1, "invalid index \\(-1\\)")
    expect_error(x[[0]] <- 1, "invalid index \\(0\\)")
    expect_error(x[[Inf]] <- 1, "invalid index \\(Inf\\)")
})
