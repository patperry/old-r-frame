context("duplicated")

test_that("fixes bugs in base R", {
    x <- as_dataset(data.frame(x = c(.15, .1 + .05), y = "a"))
    expect_equal(unique(x), as_keyset(x))
})


test_that("works for empty dataset", {
    x <- as_dataset(structure(list(), row.names = .set_row_names(3),
                              class = "data.frame"))
    expect_equal(unique(x), as_keyset(x[1, ]))
})
