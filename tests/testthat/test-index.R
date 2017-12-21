
context("index")

test_that("$ indexing requires exact", {
    x <- as_dataset(mtcars)
    expect_equal(x$mpg, mtcars$mpg)
    expect_equal(x$mp, NULL)
})

test_that("indexing with column number works", {
    x <- as_dataset(mtcars)
    j <- c(5, 3, 0, 7)
    expect_equal(x[j], as_dataset(mtcars[j]))
})


test_that("indexing with negative column number works", {
    x <- as_dataset(mtcars)
    j <- -c(5, 3, 0, 7)
    expect_equal(x[j], as_dataset(mtcars[j]))
})


test_that("indexing with mixed sign fails", {
    x <- as_dataset(mtcars)
    j <- c(-5, -3, 0, 7)
    expect_error(x[j], "only 0's may be mixed with negative subscripts")
})


test_that("indexing with out of bounds positive fails", {
    x <- as_dataset(mtcars)
    j <- c(5, 3, 2, 100, 1)
    expect_error(x[j], "column selection entry 4 is out of bounds")
})


test_that("indexing with NA fails", {
    x <- as_dataset(mtcars)
    j <- c(seq_along(mtcars)[-1], NA)
    expect_error(x[j], "column selection entry 11 is NA")
})


test_that("indexing with column logical works", {
    x <- as_dataset(mtcars)
    j <- rep(c(TRUE, FALSE), length(mtcars))[seq_along(mtcars)]
    expect_equal(x[j], as_dataset(mtcars[j]))
})


test_that("indexing with NA column logical errors", {
    x <- as_dataset(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1), NA)
    expect_error(x[j], "column selection entry 11 is NA")
})


test_that("indexing with wrong number of logical errors", {
    x <- as_dataset(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1))
    expect_error(x[j],
        "selection mask length \\(10\\) must equal number of columns \\(11\\)")
})


test_that("indexing with row number works", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], as_dataset(mtcars[i,]))
})


test_that("indexing with row number and 'drop' works", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i, , drop = FALSE], as_dataset(mtcars[i,]))
})


test_that("indexing with duplicates should not error if key is set", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19, 5, 7)
    y <- x[i,]
    z <- framed(mtcars[i,],
                as_keyset(list(name = rownames(mtcars)[i], c(1, 1, 1, 1, 2, 1))))
    expect_equal(y, z)
})


test_that("indexing with row names works", {
    x <- as_dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], x[rownames(x)[i],])
})


test_that("indexing with column number works", {
    x <- as_dataset(mtcars)
    j <- c(5, 3, 7)
    expect_equal(x[,j], as_dataset(mtcars[,j]))
})


test_that("'length' does not include key columns", {
    expect_equal(length(as_dataset(mtcars)), length(mtcars))
})


test_that("indexing without keys works", {
    x <- as_dataset(list(a = letters))
    expect_equal(as_dataset(x[1:5,,drop = FALSE]),
                 as_dataset(list(a = letters[1:5])))
})


test_that("indexing with named first key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[row = I("a"), ], x[keys(x)$row == "a", ])
    expect_equal(x[row = I("b"), ], x[keys(x)$row == "b", ])
    expect_equal(x[row = I("d"), ], x[keys(x)$row == "d", ])
})


test_that("indexing with list first key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[list(I("a"), NULL), ], x[keys(x)$row == "a", ])
    expect_equal(x[list(I("b"), NULL), ], x[keys(x)$row == "b", ])
    expect_equal(x[list(I("d"), NULL), ], x[keys(x)$row == "d", ])
})


test_that("indexing with named two first keys works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[row = c("a","c"),], x[keys(x)$row %in% c("a", "c"),])
    expect_equal(x[row = c("c","b"),], x[keys(x)$row %in% c("c", "b"),])
})


test_that("indexing with list two first keys works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[list(c("a","c"), NULL), ], x[keys(x)$row %in% c("a", "c"),])
    expect_equal(x[list(c("c","b"), NULL), ], x[keys(x)$row %in% c("c", "b"),])
})


test_that("indexing with named second key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[col = I("x"), ], x[keys(x)$col == "x",])
    expect_equal(x[col = I("y"), ], x[keys(x)$col == "y",])
    expect_equal(x[col = I("z"), ], x[keys(x)$col == "z",])
})


test_that("indexing with list second key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[list(NULL, I("x")), ], x[keys(x)$col == "x", ])
    expect_equal(x[list(NULL, I("y")), ], x[keys(x)$col == "y", ])
    expect_equal(x[list(NULL, I("z")), ], x[keys(x)$col == "z", ])
})


test_that("indexing with named two second keys works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[col = c("y","x"),], x[keys(x)$col %in% c("y", "x"),])
    expect_equal(x[col = c("x","z"),], x[keys(x)$col %in% c("x", "z"),])
})


test_that("indexing with list two second keys works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[list(NULL, c("y","x")),], x[keys(x)$col %in% c("y", "x"),])
    expect_equal(x[list(NULL, c("x","z")),], x[keys(x)$col %in% c("x", "z"),])
})


test_that("indexing with named key works", {
    a <- matrix(2 * 1:12 - 13, 4, 3)
    rownames(a) <- c("a", "b", "c", "d")
    colnames(a) <- c("x", "y", "z")

    x <- dataset(value = as.vector(a))
    keys(x) <- dataset(row = rownames(a)[row(a)],
                       col = colnames(a)[col(a)])

    expect_equal(x[row = I("b"), ], x[keys(x)$row == "b",])
    expect_equal(x[row = c("a","c")], x[keys(x)$row %in% c("a", "c"),])
    expect_equal(x[col = c("y","x"),], x[keys(x)$col %in% c("y", "x"),])
    expect_equal(x[col = I("x"), ], x[keys(x)$col == "x",])
    expect_equal(x[col = I("y"), row = I("c"), ],
                 x[with(keys(x), col == "y" & row == "c"),])
})


test_that("indexing with name 'x' works", {
    x <- framed(list(x=2:6, y = 6:10), keys = "x")
    expect_equal(x[x = c(3, 5),], x[c(2, 4),])
})


test_that("indexing with keys examples work", {
    value <- matrix(1:12, 4, 3, dimnames = list(c("a", "b", "c", "d"),
                                                c("x", "y", "z")))
    x <- dataset(value = as.vector(value))
    keys(x) <- list(row = rownames(value)[row(value)],
                    col = colnames(value)[col(value)])

    expect_equal(x[dataset(row = c("b", "d"), col = c("z", "x")), ],
                 x[c(10, 4), ])
    expect_equal(x[row = c("b", "d")],
                 x[c(2, 4, 6, 8, 10, 12),])

    expect_equal(x[row = c("b", "d"), col = "y"],
                 `keys<-`(dataset(value = c(6, 8)), dataset(row = c("b", "d"))))
    expect_equal(x[col = "x"],
                 `keys<-`(dataset(value = 1:4), dataset(row = c("a", "b", "c", "d"))))
    expect_equal(x[col = I("x")], x[1:4,])

    expect_equal(x[list(c("b", "d"), "y"),], x[row = c("b", "d"), col = "y"])
    expect_equal(x[list(c("b", "d"), I("y")),], x[row = c("b", "d"), col = I("y")])
})
