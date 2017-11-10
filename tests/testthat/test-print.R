
context("print")

test_that("'print.dataset' can print all rows", {
    d <- data.frame(x = 1:50)

    expect_equal(capture_output(print.dataset(d, -1)),
                 capture_output(print.dataset(d, .Machine$integer.max)))

    expect_equal(capture_output(print.dataset(d, NULL)),
                 capture_output(print.dataset(d, .Machine$integer.max)))

    expect_error(print.dataset(d, NA), "'rows' cannot be NA")
})


test_that("'print.dataset' produces the same results on ASCII", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    dr <- d
    dr$ch <- paste0(d$ch, " ")

    dq <- d
    names(dq) <- c("x", "f  ", "ch ")

    expect_equal(capture_output(print.dataset(d)),
                 capture_output(print(dr)))
    expect_equal(
        capture_output(print.dataset(d, quote = TRUE)),
        capture_output(print(dq, quote = TRUE)))
})


test_that("'print.dataset' handles row names", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    row.names(d) <- LETTERS[1:10]

    dr <- d
    dr$ch <- paste0(d$ch, " ")

    dq <- d
    names(dq) <- c("x", "f  ", "ch ")

    expect_equal(capture_output(print.dataset(d)),
                 capture_output(print(dr)))

    expect_equal(
        capture_output(print.dataset(d, quote = TRUE)),
        capture_output(print(dq, quote = TRUE)))
})


test_that("'print.dataset' handles NA in column names", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    dr <- d
    dr$f <- paste0(d$f, " ")
    dr$ch <- paste0(d$ch, " ")

    names(d) <- c("x", NA, "ch")
    names(dr) <- c("x", "<NA>", "ch")

    expect_equal(capture_output(print.dataset(d)),
                 capture_output(print.dataset(dr)))
})


test_that("'print.dataset' handles NA elements", {
    d <- data.frame(x = NA, ch = I(NA_character_),
                    f = as.factor(NA_character_))
    dr <- d
    names(dr) <- c("x", "ch  ", "f   ")

    dq <- d
    names(dq) <- c("x", "ch", "f ")

    dfoo <- d
    names(dfoo) <- c("x", "ch ", "f  ")

    expect_equal(capture_output(print.dataset(d)),
                 capture_output(print(dr)))

    expect_equal(capture_output(print.dataset(d, quote = TRUE)),
                 capture_output(print(dq, quote = TRUE)))

    expect_equal(capture_output(print.dataset(d, na.print = "foo")),
                 capture_output(print(dfoo, na.print = "foo")))

    expect_equal(capture_output(print.dataset(d, na.print = "foo",
                                                   quote = TRUE)),
                 capture_output(print(dfoo, na.print = "foo", quote = TRUE)))
})


test_that("'print.dataset' handles NA column names", {
    x <- list(1)
    names(x) <- NA
    d <- structure(x, row.names="foo", class = "data.frame")
    d2 <- d
    names(d2) <- "<NA>"
    expect_equal(capture_output(print.dataset(d)),
                 capture_output(print.dataset(d2)))
})


test_that("'print.dataset' handles empty data frames", {
    # no row or column names
    d1 <- data.frame()
    expect_equal(capture_output(print.dataset(d1)),
                 "data frame with 0 columns and 0 rows")

    # no row names
    d2 <- data.frame(a = integer(), b = integer(), "\n" = logical(),
                     check.names = FALSE)
    expect_equal(capture_output(print.dataset(d2)), "a b \\n\n(0 rows)")

    # columns but no column names
    d3 <- structure(list(integer(), integer()),
                    class = "data.frame", row.names = c(NA, 0))
    expect_equal(capture_output(print.dataset(d3)),
                 "data frame with 2 columns and 0 rows")
})



test_that("'print.dataset' can right justify", {
    d <- data.frame(ch = c("a", "ab", "abc"))

    expect_equal(capture_output(print.dataset(d, right = TRUE)),
                 capture_output(print(d, right = TRUE)))
})



test_that("'print.dataset' can wrap 4 columns", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(
        title = "The Declaration of Independence of The United States of America",
        author = "Founding Fathers",
        language = "English",
        text = "The Declaration of Independence of The United States of America\n\n\nWhen in the course of human events")

    lines <- c(
'  title                                                          ',
'1 The Declaration of Independence of The United States of America',
'  author           language text                                                ',
'1 Founding Fathers English  The Declaration of Independence of The United Sta...')

    expect_equal(strsplit(capture_output(print.dataset(x), width = 80),
                          "\n")[[1]],
                 lines)
})
