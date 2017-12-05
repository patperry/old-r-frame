
context("print")

test_that("'print.dataset' can print all rows", {
    d <- data.frame(x = 1:50)

    expect_equal(capture_output(print(as_dataset(d), -1)),
                 capture_output(print(as_dataset(d), .Machine$integer.max)))

    expect_error(print(as_dataset(d), NA), "'rows' cannot be NA")
})


test_that("'print.dataset' produces the same results on ASCII", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    dr <- d
    dr$ch <- paste0(d$ch, " ")

    dq <- d
    names(dq) <- c("x", "f  ", "ch ")

    expect_equal(capture_output(print(as_dataset(d))),
                 capture_output(print(dr)))
    expect_equal(
        capture_output(print(as_dataset(d), quote = TRUE)),
        capture_output(print(dq, quote = TRUE)))
})


test_that("'print.dataset' handles NA elements", {
    d <- data.frame(x = NA_real_, ch = I(NA_character_),
                    f = as.factor(NA_character_))
    dr <- d
    names(dr) <- c("x", "ch  ", "f   ")

    dq <- d
    names(dq) <- c("x", "ch", "f ")

    dfoo <- d
    names(dfoo) <- c("x", "ch ", "f  ")

    expect_equal(capture_output(print(as_dataset(d))),
                 capture_output(print(dr)))

    expect_equal(capture_output(print(as_dataset(d), quote = TRUE)),
                 capture_output(print(dq, quote = TRUE)))

    expect_equal(capture_output(print(as_dataset(d), na.print = "foo")),
                 capture_output(print(as_dataset(dfoo), na.print = "foo")))

    expect_equal(capture_output(print(as_dataset(d), na.print = "foo",
                                                     quote = TRUE)),
                 capture_output(print(as_dataset(dfoo), na.print = "foo",
                                      quote = TRUE)))
})


test_that("'print.dataset' handles empty data frames", {
    # no row or column names
    d1 <- data.frame()
    expect_equal(capture_output(print(as_dataset(d1))),
                 "data frame with 0 columns and 0 rows")

    # no row names
    d2 <- data.frame(a = integer(), b = integer(), "\n" = logical(),
                     check.names = FALSE)
    expect_equal(capture_output(print(as_dataset(d2))), "a b \\n\n(0 rows)")
})



test_that("'print.dataset' ignores 'right' argument", {
    d <- data.frame(ch = c("a", "ab", "abc"))

    expect_equal(capture_output(print(as_dataset(d), right = TRUE)),
                 capture_output(print(as_dataset(d), right = FALSE)))
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
'.',
'  author           language text                                                ',
'1 Founding Fathers English  The Declaration of Independence of The United Sta...')

    expect_equal(strsplit(capture_output(print.dataset(x, sections = 2),
                                         width = 80),
                          "\n")[[1]],
                 lines)
})


test_that("'print.dataset can print NA columns", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(title = c("For the Independent Journal",
                           "From the New York Packet"),
                 date = as.Date(c(NA, "1787-11-20")),
                 author = c("Hamilton", "Hamilton"),
                 text = c("To the People of the State of New York",
                          "To the People of the State of New York"))
    lines <- c(
'  title                       date       author   text                       ',
'1 For the Independent Journal <NA>       Hamilton To the People of the Sta...',
'2 From the New York Packet    1787-11-20 Hamilton To the People of the Sta...')

    expect_equal(strsplit(capture_output(print.dataset(x), width = 77),
                          "\n")[[1]], lines)
})
