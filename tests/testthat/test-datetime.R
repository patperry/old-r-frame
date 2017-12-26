context("datetime")

test_that("'as_date' ignores time zone for POSIXct", {
    x1 <- as.POSIXct("2017-12-25 17:23:45", tz = "UTC")
    x2 <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    x3 <- as.POSIXct("2017-12-26 01:23:45", tz = "UTC")
    x4 <- as.POSIXct("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    x <- list(x1, x2, x3, x4)
    y <- lapply(x, as_date)
    z <- list(as.Date("2017-12-25"), as.Date("2017-12-25"),
              as.Date("2017-12-26"), as.Date("2017-12-26"))
    expect_equal(y, z)
}


test_that("'as_date' ignores time zone for POSIXlt", {
    x1 <- as.POSIXlt("2017-12-25 17:23:45", tz = "UTC")
    x2 <- as.POSIXlt("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    x3 <- as.POSIXlt("2017-12-26 01:23:45", tz = "UTC")
    x4 <- as.POSIXlt("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    x <- list(x1, x2, x3, x4)
    y <- lapply(x, as_date)
    z <- list(as.Date("2017-12-25"), as.Date("2017-12-25"),
              as.Date("2017-12-26"), as.Date("2017-12-26"))
    expect_equal(y, z)
})


test_that("'as_posixct' works same on POSIXct, POSIXlt", {
    x1 <- as.POSIXct("2017-12-25 17:23:45", tz = "UTC")
    x2 <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    x3 <- as.POSIXct("2017-12-26 01:23:45", tz = "UTC")
    x4 <- as.POSIXct("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    x <- list(x1, x2, x3, x4)

    y1 <- as.POSIXlt("2017-12-25 17:23:45", tz = "UTC")
    y2 <- as.POSIXlt("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    y3 <- as.POSIXlt("2017-12-26 01:23:45", tz = "UTC")
    y4 <- as.POSIXlt("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    y <- list(y1, y2, y3, y4)

    expect_equal(lapply(x, as_posixct), lapply(y, as_posixct))
})
