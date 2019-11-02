context("clean")

xts_equal <- function(x1, x2) {
  expect_that(index(x1), equals(index(x2)))
  expect_that(as.vector(x1), equals(as.vector(x2)))
}

test_that("xts interpolation works", {
  suppressWarnings(library(xts))

  ts <- as.POSIXct("2018-05-31 15:00:00", tz="UTC")
  x <- xts(x=1:10, order.by=seq(ts, length=10, by="5 min"))
  colnames(x) <- "bandwidth"
 
  # test 01: should be unchanged
  xts_equal(interpolate(x), x)

  # test 02: 0.5 * 4+6
  x_na <- x
  x_na[5] <- NA
  xts_equal(interpolate(na.omit(x_na)), x)

  # test 03: (2/3 * 3) + (1/3 * 6)  
  x_na[4] <- NA
  xts_equal(interpolate(na.omit(x_na)), x)

  # test 04 - masive gaps (actual data)
  ts = c(as.POSIXct("2017-03-24 10:30:00", tz="UTC"),
         as.POSIXct("2017-03-24 10:35:00", tz="UTC"),
         as.POSIXct("2017-03-24 10:40:00", tz="UTC"),
         as.POSIXct("2017-03-24 10:45:00", tz="UTC"),
         as.POSIXct("2017-03-24 10:45:00", tz="UTC"),
         as.POSIXct("2017-03-24 23:55:00", tz="UTC"),
         as.POSIXct("2017-04-05 10:35:00", tz="UTC"),
         as.POSIXct("2017-04-05 10:40:00", tz="UTC"),
         as.POSIXct("2017-04-05 10:45:00", tz="UTC"),
         as.POSIXct("2017-04-05 10:50:00", tz="UTC"), 
         as.POSIXct("2017-04-05 10:55:00", tz="UTC")) 
  bw = c(119128830, 96462633, 94843774, 88553127, 0, 0, 0, 70958604, 45965504, 54939158,
         72939709)
  x <- xts(x=bw, order.by=ts)
  # expect that it returns orginal data and throws a warning.
  tryCatch({
    xts_equal(interpolate(x), x)
  }, warning=function(w) {
    expect_true(grep("ts has gap > 3 hours", w$message) != 0)
    warning_shot_fired <<- T
  }, finally = {
    expect_true(warning_shot_fired)
  })
})

test_that("msm works", {
  # basic case (in GMT)
  ts <- as.POSIXct("2018-01-31 00:00:00", tz="UTC")
  x <- xts(x=1:10, order.by=seq(ts, length=10, by="5 min"))  
  mSm <- msm(x)
  expect_that(mSm, equals(seq(0, 45, 5)))
  # +1 hour GMT -> BST
  ts <- as.POSIXct("2018-03-25 00:55:00", tz="Europe/London")
  x <- xts(x=1:3, order.by=seq(ts, length=3, by="5 min"))
  mSm <- msm(x)
  expect_that(mSm, equals(c(55, 120, 125)))
})
