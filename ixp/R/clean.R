##' Interpolate missing values in xts time series.
##'
##' Uses linear or merge interpolation.
##'
##' @param x xts object
##' @param interval the time interval to interpolate (default = "5 min")
##' @param method select the interpolation method: "linear" or "merge".
##'               default: "linear".
##' @param ...
##' @return interpolates xts object
##' @author phil
##' @export interpolate
##' @examples
##'
##' suppressWarnings(library(xts))
##' ts <- as.POSIXct("2018-05-31 15:00:00", tz="UTC")
##' x <- xts(x=1:10, order.by=seq(ts, length=10, by="5 min"))
##' x_broken <- x
##' x_broken[5] <- NA
##' x_fixed <- interpolate(x)
##' all(index(x_fixed) == index(x))
##' all(as.vector(x_fixed) == as.vector(x))
##'
interpolate <- function(x, interval="5 min", method="linear", ...) {
  if(method == "linear")
    linear_interpolate(x, interval, ...)
  else if(method == "merge")
    merge_interpolate(x, interval)
  else {
    warning(paste(method, "not implemented!"))
    x
  }
}

##' Linear interpolation
##'
##' Uses xts na.approx.
##'
##' @param x xts object
##' @param interval default: "5 min"
##' @param ...
##' @return Interpolated xts object
##' @author phil
##' @keywords internal
linear_interpolate <- function(x, interval="5 min", ...) {
  max_diff <- max(diff(index(x)))
  if(as.numeric(max_diff) > 180) {
    warning(paste("ts has gap > 3 hours:", max_diff, "could not interpolate!"))
    return(x)
  }
  xx <- na.approx(x, xout=seq(min(index(x)), max(index(x)), by=interval, ...))
  stopifnot(sum(as.numeric(abs(diff(abs(diff(index(xx))))))) == 0)
  xx
}

##' Interpolate with merge method
##'
##' Alternative interpolation technique.
##'
##' @param data_sub xts object
##' @param interval default: "5 min"
##' @return Interpolated xts object
##' @author joe
##' @keywords internal
merge_interpolate <- function(data_sub, interval="5 min") {
  missing_bins <- which(diff(index(data_sub))!=5)
  xts_merger <- xts(x = 111, order.by = as.POSIXct("1970-01-01 00:00:00 UTC"))
  for (i in 1:length(missing_bins)){
    check_bins <- (missing_bins[i]):(missing_bins[i]+1) #sequences to check.
    time_seq <- (seq(from = index(data_sub[check_bins[1]]),
                     to = index(data_sub[check_bins[2]]), by=interval))
    xts_insert <- xts(x = rep(mean(x = c(data_sub[check_bins[1]],
                                         data_sub[check_bins[2]]), na.rm = TRUE), 
                              length(time_seq)), order.by = time_seq)
    xts_merger <- rbind(xts_merger, xts_insert)
  }
  xts_merger <- xts_merger[2:length(xts_merger)]
  merge.xts(data_sub, time(xts_merger), fill = na.locf)
}

##' Generate minutes since midnight
##'
##'
##' @param ts List of POSIXct times
##' @return Integer list where each element is minute since midnight
##' @author phil
##' @keywords internal
msm <- function(ts, tz="Europe/London") {
  #if(Sys.getenv("TZ") != "UTC") warning("local time is not UTC!")
  h <- as.numeric(strftime(ts, format="%H", tz=tz), origin="1970-01-01", tz=tz)
  m <- as.numeric(strftime(ts, format="%M", tz=tz), origin="1970-01-01", tz=tz)
  60*h + m
}

##' Clean data
##' 
##' Cleans up the loaded data, adds a weekday and time of day column.
##'  
##' @param my_data Loaded data
##' @param date Date to subset
##' @param impute_bins Inteprolate missing values
##' @return data.frame
##' @author joe
##' @export
clean_data <- function(my_data, date, impute_bins=TRUE) {
  # subset the xts by date
  data_sub <- if(missing(date) || is.null(date) || is.na(date)) my_data else my_data[date]
  suppressWarnings({
    require(xts)
    require(TTR)
    require(chron)
    require(dplyr)
  })
  rm(my_data)
  if (impute_bins) {
    data_sub <- interpolate(data_sub)
  }
  # create a weekdays factor vector using the index
  weekdays <- as.factor(weekdays.POSIXt(index(data_sub)))
  # change the level names
  levels(weekdays) <- c("5", "1", "6", "7", "4", "2", "3")
  # note here that monday is NOT number one - number one is Friday as factor
  # levels are assigned alphabetically. Create a times column - this is the MsM
  # column that we will use for times as it avoids the problems that we have
  # with clock changes betwenn GMT and BST.
  time_col <- msm(index(data_sub))
  # throw it all in a dataframe and rename
  data_sub <- cbind(data_sub, as.numeric(x=weekdays), time_col)
  names(data_sub) <- c("Bandwidth", "Weekday", "MsM")
  return(data_sub)
}

##' Smooth and clean
##' 
##' @description In addition to cleaning the data, this will smooth the time-series.
##' 
##' @param my_data Loaded data
##' @param date Data to subset
##' @param SMA_n Size of simple moving average window
##' @param impute_bins Interpolate missing values
##' @return data.frame
##' @author joe
##' @export
smooth_and_clean <- function(my_data, date, SMA_n=12, impute_bins=TRUE){
  # TODO: this needs refactoring (duplicates --^)
  data_sub <- if(missing(date) || is.null(date) || is.na(date)) my_data else my_data[date]
  if (impute_bins) {
    data_sub <- interpolate(data_sub)
  }
  # this function is the same as before but puts a simple moving average step
  # in. n can be changed in the function arguments, but is set to 12 by
  # default.
  smooth_data <- SMA(data_sub, n=SMA_n)
  rm(my_data)
  weekdays <- as.factor(weekdays.POSIXt(index(smooth_data)))
  levels(weekdays) <- c("5", "1", "6", "7", "4", "2", "3")
  # note here that monday is NOT number one - number one is Friday as factor
  # levels are assigned alphabetically. 
  time_col <- msm(index(smooth_data))
  smooth_data <- cbind(smooth_data, as.numeric(x=weekdays), time_col)
  names(smooth_data) <- c("Bandwidth", "Weekday", "MsM")
  return(smooth_data)
}

##' Smooth xts time series in-place.
##'
##' Uses LOESS to smooth time series in place resulting in a non-lagged series.
##' This can not be used as input into any predictive model since it incorporates
##' future values in the smoothing process.
##'
##' @param x xts time-series
##' @param th Smoothing span/window. Default: 0.05 (higher values = more smooth)
##' @return Smoothed xts time series
##' @author phil
##' @export
##' @examples
##'
##' x <- man1["2018-01-01/2018-01-07", 1]
##' plot(x)
##' lines(smooth_in_place(x), col="red", lwd=3")
##'
smooth_in_place <- function(x, th=0.05) {
  x$i <- 1:nrow(x)
  xts(predict(loess(Bandwidth ~ i, data=x, span=th)), order.by=index(x))
}

##' Smooth xts time series.
##'
##' Use EMA to create a (lagged) smooth series.
##'
##' @param x xts time-series
##' @param th Number of periods to look back. Default: 12 (5min*12)
##' @return Smoothed xts time series
##' @author phi
##' @export
##' @examples
##'
##' x <- sco1["2018-01-01/2018-01-07", 1]
##' plot(x)
##' lines(smooth_lagged(x, 24), col="red", lwd=3")
smooth_lagged <- function(x, th=12) EMA(x, n=th)
