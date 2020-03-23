##' Plot bandwidth time-series as heatmap
##'
##' Alternative time-series visualisation.
##'
##' @param ts Bandwidth time series
##' @param col.scheme Default: inferno
##' @return ggplot
##' @author phil
##' @export
##' @examples
##' \dontrun{
##' ts_heatmap(lon1["2017"])
##' }
##'
ts_heatmap <- function(ts, col.scheme="inferno") {
  g <- ggplot(ts, aes(x=strftime(index(ts), format="%D"), y=factor(MsM)))
  g <- g + geom_tile(aes(fill=Bandwidth)) + scale_fill_viridis(option=col.scheme)
  g
}

##' Plot 52-week vs average-MsM heatmap
##'
##' Average MsM value for each week.
##'
##' @param ts Bandwidth time series
##' @param year Year to subset
##' @param col.scheme Default: inferno
##' @param daily Default: FALSE. Average by day of week, else MsM.
##' @param border Default: NA. Colour of cell border.
##' @return ggplot
##' @author phil
##' @export
##' @examples
##' \dontrun{
##' weekly_heatmap(lon2, year="2017", daily=FALSE)
##' dev.new()
##' weekly_heatmap(lon2, year="2017", daily=TRUE,  border="white", col.scheme="viridis")
##' }
weekly_heatmap <- function(ts, year, col.scheme="inferno", daily=FALSE, border=NA) {
  sub_year <- ts[year]
  if(daily) {
    avg_by <- weekdays(index(sub_year))
    ord_by <- rev(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                    "Friday", "Saturday"))
  } else {
    avg_by <-sub_year$MsM
    ord_by <- 0:1435
  }
  x <- with(sub_year, {
    tapply(sub_year$Bandwidth,
           list(factor(strftime(index(sub_year), "%W")), avg_by), mean)
  })
  df <- my_melt(x)
  df$col <- ordered(df$col, ord_by)
  g <- ggplot(df, aes(x=row, y=factor(col)))
  g <- g + geom_tile(aes(fill=val), col=border) + scale_fill_viridis(option=col.scheme)
  g
}
##' Peak plot
##' 
##' This plots the located peaks in a time-series.
##' 
##' @param my_xts an XTS object containing the date that you aim to plot.
##' @param date a single date as a character in form "yyyy-mm-dd"
##' @param windows Where to look for peaks, default is the same of peak_finder
##' @return A ggplot of the selected date, correctly labelled with lines for the
##'   peaks and detailing the distance between the peaks.
##' @author Joe Peskett
##' @export
peak_plot <- function(my_xts, date, windows = list(c(15.5*60,18.5*60), c(18.5*60, 21.5*60))) { 
    my_data <- data.frame(coredata(my_xts[date])[,c(1,3)])
    my_date <- select_date(date)
    details <- my_xts[date] %>%
        peak_finder(windows = windows)
    ggplot(data = my_data, aes(x = MsM, y = Bandwidth)) + geom_point() +
        geom_hline(yintercept = sapply(seq(length(windows)),function(x) unlist(extract_features(details,x,1))[1])) + 
        geom_vline(xintercept = sapply(seq(length(windows)),function(x) unlist(extract_features(details,x,1))[3])) +
        ggtitle(label = paste("Plot of", date), 
                subtitle = paste("Distance between peaks =",
                                 unlist(extract_features(details, 3, 2)), "minutes"))
}
##' feature_plot
##' 
##' Plot a single day with multipliers plotted as lines to as evidence of the 
##' function working. Feature_frame is called within the function and 
##' multipliers are passed directly from this function. Default multipliers are
##' the same as the feature_frame() function.
##' 
##' @param my_xts Xts object holding the data to be plotted
##' @param date a single date as a character in form "yyyy-mm-dd"
##' @param multipliers to be passed to feature_frame()
##' @return A ggplot object with the multipliers added for proof
##' @author Joe Peskett
##' @export
feature_plot <- function(my_xts, date, multipliers = c(1,1.25, 1.5, 1.75, 2)) { 
    my_data <- data.frame(coredata(my_xts[date])[,c(1,3)])
    my_date <- select_date(date)
    details <- my_xts[date] %>%
        feature_frame(multipliers = multipliers) %>% 
        my_date()
    ggplot(data = my_data, aes(x = MsM, y = Bandwidth)) + geom_point() +
        geom_hline(yintercept = unlist(extract_features(details, i = 1, d = 1))) + 
        geom_vline(xintercept = unlist(extract_features(details, i = 2, d = 1))) +
        ggtitle(label = paste("Plot of", date))
}
