##' Load IXP data
##' 
##' Avoids having the conversion to xts in the main notebooks.
##' Note this will set timezone to UTC.
##'
##' Can set timezone of xts object with:
##' \code{indexTZ(man1) <- "Europe/London"}
##' 
##' @param path Location of CSV file
##' @return An XTS data.frame
##' @author joe
##' @export
load_data <- function(path) {
  # read the csv file
  data <- read.csv(file=path, header=FALSE)
  # convert first column in POSIXct
  data[, 1] <- as.POSIXct(data[, 1], tz="UTC", origin="1970-01-01")
  # create xts object using the POSIXct column as the index
  xts_data <- xts(x=data[, 2], order.by=data[, 1], tzone="UTC")
  colnames(xts_data) <- "bandwidth"
  xts_data
}

##' load csv, process and save in data/
##'
##' convenience function.
##'
##' @return 
##' @author phil
##' @keywords internal
##' @export
load_and_save <- function(base_dir="",
                          ixps=c("lon1", "lon2", "man1", "sco1", "nva1", "car1")) {
  for(ixp in ixps) {
    cat(paste(ixp, "...\n"))
    x <- load_data(paste0(base_dir, "inst/extdata/csv/", ixp, ".csv"))
    x <- clean_data(x)
    assign(ixp, x)
    rm(x)
    do.call(save, args=list(ixp, compress="xz",
			    file=paste0(base_dir, "data/", ixp, ".data.RData")))
  }
}
