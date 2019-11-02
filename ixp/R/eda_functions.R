##' set_windows
##' 
##' Function to split data into windows. Creates a closure that will subset the
##' data passed to it.
##' 
##' @param windows a list of pairs of numbers. These should be in MsM.
##' @return A function that takes an xts object and returns the defined window.
##' @author Joe Peskett
##' @export

set_windows <- function(windows){
    function(my_data){
        window <- my_data[my_data$MsM > windows[1] & my_data$MsM < windows[2]]
        return(window)           
    }
}

##'select_max
##'
##' Function to collapse a window of recordings into a vector
##' 
##' @param list a list of xts objects from calling set_windows
##' @return a dataframe of the required data
##' @author Joe Peskett
##' @export

select_max <- function(list){
    stopifnot(lapply(list, class)[[1]]==c("xts", "zoo"))
    lapply(X = list, FUN = function(x) coredata(x[which.max(x$Bandwidth)]))
}

##' lag_calculator
##' 
##' Function to calculate the difference in bandwidth and time between the 
##' windows. Be careful on what level of the list this is being run on.
##' @param list a list of recordings that have been selected as maximal from the
##'   select_max() function
##' @return list with differences in bandwidth and MsM
##' @author Joe Peskett
##' @export

lag_calculator <- function(list){
    lapply(list, function(x) x[[2]] - x[[1]])
}

##' Peak finder
##' 
##' This algorithm locates the peaks in a time-series. Not that at this time you
##' can only give one day to the function at a time - you can iterate through a 
##' collection of dates using a combination of sapply, do.call and rbind.
##' 
##' @param my_data A list or xts object. Currently requires a time-based index 
##'   and columns named MsM(Minutes since midnight) and Bandwidth.
##' @param windows in list form
##' @return One long list containing the location of all the peaks and the lags
##'   between these and the dates for each day that has been calculated
##' @author Joe Peskett
##' @export
peak_finder <- function(my_data, windows = list(c(15.5*60,18.5*60), 
                                                c(18.5*60, 21.5*60))) {  
    
    function_list <- lapply(X = windows, FUN = set_windows)
    peaks <-lapply(split(my_data, "days"),
                   FUN = function(x) lapply(function_list, function(f) f(x)) %>% 
                       select_max())
    gaps <- lag_calculator(peaks)#might remove this as it's not vital that this function is here.
    list(peaks, gaps, unique(as.Date(index(my_data))))
}

##' select_date
##' 
##' Function for selecting the a date. This has been formatted to work on both 
##' peak_finder lists and feature_frame lists.
##' 
##' @param date This should be a character vector of a single or multiple dates 
##'   in form "yyyy-mm-dd". The function is restricted to take lists that are
##'   the output of feature_frame or peak_finder.
##' @return A function that takes a list and returns a list.
##' @author Joe Peskett
##' @export
select_date <- function(date = "2016-01-01"){
   #stopifnot(class(date)=="character")
    function(list){
        if(length(list) ==3){
            positions<- which(list[[length(list)]] %in% as.Date(date))
            lapply(positions, 
                   function(y) lapply(seq(length(list)), 
                                                 function(x) list[[x]][[y]]))
        }else if(length(list)==2){
            positions <- which(list[[length(list)]] %in% as.Date(date))
            lapply(positions, 
                   function(y) lapply(seq(length(list[[1]])), 
                                                 function(x) c(list[[1]][[x]][[y]])))
        }else{
            print("List of incorrect length - has data been processed correctly?")
        }
    }
}

##' Feature finder
##' 
##' Function will find the time and recording of the first record to cross a 
##' defined threshold. This threshold is set to be a multiple of the minimum 
##' found for the dataset. This function is called by feature_frame, and it is
##' recommended that you use feature_frame over feature_finder.
##' 
##' @param my_df an xts object of a single day. See details above for info.
##' @param multiplier a given multiplier
##' @return a vector of the Bandwidth and the MsM value for that multiple.
##' @author Joe Peskett
##' @export
feature_finder <- function(multiplier){
    function(my_xts){
        trough <- coredata(my_xts[which.min(my_xts$Bandwidth)])[,c(1,3)]
        if(NA %in% my_xts$Bandwidth){
            multiple <- print("NA in data and exluded")
        }else if(max(my_xts$Bandwidth)<multiplier*trough[1]){
            print(paste("Warning: multiplier", multiplier, "exceeds maximum value of data"))
            stop(call. = T)
        }else{
        multiple <- xts2df(xts = my_xts) %>% 
            filter(MsM > trough[2]) %>% 
            filter(Bandwidth>(trough[1]*multiplier)) %>% 
            top_n(n = -1, wt = MsM) %>% 
            select(Bandwidth, MsM)
        return(multiple)}
    }
}
##' time_2_increase
##' 
##' Takes the result of feature_frame and returns a list of differences in MsM 
##' and Bandwidth - note that the dates will be removed from this list.
##' 
##' @param list Takes
##' @return A list with a space for each day. Within each day there will be two
##'   spaces, one for MsM and another for Bandwidth.
##' @author Joe Peskett
##' @export
time_2_increase <- function(list){
    if(length(list)==1){
        list(
            MsM = list %>% extract_features(2,1) %>% diff_find(),
            Bandwidth = list %>% extract_features(1, 1) %>% diff_find()
        )}else if(length(list)>1){
            lapply(seq(length(list)), function(x) 
                list(
                    MsM = list %>% extract_features(2, x) %>% diff_find(),
                    Bandwidth =  list%>% extract_features(1, x) %>% diff_find()
                )
            )
        }
}

##' find_diff
##' 
##' @param list takes a list of multiples of a single day for a single variable.
##'   Used within the time_2_increase function and not recommeneded for use
##'   outside of this funciton.
##' @return a vector of differnces
##' @author Joe Peskett
##' @export
diff_find <- function(list){
    diff(unlist(x = list, use.names = F, recursive = T))
}

##' extract_features
##' 
##' An easy way to extract required components of the list of multiples. Not
##' recommended for use outside time_2_increase function.
##' @param list takes a list of multiples of a single day.
##' @return a list of the required day and variable
##' @author Joe Peskett
##' @export
extract_features <- function (list, i, d){
    lapply(seq(list[[1]]), function(x) list[[d]][[x]][i])
}

##' gradients
##' 
##' Returns a list of gradients from a list of differences in Bandwidth and MsM
##' @param list output of time_2_increase
##' @return a list for each day of the gradients
##' @author Joe Peskett
##' @export
gradients <- function(list){
    lapply(seq(length(list)), function(x) grads(list, x))
}

##' grads
##' 
##' Helper function for gradients. May be useful elsewhere
##' @param list a list holding a single day of differences in Bandwidth and MsM
##' @return vector of gradients for that given day.
##' @author Joe Peskett
##' @export 
grads <- function(list, i){Map("/", list[[i]][2], list[[i]][1])}

##' Feature frame
##' 
##' A functional wrapper for feature_finder. Takes any number of multipliers, 
##' however if multiples are not present in a given day then error will be
##' thrown stating this.
##' 
##' @param my_xts xts object that holds the data. Note that this can be the 
##'   entire xts, not just a single day.
##' @param multipliers Numerical vector of any length. Will break if multiplier 
##'   exceeds the maximum available in each day of data.
##' @return A large list. The first level dictates the level of the multiplier, 
##'   second is the recording number, third is either 1 for the trough record or
##'   2 for the given multiplier.
##' @author Joe Peskett
##' @export
feature_frame <- function(my_xts, multipliers = c(1, 1.25, 1.5, 1.75, 2)) {
    
    multi_list <- lapply(multipliers, feature_finder)
    res <- lapply(multi_list, function(f) lapply(split(my_xts, "days"), f))
    list(mults = res,  Date = unique(as.Date(index(my_xts))))
    
}

##' xts2datframe
##' 
##' Quickly take a subset by dates and returns a dataframe - used as a utilty 
##' for plotting
##' 
##' @param xts an xts object
##' @param date a character vector that xts can be subsetted with. Either 
##'   "yyyy-mm-dd" for a day, "yyyy-mm-dd hh:MM" for a time, or 
##'   "yyyy-mm-dd/yyyy-mm-dd" for a period of time. See xts subsetting rules for
##'   more information. This can be left blank.
##' @return dataframe with the subsetted data
##' @author Joe Peskett
##' @export

xts2df <- function(xts, date){data.frame("Time" = index(xts[date]), coredata(xts[date]))}

##' MsM2Date
##' 
##' Quickly move from MsM to Date value - used as a utility for plotting. Not
##' ideal that we require the shunt argument but it's just a hack for the time
##' being.
##' 
##' @param date A date as a character in format "yyyy-mm-dd"
##' @param time Time in number of minutes since midnight
##' @param shunt A number reflecting the number of seconds to shunt the values 
##'   by. Default is -3600 to bring it back an hour, relecting the change in 
##'   clocks.
##' @return  A POSIXct object, tz = UTC, with minutes added on.
##' @author Joe Peskett
##' @export
MsM2Date <- function(date, MsM, shunt = -3600){as.POSIXct(date, tz = "UTC")+(MsM*60)-3600}

