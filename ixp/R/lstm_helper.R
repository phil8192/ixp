##' tstep_lag
##' 
##' Function to aid in the lagging of data to create x and y sets ready for
##' LSTM.
##' @param vector of univariate time series data
##' @return a function that takes a time step (tstep). This function will then 
##'   create a list of vectors of indices that can be used to subset the 
##'   original input vector into the length of tstep.
##' @author Joe Peskett
##' @export
tstep_lag <- function(data){
    function(tstep){
        stopifnot(tstep<length(data))
        lapply(seq_along(head(data, n = (length(data)-tstep))), 
             function(x) seq(x, length = tstep))
    }
    
}

##' subset_wrap
##' 
##' Will use the indices created in the tstep_lag closure to create an array.
##' @param vector of univariate time series data - should be the same that was
##'   used to create the indices that will be used in the outputted closure.
##' @return a function that takes a list of indices to 
##' @author Joe Peskett
##' @export
subset_wrap <- function(data){
    function(sub_list){
        stopifnot(class(sub_list)=="list")
        stopifnot(length(sub_list) == (length(data)-length(sub_list[[1]])))
        feature_matrix <- do.call(rbind, lapply(sub_list, function(x) data[x]))
        dim(feature_matrix) <- c(dim(feature_matrix), 1)
        return(feature_matrix)
    }
}

##' scaling_saver
##' 
##' Function to scale inputs and save as objects.
##' @param data a vector for scaling
##' @return a function to scale the data set, and any data sets that require the
##'   same values for scaling.
##' @author Joe Peskett
##' @export
scaling_saver <- function(data){
    function(scale_this){
        center <- mean(data)
        dev <- sd(data)
        scaled_it <- (scale_this - center)/dev
        return(scaled_it)
    }
}

