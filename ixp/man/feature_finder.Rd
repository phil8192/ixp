% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eda_functions.R
\name{feature_finder}
\alias{feature_finder}
\title{Feature finder}
\usage{
feature_finder(multiplier)
}
\arguments{
\item{multiplier}{a given multiplier}

\item{my_df}{an xts object of a single day. See details above for info.}
}
\value{
a vector of the Bandwidth and the MsM value for that multiple.
}
\description{
Function will find the time and recording of the first record to cross a 
defined threshold. This threshold is set to be a multiple of the minimum 
found for the dataset. This function is called by feature_frame, and it is
recommended that you use feature_frame over feature_finder.
}
\author{
Joe Peskett
}
