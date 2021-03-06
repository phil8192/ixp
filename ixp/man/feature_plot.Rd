% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visual.R
\name{feature_plot}
\alias{feature_plot}
\title{feature_plot}
\usage{
feature_plot(my_xts, date, multipliers = c(1, 1.25, 1.5, 1.75, 2))
}
\arguments{
\item{my_xts}{Xts object holding the data to be plotted}

\item{date}{a single date as a character in form "yyyy-mm-dd"}

\item{multipliers}{to be passed to feature_frame()}
}
\value{
A ggplot object with the multipliers added for proof
}
\description{
Plot a single day with multipliers plotted as lines to as evidence of the 
function working. Feature_frame is called within the function and 
multipliers are passed directly from this function. Default multipliers are
the same as the feature_frame() function.
}
\author{
Joe Peskett
}
