% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{linear_interpolate}
\alias{linear_interpolate}
\title{Linear interpolation}
\usage{
linear_interpolate(x, interval = "5 min", ...)
}
\arguments{
\item{x}{xts object}

\item{interval}{default: "5 min"}
}
\value{
Interpolated xts object
}
\description{
Uses xts na.approx.
}
\author{
phil
}
\keyword{internal}
