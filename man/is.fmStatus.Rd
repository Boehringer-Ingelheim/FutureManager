% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is.fmStatus}
\alias{is.fmStatus}
\title{Check if object is of fmStatus class}
\usage{
is.fmStatus(x)
}
\arguments{
\item{x}{object to test}
}
\value{
logical
}
\description{
Check if object is of fmStatus class
}
\examples{
status <- fmStatus("task1", "success", "Task completed successfully", iris)
is.fmStatus(status)
}
