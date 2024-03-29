% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backend.R
\name{fmParallelStrategy}
\alias{fmParallelStrategy}
\title{Utility function for selecting appropriate parallel strategy for FutureManager.}
\usage{
fmParallelStrategy()
}
\value{
Returns a \code{\link[future]{multicore}} parallel strategy if it is supported, or \code{\link[future]{multisession}} if forking is not available in a given environment.
}
\description{
Utility function for selecting appropriate parallel strategy for FutureManager.
}
\details{
For a better performance, this function returns a \code{\link[future]{multicore}} strategy for \code{future} \code{\link[future]{plan}}. You may experience a long app 
startup time and some lag when running background processes using RStudio, because it uses \code{\link[future]{multisession}} strategy.

See \code{\link[future]{multicore}} and \code{\link[future]{multisession}} for further details. 

Note that you are not obliged to set the parallel strategy using \code{fmParallelBackend}. 
You can simply run \code{plan(future::multicore)} or \code{plan(future::multicore)}.
}
\examples{

plan(fmParallelStrategy())

}
