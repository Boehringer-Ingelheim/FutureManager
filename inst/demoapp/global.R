library(shiny)
library(ggplot2)

library(FutureManager)
plan(multiprocess) # this call may take some time in RStudio

`%||%` <- function(x, y) if (is.null(x)) y else x
