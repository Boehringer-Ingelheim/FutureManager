# nocov start
#' FutureManager demo app
#' 
#' Runs the demo app that shows the abilities of the package
#' 
#' Demo files location: \code{system.file("demoapp", package = "FutureManager")}
#' 
#' @param host host IP
#' @param port app port
#' @param launch.browser logical, should launch the browser?
#' @export
#' 
#' @return No return value, called for side effects.
#' 
#' @examples 
#' if(interactive()) {
#' demo(launch.browser = TRUE)
#' }
demo <- function(host = "0.0.0.0", port = 3838L, launch.browser = FALSE){
  shiny::runApp(
    appDir = system.file("demoapp", package = "FutureManager"),
    host = host,
    port = port,
    launch.browser = launch.browser
  )
}

#' FutureManager demo app without the run button
#' 
#' Runs another demo app, that doesn't use run button (heavy calculations start 
#' on-demand, without additional user action)
#' 
#' Demo files location: \code{system.file("demoapp2", package = "FutureManager")}
#' 
#' @param host host IP
#' @param port app port
#' @param launch.browser logical, should launch the browser?
#' @export
#' 
#' @return No return value, called for side effects.
#' @examples 
#' if(interactive()) {
#' demo_noButton(launch.browser = TRUE)
#' }
demo_noButton <- function(host = "0.0.0.0", port = 3838L, launch.browser = FALSE){
  shiny::runApp(
    appDir = system.file("demoapp2", package = "FutureManager"),
    host = host,
    port = port,
    launch.browser = launch.browser
  )
}
# nocov end
