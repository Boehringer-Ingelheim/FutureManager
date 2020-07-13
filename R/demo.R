#' @export
demo <- function(host = "0.0.0.0", port = 3838L, launch.browser = TRUE){
  shiny::runApp(
    appDir = system.file("demo", package = "FutureManager"),
    host = host,
    port = port,
    launch.browser = launch.browser
  )
}
