# fmStatus --------------------------------------------------------------------
#' Status class
#' 
#' This class is used to pass the background process results and metadata to 
#' the app. You can access the value directly (i.e. obj$value) or via 
#' fmGetValue() function (recommended!).
#' 
#' Possible statuses:
#' - "running": task is running;
#' - "failed": task has been stopped using fmError(); 
#' - "success": task completed succesfully;
#' - "error": unexpected error occured in the long function;
#' - "canceled": task has been canceled by the user
#' 
#' This function should NOT be used directly.
#' 
#' @param id character string, task ID
#' @param status character string
#' @param message message that may be used on front-end to inform user what's happeninged
#' @param value NULL for not-"success" tasks, the long function value for "success" tasks
#' 
#' @return fmStatus object
#' @export
#' @examples
#' fmStatus("task1", "success", "Task completed successfully", iris)
fmStatus <- function(id, status, message, value = NULL) {
  structure(
    list(
      id = id,
      status = status,
      message = message,
      value = value,
      timestamp = Sys.time()
    ),
    class = "fmStatus"
  )
}

#' Check if object is of fmStatus class
#' 
#' @param x object to test
#' 
#' @return logical
#' @export
#' @examples 
#' status <- fmStatus("task1", "success", "Task completed successfully", iris)
#' is.fmStatus(status)
is.fmStatus <- function(x) {
  methods::is(x, "fmStatus")
}

# fmError ---------------------------------------------------------------------
#' Error class
#' 
#' Allows to signalize some error in the long function (not enough data to run 
#' calculations etc)
#' 
#' @param msg Message that should be displayed to the user
#' 
#' @return fmError object
#' @export
#' @examples 
#' if (interactive()) {
#'   library(shiny)
#'   plan(fmParallelStrategy())
#'   
#'   longFun <- function(task, n){
#'     if (n < 0) return(fmError("n must be >= 0"))
#'     log(n)
#'   }
#'   
#'   shinyApp(
#'     ui = basicPage(
#'       uiOutput("button"), 
#'       numericInput("n", "n", -3, -5, 5), 
#'       textOutput("result")
#'     ),
#'     server = function(input, output, session){
#'       fm <- FutureManager$new(input, session)
#'       output$button <- renderUI(fmRunButton("run", fm))
#'       Res <- reactiveVal()
#'       Args <- reactive(list(n = input$n))
#'       fm$registerRunObserver("run", NULL, Res, longFun, Args, progress = FALSE)
#'       output$result <- renderText({
#'         res <- Res()
#'         fmValidate(res)
#'         fmGetValue(res)
#'       })
#'     }
#'   )
#' }
fmError <- function(msg){
  structure(
    msg,
    class = "fmError"
  )
}

#' Check if object is of fmError class
#' 
#' This is useful when using helper functions in the long running function.
#' 
#' @param x object to test
#' 
#' @return logical
#' @export
#' @examples 
#' if (interactive()) {
#'   library(shiny)
#'   plan(fmParallelStrategy())
#'   
#'   helper <- function(n){
#'     if (n < 0) return(fmError("n must be >= 0"))
#'     log(n)
#'   }
#'   longFun <- function(task, n){
#'     x <- helper(n)
#'     if (is.fmError(x)) return(x)
#'     x^2
#'   }
#'   
#'   shinyApp(
#'     ui = basicPage(
#'       uiOutput("button"), 
#'       numericInput("n", "n", -3, -5, 5), 
#'       textOutput("result")
#'     ),
#'     server = function(input, output, session){
#'       fm <- FutureManager$new(input, session)
#'       output$button <- renderUI(fmRunButton("run", fm))
#'       Res <- reactiveVal()
#'       Args <- reactive(list(n = input$n))
#'       fm$registerRunObserver("run", NULL, Res, longFun, Args, progress = FALSE)
#'       output$result <- renderText({
#'         res <- Res()
#'         fmValidate(res)
#'         fmGetValue(res)
#'       })
#'     }
#'   )
#' }
is.fmError <- function(x){
  inherits(x, "fmError")
}

# messaging -------------------------------------------------------------------
#' Check if process has been interrupted
#' 
#' Basically, it checks if the process has been canceled by the user.
#' 
#' @param task task object
#' 
#' @return logical
#' @export
#' @examples 
#' if (interactive()) {
#'   library(shiny)
#'   plan(fmParallelStrategy())
#'   
#'   longFun <- function(task, n){
#'     for (i in seq_len(10)){
#'       # check if the process has been canceled (every 1s)
#'       if (fmIsInterrupted(task)) return()
#'       Sys.sleep(1)
#'     }
#'     n
#'   }
#'   
#'   shinyApp(
#'     ui = basicPage(
#'       uiOutput("button"), 
#'       numericInput("n", "n", -3, -5, 5), 
#'       textOutput("result")
#'     ),
#'     server = function(input, output, session){
#'       fm <- FutureManager$new(input, session)
#'       output$button <- renderUI(fmRunButton("run", fm))
#'       Res <- reactiveVal()
#'       Args <- reactive(list(n = input$n))
#'       fm$registerRunObserver("run", NULL, Res, longFun, Args, progress = FALSE)
#'       output$result <- renderText({
#'         res <- Res()
#'         fmValidate(res)
#'         fmGetValue(res)
#'       })
#'     }
#'   )
#' }
fmIsInterrupted <- function(task) {
  file.exists(task$cancelFile)
}

#' Update progress of the background process
#' 
#' This function allows to pass progress info from the background process
#' to the main process, i.e. to display a progress in the application
#' 
#' @param task task object
#' @param progress numeric in range 0..1
#' @param msg character string, message that will be displayed in the progress bar
#' 
#' @return No return value, called for side effects.
#' @export
#' @examples 
#' if (interactive()) {
#'   library(shiny)
#'   plan(fmParallelStrategy())
#'   
#'   longFun <- function(task, n){
#'     for (i in seq_len(10)){
#'       if (fmIsInterrupted(task)) return()
#'       fmUpdateProgress(task, progress = i/10, msg = "busy...")
#'       Sys.sleep(1)
#'     }
#'     n
#'   }
#'   
#'   shinyApp(
#'     ui = basicPage(
#'       uiOutput("button"), 
#'       numericInput("n", "n", -3, -5, 5), 
#'       textOutput("result")
#'     ),
#'     server = function(input, output, session){
#'       fm <- FutureManager$new(input, session)
#'       output$button <- renderUI(fmRunButton("run", fm))
#'       Res <- reactiveVal()
#'       Args <- reactive(list(n = input$n))
#'       fm$registerRunObserver("run", "Progress", Res, longFun, Args)
#'       output$result <- renderText({
#'         res <- Res()
#'         fmValidate(res)
#'         fmGetValue(res)
#'       })
#'     }
#'   )
#' }
fmUpdateProgress <- function(task, progress = 0, msg = NULL) {
  jsonlite::write_json(
    x = list(
      taskId = task$id,
      progress = progress,
      msg = msg
    ),
    path = task$outFile,
    auto_unbox = TRUE
  )
}

# shiny utils -----------------------------------------------------------------
#' Get value from fmStatus
#' 
#' This function should be used when there's a need to get a value returned by
#' a background process. In particular, it signalizes any errors that may happen
#' in the process. Direct access (i.e. x[["value"]]) doesn't show errors.
#' 
#' The value is NULL for every status, except:
#' - "success": the value from the process
#' - "failed": the error message (see fmValidate() for handling this)
#' 
#' See \code{\link{fmError}} for some example.
#' 
#' @param x fmStatus object
#' 
#' @return the process value
#' @export
fmGetValue <- function(x) {
  if (!is.fmStatus(x)) return()
  if (x[["status"]] == "error") {
    warning("The background process returned an error!")
    stop(x[["message"]])
  }
  if (x[["status"]] == "failed") {
    warning(x[["value"]])
  }
  x[["value"]]
}

#' Validate the process value
#' 
#' This function should be used in every shiny output, which uses the background 
#' process value directly. It ensures that the value is valid and displays an
#' error message in case of any issues. Also, it displays helpful messages regarding 
#' the process start/wait with a CSS formatting.
#' 
#' See \code{\link{fmError}} for some example.
#' 
#' @param x fmStatus object
#' @param ... arguments passed to fmNeed function (msgInit and msgRun)
#' 
#' @return shiny validation
#' @export
fmValidate <- function(x, ...){
  args <- fmNeed(x, ...)
  
  isError <- sapply(args, is.fmError)
  args$errorClass <- if (any(isError)){
    "fm-failed"
  } else {
    "fm-wait"
  }
  
  do.call(shiny::validate, args)
}

#' Need a valid background process value
#' 
#' Ensures the value is valid.
#' 
#' @param x fmStatus object
#' @param msgInit character string, message that should be displayed when the 
#' process is not run yet
#' @param msgRun character string, message that should be displayed when the
#' process is still running
#' 
#' @return list of shiny needs
#' @export
fmNeed <- function(x, msgInit = "run the process first", msgRun = "wait for the process"){
  value <- fmGetValue(x) # will throw an error in case of error status
  msg <- if (is.null(x) || x[["status"]] == "canceled") msgInit else msgRun
  list(
    shiny::need(value, msg), 
    shiny::need(!is.fmError(value), value)
  )
}

#' Generate task ID
#' 
#' Use this function if when you don't need an fmRunButton. 
#' 
#' See \code{\link{demo_noButton}} for some example
#' 
#' @param id character string, ID pattern
#' @return character string
#' @export
fmGenerateTaskId <- function(id){
  paste(id, sample(1e6, 1), sep = "_")
}
