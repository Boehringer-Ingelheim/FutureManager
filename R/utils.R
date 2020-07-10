# fmStatus --------------------------------------------------------------------
#' @export
fmStatus <- function(id, status, message, value = NULL) {
  structure(
    list(
      id = id,
      status = status,
      message = message,
      value = value,
      timestamp = `if`(!is.null(value), Sys.time())
    ),
    class = "fmStatus"
  )
}

#' @export
is.fmStatus <- function(x) {
  is(x, "fmStatus")
}

# fmError ---------------------------------------------------------------------
#' @export
fmError <- function(msg){
  structure(
    msg,
    class = "fmError"
  )
}

#' @export
is.fmError <- function(x){
  inherits(x, "fmError")
}

# messaging -------------------------------------------------------------------
#' @export
fmIsInterrupted <- function(task) {
  file.exists(task$cancelFile)
}

#' @export
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
#' @export
fmGetValue <- function(x) {
  if (!is.fmStatus(x)) return()
  if (x$status == "error") {
    warning("The background process returned an error!")
    stop(x$message)
  }
  if (x$status == "failed") {
    warning(x$value)
  }
  x$value
}

#' @export
fmValidate <- function(x, ...){
  args <- fmNeed(x, ...)
  
  isError <- sapply(args, is.fmError)
  args$errorClass <- if (any(isError)){
    "fm-failed"
  } else {
    "fm-wait"
  }
  
  do.call(validate, args)
}

#' @export
fmNeed <- function(x, msgInit = "run the process first", msgRun = "wait for the process"){
  value <- fmGetValue(x) # will throw an error in case of error status
  msg <- if (is.null(x)) msgInit else msgRun
  list(
    need(value, msg), 
    need(!is.fmError(value), value)
  )
}

# internal utils --------------------------------------------------------------
fmUpdateTimestamp <- function(statusVar) {
  s <- statusVar()
  
  if (!is.null(s$value)){
    s$timestamp <- Sys.time()
    statusVar(s)
  }
}
