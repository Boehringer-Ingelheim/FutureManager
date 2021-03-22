#' Background calculations in shiny apps
#' 
#' Manages the background processes and also keeps the state.
#' FutureManager is designed to work with fmRunButton. See demo() for details
#' 
#' See demo app (both) for some complete examples:
#' \itemize{
#'   \item{With run button:}{ \code{\link{demo}} }
#'   \item{Without run button:}{ \code{\link{demo_noButton}} }
#' }
#' @export
FutureManager <- R6::R6Class(
  classname = "FutureManager",
  public = list(
    #' @description 
    #' Create a new manager object
    #' @param input shiny input object
    #' @param session shiny session object
    #' @param opts character, names of options that should be passed to every background process
    #' @param keepPreviousResults logical, should keep the results from the previous run until the latest ones are available? 
    #' @return MinimalProgress object
    initialize = function(input, session = shiny::getDefaultReactiveDomain(), opts = c(), keepPreviousResults = FALSE){
      private$opts <- opts
      private$input <- input
      private$session <- session
      private$keepPreviousResults <- keepPreviousResults
      
      invisible(self)
    },
    
    #' @description 
    #' Show progress bar for a specific task
    #' @param taskId character string, the task ID
    #' @param label character string, the progress label
    #' @param statusVar reactiveVal object that is linked with the process
    #' @param millis integer, refreshing interval (in ms)
    #' @param session shiny session object
    #' @return self
    showProgress = function(taskId, label, statusVar, millis = 500, session = shiny::getDefaultReactiveDomain()) {
      pb <- MinimalProgress$new(paste0(taskId, "_progress"), session)
      private$pb[[taskId]] <- pb
      
      private$observers[[taskId]] <- shiny::observe({
        statusVar()
        
        task <- private$getTask(taskId)
        if (is.null(task)){
          return()
        }
        
        shiny::invalidateLater(millis, session)
        if (private$outputChanged(task)){
          x <- tryCatch(
            expr = jsonlite::read_json(task$outFile),
            error = function(e) NULL # premature EOF may happen when cancelling
          )
          
          if (length(x) > 0){
            pb$set(value = x$progress, label = label, msg = x$msg)
          }
        }
      })
      
      invisible(self)
    },
    
    #' @description 
    #' Run background process
    #' @param taskId character string, the task ID
    #' @param fun the long running function, the function should accept at least 
    #' 1 argument "task"
    #' @param args list of additional arguments passed to fun
    #' @param statusVar reactiveVal object that is linked with the process
    #' @param opts character, names of options that should be passed to the 
    #' background process
    #' @param finally NULL or function, that will be executed after the process 
    #' finishes. Function should accept 1 argument, the process status (string)
    #' @param seed passed to future::future
    #' @param ... arguments passed to future::future
    #' @return self
    run = function(taskId, fun, args, statusVar, opts = c(), finally = NULL, seed = TRUE, ...){
      if (private$taskExists(taskId)) {
        warning("Task '", taskId, "' is already running!")
        return(invisible(self))
      }
      
      task <- list(
        id = taskId,
        outFile = tempfile(fileext = ".json"),
        cancelFile = tempfile(fileext = ".stp"),
        checkSum = NULL
      )
      private$addTask(task)
      
      oldStatus <- shiny::isolate(statusVar())
      if (!private$keepPreviousResults || !is.fmStatus(oldStatus) || oldStatus[["status"]] != "success"){
        statusVar(fmStatus(
          id = task$id,
          status = "running",
          message = "Task is running"
        ))
      }
      
      args[["task"]] <- task
      
      opts <- union(private$opts, opts)
      fopts <- options()[opts]
      
      result <- future::future({
        options(fopts)
        res <- tryCatch(
          expr = { do.call(fun, args) },
          error = function(e){
            traceback(e)
            stop(e)
          }
        )
      }, seed = seed, ...)
      
      result <- promises::then(
        promise = result, 
        onFulfilled = function(value) {
          status <- if (fmIsInterrupted(task)){
            fmStatus(
              id = task$id,
              status = "canceled",
              message = "Canceled by the user"
            )
          } else if (is.fmError(value)){
            fmStatus(
              id = task$id,
              status = "failed",
              message = "Task failed",
              value = value
            )
          } else {
           fmStatus(
              id = task$id,
              status = "success",
              message = "Task completed",
              value = value
            )
          }
          
          statusVar(status)
        },
        onRejected = function(e) {
          statusVar(fmStatus(
            id = task$id,
            status = "error",
            message = e$message
          ))
        })
      
      result <- promises::finally(
        promise = result,
        onFinally = function() {
          if (file.exists(task$outFile)) file.remove(task$outFile)
          if (fmIsInterrupted(task)) file.remove(task$cancelFile)
          status <- statusVar()[["status"]]
          if (is.function(finally)) finally(status) # user defined action
          private$removeTask(task$id)
        }
      )
      
      invisible(self)
    },
    
    #' @description 
    #' Cancel the background process
    #' @details 
    #' Note that the long function must be able to detect the cancel. See 
    #' fmIsInterrupted() for details
    #' @param taskId character string, the task ID
    #' @return self
    cancel = function(taskId) {
      task <- private$getTask(taskId)
      
      if (!is.null(task)){
        file.create(task$cancelFile)
      }
      
      invisible(self)
    },
    
    #' @description 
    #' Init button state if not initialized yet
    #' @param inputId character string, the button ID
    #' @param defaultValue logical, the default button value
    #' @return current button state
    initButtonState = function(inputId, defaultValue = FALSE){
      if (is.null(private$buttonState[[inputId]])){
        private$buttonState[[inputId]] <- list(
          value = defaultValue,
          status = "init",
          mustRerun = FALSE
        )
      }
      
      private$buttonState[[inputId]]
    },
    
    #' @description 
    #' Get button state
    #' @param inputId character string, the button ID
    #' @return current button state
    getButtonState = function(inputId){
      private$buttonState[[inputId]]
    },
    
    #' @description 
    #' Update button state
    #' @param inputId character string, the button ID
    #' @param value logical, the button value
    #' @param status character string, see bsButton for details
    #' @return current button state
    updateButtonState = function(inputId, value, status){
      if (!missing(value) && !is.null(value)){
        private$buttonState[[inputId]]$value <- value
      }
      
      if (!missing(status) && !is.null(status)){
        private$buttonState[[inputId]]$status <- status
        
        if (status == "success"){
          private$buttonState[[inputId]]$mustRerun <- FALSE
        } else if (status == "rerun"){
          private$buttonState[[inputId]]$mustRerun <- TRUE
        }
      }
      
      private$buttonState[[inputId]]
    },
    
    #' @description 
    #' Outdate (invalidate) the process
    #' @param inputId character string, the button ID
    #' @param immediate logical, should push the message to the frontend?
    #' @return self
    outdateRun = function(inputId, immediate = FALSE){
      buttonState <- self$getButtonState(inputId)

      if (!is.null(buttonState) && buttonState$status %in% c("success", "running")){
        self$updateButtonState(
          inputId = inputId,
          status = "rerun"
        )
        
        if (immediate && buttonState$status == "success"){
          fmUpdateRunButton(inputId, "rerun", self, private$session)
        }
      }
      
      invisible(self)
    },
    
    #' @description 
    #' Outdate (invalidate) all processes
    #' @param immediate logical, should push the message to the frontend?
    #' @return self
    outdateRuns = function(immediate = FALSE){
      for (inputId in names(private$buttonState)){
        self$outdateRun(inputId, immediate)
      }
      
      invisible(self)
    },
    
    #' @description 
    #' Register run observer
    #' @param inputId character string, the button ID
    #' @param label character string, the progress bar label (used only if progress = TRUE)
    #' @param statusVar reactiveVal object that is linked with the process
    #' @param longFun long running function, see run() method for details
    #' @param Args reactive, that should return a named list of additional longFun 
    #' arguments
    #' @param opts character vector, additional options that should be passed to 
    #' the background process (apart of the opts specified in initialize())
    #' @param progress logical, should progress bar be displayed?
    #' @param input shiny input object, overrides the input defined in initialize();
    #'  helpful in shiny modules
    #' @return self
    registerRunObserver = function(inputId, label, statusVar, longFun, Args, 
                                   opts = c(), progress = TRUE, input = NULL){
      taskId <- fmGenerateTaskId(inputId)
      argsChanged <- FALSE
      
      if (is.null(input)) {
        input <- private$input
      }
      
      shiny::observeEvent(
        eventExpr = input[[inputId]],
        handlerExpr = {
          isTriggered <- input[[inputId]]
          buttonState <- self$getButtonState(inputId)
          if (buttonState$value != isTriggered){
            if (isTriggered){
              if (progress){
                self$showProgress(taskId, label, statusVar)
              }
              
              argsChanged <<- FALSE
              args <- Args()
              self$run(
                taskId = taskId, 
                fun = longFun, 
                args = args,
                statusVar = statusVar,
                opts = opts,
                finally = function(taskStatus){
                  if (argsChanged && taskStatus == "success"){
                    taskStatus <- "rerun"
                  }
                  fmUpdateRunButton(inputId, taskStatus, self, private$session)
                }
              )
              status <- "running"
            } else {
              self$cancel(taskId)
              status <- NULL # will be updated in finally function
            }
            
            self$updateButtonState(
              inputId = inputId,
              value = isTriggered, # save the button state to avoid cache issue
              status = status
            )
          }
        }
      )
      
      shiny::observeEvent(
        eventExpr = Args(),
        handlerExpr = {
          argsChanged <<- TRUE
          self$outdateRun(inputId, TRUE)
        }
      )
    }
  ),
  
  private = list(
    tasks = list(),
    observers = list(),
    pb = list(),
    buttonState = list(),
    input = NULL,
    session = NULL,
    opts = c(),
    keepPreviousResults = FALSE,
    
    addTask = function(task) {
      taskId <- task$id
      private$tasks[[taskId]] <- task
    },
    
    removeTask = function(taskId) {
      private$tasks[[taskId]] <- NULL
      
      obs <- private$observers[[taskId]]
      if (!is.null(obs)){
        obs$destroy()
        private$observers[[taskId]] <- NULL
      }
      
      
      pb <- private$pb[[taskId]]
      if (!is.null(pb)){
        pb$close()
        private$pb[[taskId]] <- NULL
      }
    },
    
    getTask = function(taskId) {
      private$tasks[[taskId]]
    },
    
    taskExists = function(taskId) {
      !is.null(private$getTask(taskId))
    },
    
    outputChanged = function(task) {
      if (!file.exists(task$outFile)) return(FALSE)
      
      checkSum <- tools::md5sum(task$outFile)
      if (!identical(checkSum, task$checkSum)){
        private$tasks[[task$id]]$checkSum <- checkSum
        TRUE
      } else {
        FALSE
      }
    }
  )
)
