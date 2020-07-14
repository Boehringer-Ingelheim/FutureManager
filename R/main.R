#' Background calculations in shiny apps
#' 
#' Manages the background processes and also keeps the state.
#' FutureManager is designed to work with fmRunButton. See demo() for details
#' @export
FutureManager <- R6::R6Class(
  classname = "FutureManager",
  public = list(
    #' @description 
    #' Create a new manager object
    #' @param input shiny input object
    #' @param session shiny session object
    #' @param opts character, names of options that should be passed to every background process
    #' @return MinimalProgress object
    initialize = function(input, session = shiny::getDefaultReactiveDomain(), opts = c()){
      private$opts <- opts
      private$input <- input
      private$session <- session
      
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
    showProgress = function(taskId, label, statusVar, millis = 500, session = getDefaultReactiveDomain()) {
      pb <- MinimalProgress$new(paste0(taskId, "_progress"), session)
      
      shiny::observe({
        statusVar()
        
        task <- private$getTask(taskId)
        if (is.null(task)){
          pb$close()
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
    #' @param ... arguments passed to future::future
    #' @return self
    run = function(taskId, fun, args, statusVar, opts = c(), finally = NULL, ...){
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
      
      statusVar(fmStatus(
        id = task$id,
        status = "running",
        message = "Task is running"
      ))
      
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
      }, ...)
      
      result <- promises::then(
        promise = result, 
        onFulfilled = function(value) {
          if (is.fmError(value)){
            statusVar(fmStatus(
              id = task$id,
              status = "failed",
              message = "Task failed",
              value = value
            ))
          } else {
            statusVar(fmStatus(
              id = task$id,
              status = "success",
              message = "Task completed",
              value = value
            ))
          }
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
          if (fmIsInterrupted(task)){
            statusVar(fmStatus(
              id = task$id,
              status = "canceled",
              message = "Canceled by the user"
            ))
            
            file.remove(task$cancelFile)
          }
          private$removeTask(task$id)
          status <- statusVar()$status
          if (is.function(finally)) finally(status) # user defined action
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
          style = "default",
          disabled = FALSE
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
    #' @param style character string, see bsButton for details
    #' @param disabled logical, should disable the button?
    #' @return current button state
    updateButtonState = function(inputId, value, style, disabled){
      if (!missing(value)){
        private$buttonState[[inputId]]$value <- value
      }
      
      if (!missing(style)){
        private$buttonState[[inputId]]$style <- style
      }
      
      if (!missing(disabled)){
        private$buttonState[[inputId]]$disabled <- disabled
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
      
      if (!is.null(buttonState) && buttonState$style == "success"){
        self$updateButtonState(
          inputId = inputId,
          style = "danger",
          disabled = FALSE
        )
        
        if (immediate){
          fmUpdateRunButton(inputId, "danger", self, private$session)
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
    #' @param label character string, the progress bar label
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
      taskId <- paste0(inputId, "_task")
      
      if (is.null(input)){
        input <- private$input
      }
      
      if (progress){
        self$showProgress(taskId, label, statusVar)
      }
      
      shiny::observeEvent(
        eventExpr = input[[inputId]],
        handlerExpr = {
          isTriggered <- input[[inputId]]
          
          buttonState <- self$getButtonState(inputId)
          
          if (buttonState$value != isTriggered){
            self$updateButtonState(
              inputId = inputId,
              value = isTriggered # save the button state to avoid cache issue
            )
            
            if (isTriggered){
              if (is.function(longFun)){
                args <- Args()
                
                self$run(
                  taskId = taskId, 
                  fun = longFun, 
                  args = args,
                  statusVar = statusVar,
                  opts = opts,
                  finally = function(status){
                    fmUpdateRunButton(inputId, status, self)
                  }
                )
              }
            } else {
              self$cancel(taskId)
            }
          }
        }
      )
      
      shiny::observeEvent(
        eventExpr = Args(),
        handlerExpr = {
          self$outdateRun(inputId, TRUE)
        }
      )
    }
  ),
  
  private = list(
    tasks = list(),
    buttonState = list(),
    input = NULL,
    session = NULL,
    opts = c(),
    
    addTask = function(task) {
      taskId <- task$id
      private$tasks[[taskId]] <- task
    },
    
    removeTask = function(taskId) {
      private$tasks[[taskId]] <- NULL
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
