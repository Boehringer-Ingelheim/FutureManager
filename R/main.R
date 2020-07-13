#' @export
FutureManager <- R6::R6Class(
  classname = "FutureManager",
  public = list(
    initialize = function(input, session = shiny::getDefaultReactiveDomain(), opts = c()){
      private$opts <- opts
      private$input <- input
      private$session <- session
      
      invisible(self)
    },
    
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
      })
      
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
    
    cancel = function(taskId) {
      task <- private$getTask(taskId)
      
      if (!is.null(task)){
        file.create(task$cancelFile)
      }
      
      invisible(self)
    },
    
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
    
    getButtonState = function(inputId){
      private$buttonState[[inputId]]
    },
    
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
    
    outdateRuns = function(immediate = FALSE){
      for (inputId in names(private$buttonState)){
        self$outdateRun(inputId, immediate)
      }
      
      invisible(self)
    },
    
    registerRunObserver = function(inputId, label, statusVar, longFun, Args, opts = c(), progress = TRUE, input = NULL){
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
