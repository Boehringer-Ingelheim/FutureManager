#' @export
FutureManager <- R6::R6Class(
  classname = "FutureManager",
  public = list(
    showProgress = function(taskId, label, statusVar, millis = 500, session = getDefaultReactiveDomain()) {
      pb <- MinimalProgress$new(paste0(taskId, "_progress"), session)
      
      observe({
        statusVar()
        
        task <- private$getTask(taskId)
        if (is.null(task)){
          pb$close()
          return()
        }
        
        invalidateLater(millis, session)
        if (private$outputChanged(task)){
          x <- tryCatch(
            expr = {
              jsonlite::read_json(task$outFile)
            },
            error = function(e){
              NULL # premature EOF may happen when cancelling
            } 
          )
          
          if (length(x) > 0){
            pb$set(value = x$progress, label = label, msg = x$msg)
          }
        }
      })
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
    }
  ),
  private = list(
    tasks = list(),
    
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
