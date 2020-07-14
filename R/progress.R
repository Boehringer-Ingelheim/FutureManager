#' Minimal progress
#' 
#' Minimal progress class that enhances the default shiny progress behavior.
#' The most important is ability to initialize the progress without opening
#' in the app frontend. Also, the interface is simplified.
MinimalProgress <- R6::R6Class(
  classname = "MinimalProgress",
  public = list(
    #' @description 
    #' Create a new progress object
    #' @param id character string, the progress ID
    #' @param session shiny session object
    #' @return MinimalProgress object
    initialize = function(id, session){
      private$id <- id
      private$session <- session
      invisible(self)
    },
    
    #' @description 
    #' Opens the progress bar in the frontend
    #' @return self
    open = function(){
      if (!private$isOpened){
        private$isOpened <- TRUE
        private$session$sendProgress("open", list(
          id = private$id, 
          style = private$style
        ))
      }
      
      invisible(self)
    },
    
    #' @description 
    #' Close the progress bar in the frontend
    #' @return self
    close = function(){
      if (private$isOpened){
        private$isOpened <- FALSE
        private$session$sendProgress("close", list(
          id = private$id, 
          style = private$style
        ))
      }
      
      invisible(self)
    },
    
    #' @description 
    #' Set a progress bar state
    #' @param label character string, the progress label
    #' @param value numeric in range 0..1
    #' @param msg character string, the progress message
    #' @return self
    set = function(label, value, msg){
      if (!private$isOpened){
        self$open()
      }
      
      private$session$sendProgress("update", list(
        id = private$id,
        message = paste0(label, ":"),
        detail = msg,
        value = value, 
        style = private$style
      ))
      
      invisible(self)
    }
  ),
  private = list(
    id = NULL,
    session = NULL,
    isOpened = FALSE,
    style = "notification"
  )
)
