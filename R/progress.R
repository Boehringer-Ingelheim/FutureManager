MinimalProgress <- R6::R6Class(
  classname = "MinimalProgress",
  public = list(
    initialize = function(id, session){
      private$id <- id
      private$session <- session
      invisible(self)
    },
    
    open = function(){
      if (!private$isOpened){
        private$isOpened <- TRUE
        private$session$sendProgress("open", list(
          id = private$id, 
          style = private$style
        ))
      }
    },
    
    close = function(){
      if (private$isOpened){
        private$isOpened <- FALSE
        private$session$sendProgress("close", list(
          id = private$id, 
          style = private$style
        ))
      }
    },
    
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
    }
  ),
  private = list(
    id = NULL,
    session = NULL,
    isOpened = FALSE,
    style = "notification"
  )
)
