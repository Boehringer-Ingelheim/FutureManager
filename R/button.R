#' @export
fmRunButton <- function(inputId, fm, cs, defaultValue = FALSE, allowOneClass = FALSE){
  buttonState <- fm$initButtonState(
    inputId = inputId,
    defaultValue = defaultValue
  )
  
  operator <- if (allowOneClass) `&&` else `||`
  
  if (operator(length(cs$class1) == 0, length(cs$class2) == 0)){
    style <- "warning"
    disabled <- TRUE
  } else {
    style <- buttonState$style
    disabled <- buttonState$disabled
  }
  
  tagList(
    shinyBS::bsButton(
      inputId = inputId,
      label = NULL, # controlled in CSS
      value = buttonState$value,
      style = style,
      disabled = disabled,
      type = "toggle",
      class = "fm-run"
    ),
    htmltools::htmlDependency(
      name = "FutureManager",
      package = "FutureManager",
      version = packageVersion("FutureManager"),
      src = "FutureManager",
      stylesheet = "FutureManager.css"
    )
  )
}

#' @export
fmUpdateRunButton <- function(inputId, status, fm, session = getDefaultReactiveDomain()) {
  isSuccess <- status == "success"
  
  currentState <- fm$getButtonState(inputId)
  style <- if (isSuccess) "success" else currentState$style
  
  buttonState <- fm$updateButtonState(
    inputId = inputId,
    value = FALSE,
    style = style,
    disabled = isSuccess
  )

  if (status == "danger"){
    print(buttonState)
  }
  shinyBS::updateButton(
    session = session,
    inputId = inputId,
    value = buttonState$value,
    style = buttonState$style,
    disabled = buttonState$disabled
  )
}

#' @export
fmRegisterRunObserver <- function(inputId, label, statusVar, longFun, input, fm, Args,
                                  opts = c("dbname", "dbhost", "dbport", "dbuser", "dbpass"), progress = TRUE){
  taskId <- paste0(inputId, "_task")
  
  if (progress){
    fm$showProgress(taskId, label, statusVar)
  }
  
  observeEvent(
    eventExpr = input[[inputId]],
    handlerExpr = {
      isTriggered <- input[[inputId]]
      
      buttonState <- fm$getButtonState(inputId)
      
      if (buttonState$value != isTriggered){
        fm$updateButtonState(
          inputId = inputId,
          value = isTriggered # save the button state to avoid cache issue
        )
        
        if (isTriggered){
          if (is.function(longFun)){
            args <- Args()
            
            fm$run(
              taskId = taskId, 
              fun = longFun, 
              args = args,
              statusVar = statusVar,
              opts = opts,
              finally = function(status){
                fmUpdateRunButton(inputId, status, fm)
              }
            )
          }
        } else {
          fm$cancel(taskId)
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = Args(),
    handlerExpr = {
      fm$outdateRun(inputId, TRUE)
    }
  )
}
