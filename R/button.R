#' @export
fmRunButton <- function(inputId, fm, defaultValue = FALSE, blocked = FALSE){
  buttonState <- fm$initButtonState(
    inputId = inputId,
    defaultValue = defaultValue
  )
  
  if (blocked){
    style <- "warning"
    disabled <- TRUE
  } else {
    style <- buttonState$style
    disabled <- buttonState$disabled
  }
  
  shiny::tagList(
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
  
  shinyBS::updateButton(
    session = session,
    inputId = inputId,
    value = buttonState$value,
    style = buttonState$style,
    disabled = buttonState$disabled
  )
}

