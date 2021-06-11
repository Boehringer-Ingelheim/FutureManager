#' Run button
#' 
#' Creates toggle button, that may be used to control a long background process (start, cancel etc)
#' 
#' See \code{\link{fmError}} for some example.
#' 
#' @param inputId character string, the button ID
#' @param fm FutureManager object
#' @param defaultValue logical, the initial button value
#' @param blocked logical, should the button be blocked?
#' 
#' @return button HTML
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
    style <- getStyleForStatus(buttonState$status)
    disabled <- style == "success"
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
      version = utils::packageVersion("FutureManager"),
      src = "FutureManager",
      stylesheet = "FutureManager.css"
    )
  )
}

#' Update run button
#' 
#' Updates the run button (on frontend) and also its state (on backend)
#' 
#' @param inputId character string, the button ID
#' @param status character string, the process status
#' @param fm FutureManager object
#' @param session shiny session object
#' 
#' @return No return value, called for side effects.
#' @export
fmUpdateRunButton <- function(inputId, status, fm, session = shiny::getDefaultReactiveDomain()) {
  isSuccess <- status == "success"
  
  currentState <- fm$getButtonState(inputId)
  if (!isSuccess && currentState$mustRerun){
    status <- "rerun"
    isSuccess <- FALSE
  }
  
  buttonState <- fm$updateButtonState(
    inputId = inputId,
    value = FALSE,
    status = status
  )
  
  shinyBS::updateButton(
    session = session,
    inputId = inputId,
    value = FALSE,
    style = getStyleForStatus(status),
    disabled = isSuccess
  )
}

getStyleForStatus <- function(status){
  switch(
    EXPR = status,
    success = "success",
    rerun = "danger",
    blocked = "warning",
    "default"
  )
}
