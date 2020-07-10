#' @export
fmRunButton <- function(inputId, appState, cs, defaultValue = FALSE, allowOneClass = FALSE){
  if (is.null(appState[[inputId]])){
    appState[[inputId]] <- list(
      value = defaultValue,
      style = "default",
      disabled = FALSE
    )
  }
  
  operator <- if (allowOneClass) `&&` else `||`
  
  if (operator(length(cs$class1) == 0, length(cs$class2) == 0)){
    style <- "warning"
    disabled <- TRUE
  } else {
    style <- appState[[inputId]]$style
    disabled <- appState[[inputId]]$disabled
  }
  
  tagList(
    shinyBS::bsButton(
      inputId = inputId,
      label = NULL, # controlled in CSS
      value = appState[[inputId]]$value,
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
fmUpdateRunButton <- function(inputId, status, appState, session = getDefaultReactiveDomain()) {
  isSuccess <- status == "success"
  style <- if (isSuccess) "success" else appState[[inputId]]$style
  
  appState[[inputId]] <- list(
    value = FALSE,
    style = style,
    disabled = isSuccess
  )
  
  if (status == "danger"){
    print(appState[[inputId]])
  }
  
  shinyBS::updateButton(
    session = session,
    inputId = inputId,
    value = appState[[inputId]]$value,
    style = appState[[inputId]]$style,
    disabled = appState[[inputId]]$disabled
  )
}

#' @export
fmRegisterRunObserver <- function(id, label, statusVar, longFun, input, appState, fm, Args,
                                  opts = c("dbname", "dbhost", "dbport", "dbuser", "dbpass"), progress = TRUE){
  taskId <- paste0(id, "_task")
  
  if (progress){
    fm$showProgress(taskId, label, statusVar)
  }
  
  observeEvent(
    eventExpr = input[[id]],
    handlerExpr = {
      isTriggered <- input[[id]]
      
      if (appState[[id]]$value != isTriggered){
        appState[[id]]$value <- isTriggered # save the button state to avoid cache issue
        
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
                fmUpdateRunButton(id, status, appState)
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
      print(id)
      fmOutdateRun(
        name = id, 
        appState = appState, 
        immediate = TRUE
      )
    }
  )
}

#' @export
fmOutdateRun <- function(name, appState, immediate = FALSE){
  state <- appState[[name]]
  if (!is.null(state) && state$style == "success"){
    appState[[name]]$style <- "danger"
    appState[[name]]$disabled <- FALSE
    if (immediate){
      fmUpdateRunButton(name, "danger", appState)
    }
  }
}

#' @export
fmOutdateRuns <- function(appState){
  for (x in names(appState)){
    fmOutdateRun(x, appState)
  }
}
