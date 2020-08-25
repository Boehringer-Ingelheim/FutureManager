validateArgs <- function(args){
  filtered <- args[!vapply(args, is.null, FUN.VALUE=logical(1))]
  length(filtered) == length(args)
}

shinyServer(function(input, output, session) {
  fm <- FutureManager$new(
    input = input, 
    session = session
  )
  
  PlotObj <- reactiveVal()
  Args <- reactive({
    list(
      xVar = input$xVar,
      yVar = input$yVar
    )
  })
  
  taskId <- "plot" # some initial ID
  observeEvent(
    eventExpr = Args(),
    handlerExpr = {
      args <- Args()
      req(validateArgs(args))
      
      fm$cancel(taskId)
      
      # generate new ID in case of changing args
      # new process with the latest args will spin up
      # the previous process with outdated args will be canceled
      taskId <<- fmGenerateTaskId("plot") 
      fm$showProgress(taskId, "Plot", PlotObj)
      fm$run(
        taskId = taskId,
        fun = plotLongFun,
        args = args,
        statusVar = PlotObj
      )
    }
  )
  
  output$plot <- renderPlot({
    p <- PlotObj() # invalidates only on PlotObj change
    fmValidate(p)
    fmGetValue(p)
  })
  
  output$table <- renderTable({
    head(iris, input$nRows)
  })
})

# Long fun --------------------------------------------------------------------
plotLongFun <- function(task, xVar, yVar){
  xVar <- sym(xVar)
  yVar <- sym(yVar)
  
  for (i in seq_len(10)){
    p <- i/10
    
    if (fmIsInterrupted(task)) return() # is canceled?
    fmUpdateProgress(task, progress = p, msg = "rendering plot...")
    Sys.sleep(1)
  }
  
  ggplot(
    data = iris, 
    mapping = aes(
      x = !!xVar,
      y = !!yVar
    )
  ) + geom_point()
}
