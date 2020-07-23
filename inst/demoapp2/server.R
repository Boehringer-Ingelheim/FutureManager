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
  
  taskId <- "plot"
  fm$showProgress(taskId, "Plot", PlotObj)
  observeEvent(
    eventExpr = Args(),
    handlerExpr = {
      args <- Args()
      req(validateArgs(args))
      fm$cancel(taskId)
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
