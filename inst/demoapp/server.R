shinyServer(function(input, output, session) {
  fm <- FutureManager$new(
    input = input, 
    session = session
  )
  
  # Sidebar menu --------------------------------------------------------------
  output$sidebar <- renderUI({
    if (input$tabset == "plot"){
      choices <- names(iris)
      tagList(
        fmRunButton(
          inputId = "plot_run",
          fm = fm
        ),
        hr(),
        selectInput(
          inputId = "xVar",
          label = "X var",
          choices = choices,
          selected = isolate(input$xVar) %||% choices[1]
        ),
        selectInput(
          inputId = "yVar",
          label = "Y var",
          choices = choices,
          selected = isolate(input$yVar) %||% choices[2]
        )
      )
    } else {
      tagList(
        fmRunButton(
          inputId = "table_run",
          fm = fm
        ),
        hr(),
        sliderInput(
          inputId = "nRows",
          label = "Number of rows",
          min = 1,
          max = 150,
          value = isolate(input$nRows) %||% 6
        )
      )
    }
  })
  
  # Plot tab ------------------------------------------------------------------
  PlotObj <- reactiveVal()
  PlotArgs <- reactive({
    list(
      xVar = input$xVar,
      yVar = input$yVar
    )
  })
  
  fm$registerRunObserver(
    inputId = "plot_run",
    label = "Plot",
    statusVar = PlotObj,
    longFun = plotLongFun,
    Args = PlotArgs
  )
  
  output$plot <- renderPlot({
    p <- PlotObj()
    fmValidate(p)
    
    fmGetValue(p)
  })
  
  # Table tab -----------------------------------------------------------------
  TableObj <- reactiveVal()
  TableArgs <- reactive({
    list(nRows = input$nRows)
  })
  
  fm$registerRunObserver(
    inputId = "table_run",
    label = "Table",
    statusVar = TableObj,
    longFun = tableLongFun,
    Args = TableArgs
  )
  
  output$table <- renderTable({
    t <- TableObj()
    fmValidate(t)
    
    fmGetValue(t)
  })
})

# Long funs -------------------------------------------------------------------
plotLongFun <- function(task, xVar, yVar){
  if (xVar == "Species") return(fmError("Species column not allowed as xVar"))
  if (yVar == "Species") stop("Species column not allowed as yVar")
  
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

tableLongFun <- function(task, nRows){
  for (i in seq_len(10)){
    p <- i/10
    
    if (fmIsInterrupted(task)) return() # is canceled?
    fmUpdateProgress(task, progress = p, msg = "preparing table...")
    
    Sys.sleep(1)
  }
  
  head(iris, nRows)
}
