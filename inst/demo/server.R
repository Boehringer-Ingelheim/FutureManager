shinyServer(function(input, output, session) {
  fm <- FutureManager$new(session)
  
  # Sidebar menu --------------------------------------------------------------
  output$sidebar <- renderUI({
    if (input$tabset == "plot"){
      choices <- names(iris)[-5]
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
          selected = input$xVar %||% choices[1]
        ),
        selectInput(
          inputId = "yVar",
          label = "Y var",
          choices = choices,
          selected = input$yVar %||% choices[2]
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
          value = input$nRows %||% 6
        )
      )
    }
  })
  
  # Plot tab ------------------------------------------------------------------
  PlotObj <- reactiveVal()
  PlotArgs <- reactive({
    print("Plot args")
    list(
      xVar = input$xVar,
      yVar = input$yVar
    )
  })
  
  fmRegisterRunObserver(
    inputId = "plot_run",
    label = "Plot",
    statusVar = PlotObj,
    longFun = plotLongFun,
    input = input,
    fm = fm,
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
    print("Table args")
    list(nRows = input$nRows)
  })
  
  fmRegisterRunObserver(
    inputId = "table_run",
    label = "Table",
    statusVar = TableObj,
    longFun = tableLongFun,
    input = input,
    fm = fm,
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
  xVar <- sym(xVar)
  yVar <- sym(yVar)
  
  for (i in seq_len(10)){
    if (fmIsInterrupted(task)) return() # is canceled?
    p <- i/10
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
    if (fmIsInterrupted(task)) return() # is canceled?
    p <- i/10
    fmUpdateProgress(task, progress = p, msg = "preparing table...")
    
    Sys.sleep(1)
  }
  
  head(iris, nRows)
}
