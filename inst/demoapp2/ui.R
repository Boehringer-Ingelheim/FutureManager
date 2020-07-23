choices <- names(iris)[-5]
shinyUI(fluidPage(
  titlePanel("FutureManager example 2"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(
        inputId = "xVar",
        label = "X var",
        choices = choices,
        selected = choices[1]
      ),
      selectInput(
        inputId = "yVar",
        label = "Y var",
        choices = choices,
        selected = choices[2]
      ),
      hr(),
      sliderInput("nRows", "Rows", 1, 20, 6)
    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
))
