shinyUI(fluidPage(
  titlePanel("FutureManager example"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      uiOutput("sidebar")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          title = "plot",
          plotOutput("plot")
        ),
        tabPanel(
          title = "table",
          tableOutput("table")
        )
      )
    )
  )
))
