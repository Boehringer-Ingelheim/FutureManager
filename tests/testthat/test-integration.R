runClick <- function(app, id, cancel = FALSE){
  app$executeScript(sprintf("$('#%s').click();", id))
  app$waitForValue(
    name = id, 
    iotype = "input",
    ignore = list(cancel, NULL),
    timeout = 20000
  )
}

getValue <- function(app, name, input = TRUE){
  state <- app$getAllValues(
    input = input,
    output = !input,
    export = FALSE
  )
  state[[1]][[name]]
}

test_that(
  desc = "Run tasks work properly",
  code = {
    
    testthat::skip_on_cran()
    
    # run the app
    app <- shinytest::ShinyDriver$new(
      path = system.file("demoapp", package = "FutureManager"),
      loadTimeout = 10000 # 10s to spin up processes in Rstudio
    )
    app$waitForValue( # wait for the UI render
      name = "yVar", 
      iotype = "input"
    )
    
    # check the initial state
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "run the process first"
    )
    expect_true("fm-wait" %in% plot$type)
    
    # start calculations
    t_start <- Sys.time()
    runClick(app, "plot_run")
    
    app$setInputs(tabset = "table")
    app$waitForValue( # wait for the UI render
      name = "nRows", 
      iotype = "input"
    )
    runClick(app, "table_run") # turn on table calculations
    
    # verify running state
    app$setInputs(tabset = "plot") # go back to plot
    t_init <- Sys.time() - t_start 
    
    # check responsiveness of the app during the calculations and also if the app keeps the state
    expect_true(getValue(app, "plot_run"))
    
    # wait for the results
    app$waitFor(
      expr = "$('#plot').find('img').length;",
      checkInterval = 500,
      timeout = 25000 # 25s
    )
    
    t_total <- Sys.time() - t_start
    
    # check if the processes were run in another thread
    # each process takes at least 10s, so t_init should be smaller than 9s
    expect_true(t_init < 9)
    # check if the processes were run in parallel, i.e. t_total should be smaller than 20s
    expect_true(t_total > 10 && t_total < 19)
    
    # verify button state
    plotBtn <- app$findElement("#plot_run")
    expect_true("btn-success" %in% plotBtn$getClass())
    
    # verify button invalidation
    app$setInputs(xVar = "Petal.Width")
    plotBtn <- app$findElement("#plot_run")
    expect_true("btn-danger" %in% plotBtn$getClass())
    
    app$setInputs(xVar = "Petal.Length")
    runClick(app, "plot_run")
    
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "wait for the process"
    )
    expect_true("fm-wait" %in% plot$type)
  }
)


test_that(
  desc = "Cancel and error handling",
  code = {
    
    testthat::skip_on_cran()
    
    app <- shinytest::ShinyDriver$new(
      path = system.file("demoapp", package = "FutureManager"),
      loadTimeout = 10000 # 10s to spin up processes in Rstudio
    )
    app$waitForValue( # wait for the UI render
      name = "yVar", 
      iotype = "input"
    )
    runClick(app, "plot_run")
    Sys.sleep(1)
    app$setInputs(xVar = "Petal.Length")
    Sys.sleep(2)
    
    runClick(app, "plot_run", TRUE) # cancel the process
    
    app$waitFor( # wait for the cancel
      expr = "$('#plot').text() === 'run the process first';",
      checkInterval = 500,
      timeout = 2000 # 2s
    )
    
    plotBtn <- app$findElement("#plot_run")
    expect_true("btn-danger" %in% plotBtn$getClass())
    
    # expected error in the process (fmError)
    app$setInputs(xVar = "Species")
    runClick(app, "plot_run")
    
    app$waitFor(
      expr = "$('#plot').hasClass('shiny-output-error-fm-failed');",
      checkInterval = 500,
      timeout = 2000 # 2s
    )
    
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "Species column not allowed as xVar"
    )
    expect_true("fm-failed" %in% plot$type)
    
    plotBtn <- app$findElement("#plot_run")
    expect_true("btn-danger" %in% plotBtn$getClass())
    
    # unexpected error in the process (stop)
    app$setInputs(xVar = "Sepal.Width")
    app$setInputs(yVar = "Species")
    
    runClick(app, "plot_run")
    
    app$waitFor(
      expr = "! $('#plot').hasClass('shiny-output-error-validation');",
      checkInterval = 500,
      timeout = 2000 # 2s
    )
    
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "Species column not allowed as yVar"
    )
    expect_null(plot$type)
    
    plotBtn <- app$findElement("#plot_run")
    expect_true("btn-danger" %in% plotBtn$getClass())
  }
)
