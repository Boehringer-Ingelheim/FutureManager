library(testthat)
library(FutureManager)
library(shinytest)

if (!shinytest::dependenciesInstalled()){
  shinytest::installDependencies()
}

test_check("FutureManager")
