library(FutureManager)
library(testthat)

test_that(
  desc = "fmStatus methods work correctly",
  code = {
    status <- fmStatus(
      id = "dummy",
      status = "success",
      message = "Job completed",
      value = iris
    )
    
    expect_equal(
      object = status %>% filter(Species == "setosa") %>% nrow(),
      expected = 50
    )
    expect_equal(
      object = status %>% arrange(Species) %>% nrow(),
      expected = 150
    )
    expect_equal(
      object = status %>% mutate(Species = toupper(Species)) %>% pull(Species) %>% unique(),
      expected = c("SETOSA", "VERSICOLOR", "VIRGINICA")
    )
    expect_equal(
      object = status %>% select(Petal.Width, Petal.Length) %>% names(),
      expected = c("Petal.Width", "Petal.Length")
    )
    expect_equal(
      object = status %>% rename(pw = Petal.Width, pl = Petal.Length) %>% names(),
      expected = c("Sepal.Length", "Sepal.Width", "pl", "pw", "Species")
    )
    expect_equal(
      object = status %>% tbl_vars() %>% as.character(),
      expected =  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
    )
    expect_equal(
      object = status %>% group_vars(),
      expected = character(0)
    )
    expect_equal(
      object = status[c("Sepal.Length", "Petal.Width")] %>% names(),
      expected = c("Sepal.Length", "Petal.Width")
    )
    expect_output(
      object = print(status),
      regexp = "dummy \\[success\\]"
    )
    expect_output(
      object = print(status),
      regexp = "msg: Job completed"
    )
    expect_output(
      object = print(status),
      regexp = "value: data.frame-class object"
    )
  }
)