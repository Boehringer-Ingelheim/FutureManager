library(dplyr)

test_that(
  desc = "fmStatus-class", 
  code = {
    obj <- fmStatus(
      id = "testId",
      status = "success",
      message = "Success msg",
      value = iris
    )
    
    expect_s3_class(obj, "fmStatus")
    expect_true(is.fmStatus(obj))
    expect_false(is.fmStatus(list()))
    expect_equal(
      object = obj[["id"]],
      expected = "testId"
    )
    expect_equal(
      object = obj[["status"]],
      expected = "success"
    )
    expect_equal(
      object = obj[["message"]],
      expected = "Success msg"
    )
    expect_is(obj[["timestamp"]], "POSIXct")
    expect_equal(
      object = obj[["value"]],
      expected = iris
    )
    expect_equal(
      object = fmGetValue(obj),
      expected = iris
    )
    
    obj2 <- fmStatus(
      id = "errorId",
      status = "error",
      message = "Something wrong",
      value = NULL
    )
    
    expect_error(suppressWarnings(fmGetValue(obj2)))
    expect_warning(
      object = tryCatch(fmGetValue(obj2), error = function(e) NULL),
      regexp = "The background process returned an error!"
    )
  }
)
