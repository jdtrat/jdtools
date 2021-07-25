test_that("time based id warning for excessive length works", {

  current_length <- nchar(format(Sys.time(), "%s"))

  expect_warning(time_string(length = current_length + 1))

})

test_that("time based id increments appropriately", {

  now <- time_string(3)
  Sys.sleep(1)
  now_again <- time_string(3)

  expect_equal(as.numeric(now), as.numeric(now_again ) - 1)

})
