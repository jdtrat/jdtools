get_col <- function(data, column) {
  name <- substitute(column)
  data[,get_sub_name(name)]
}

test_that("get_sub_name() works with unquoted column name", {

  expect_equal(mtcars$hp, get_col(mtcars, hp))

})

test_that("get_sub_name() works with quoted column name", {

  expect_equal(mtcars$hp, get_col(mtcars, "hp"))

})
