test_that("how many works - vector", {

  expect_equal(how_many(letters), 26)

})

test_that("how many works - data frane", {

  expect_equal(how_many(mtcars, cyl), 3)

})
