x <- seq(1,20) # Create vector

test_that("vec between works - lower half", {

  # Save x_lower as the lower half of the vector
  x_lower <- vec_between(vec = x, left = min(x), right = median(x))

  expect_equal(x_lower, 1:10)

})


test_that("vec between works - upper half", {

  # Save x_upper as the upper half of the vector
  x_upper <- vec_between(vec = x, left = median(x), right = max(x))

  expect_equal(x_upper, 11:20)

})

test_that("vec between works - five to ten", {
  # Get the vector for all values between 5 and 10
  x5_10 <- vec_between(vec = x, left = 5, right = 10)

  expect_equal(x5_10, 5:10)

})

test_that("vec between works - subset FALSE", {

  x_indexed <- x[vec_between(vec = x, left = 5, right = 10, subset = FALSE)]

  expect_equal(x_indexed, 5:10)

})

test_that("vec between works - mtcars with logical subset", {
  vec_version <- subset(mtcars, vec_between(hp, 90, 110, subset = FALSE))
  base_version <- mtcars[which(mtcars$hp >= 90 & mtcars$hp <= 110),]
  expect_equal(vec_version, base_version)
})
