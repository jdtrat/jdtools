test_that("rand_str `num_strings` works", {
  expect_equal(100, length(rand_str(num_strings = 100)))
})

test_that("rand_str `length` works", {

  length_6 <- rand_str(num_str = 100, length = 6)
  length_10 <- rand_str(num_str = 100, length = 10)
  length_20 <- rand_str(num_str = 100, length = 20)

  expect_true(all(nchar(length_6) == 6))
  expect_true(all(nchar(length_10) == 10))
  expect_true(all(nchar(length_20) == 20))
})

test_that("rand_str `digits` works", {
  no_digits <- rand_str(num_str = 100, digits = FALSE)
  digits <- rand_str(num_str = 100, digits = TRUE)

  expect_false(any(grepl(pattern = "[[:digit:]]", x = no_digits)))

  expect_true(any(grepl(pattern = "[[:digit:]]", x = digits)))

})

test_that("rand_str `upperalpha` works", {
  no_upperalpha <- rand_str(num_str = 100, upperalpha = FALSE)
  upperalpha <- rand_str(num_str = 100, upperalpha = TRUE)

  expect_false(any(grepl(pattern = "[[:upper:]]", x = no_upperalpha)))

  expect_true(any(grepl(pattern = "[[:upper:]]", x = upperalpha)))

})


test_that("rand_str `upperalpha` works", {
  no_loweralpha <- rand_str(num_str = 100, loweralpha = FALSE)
  loweralpha <- rand_str(num_str = 100, loweralpha = TRUE)

  expect_false(any(grepl(pattern = "[[:lower:]]", x = no_loweralpha)))

  expect_true(any(grepl(pattern = "[[:lower:]]", x = loweralpha)))

})

test_that("rand_str `unique` works", {

  no_unique <- rand_str(num_str = 1000, length = 2, unique = FALSE)
  unique <- rand_str(num_str = 1000, length = 2, unique = TRUE)

  expect_gt(sum(duplicated(no_unique)), 0)

  expect_equal(sum(duplicated(unique)), 0)

})
