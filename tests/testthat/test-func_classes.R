is_df <- function(x) {
  if (is.data.frame(x)) {
    cls_abort(x, expected_class = c("character", "numeric"))
  }
}

int_vec <- 1:5
chr_vec <- c("a", "b", "c", "d", "e")

test_that("`cls_abort()` works - regex", {

  expect_error(object = is_df(mtcars),
               regexp = "`x` must be <character> or <numeric>, not of class <data.frame>.")

})

test_that("`cls_abort()` works - class", {

  expect_error(object = is_df(mtcars),
               class = "cls_abort")

})

test_that("`cls_check()` works - no error messages", {
  expect_silent(cls_check(int_vec, "integer"))
  expect_silent(cls_check(chr_vec, "character"))
})

test_that("`cls_check()` works - error, regex", {
  expect_error(cls_check(int_vec, "numeric"),
               regexp = "`object` must be <numeric>, not of class <integer>.")

  expect_error(cls_check(chr_vec, "data.frame"),
                regexp = "`object` must be <data.frame>, not of class <character>.")
})

test_that("`cls_check()` works - error, class", {
  expect_error(cls_check(int_vec, "numeric"),
               class = "cls_abort")

  expect_error(cls_check(chr_vec, "data.frame"),
               class = "cls_abort")
})

