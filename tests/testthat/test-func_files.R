test_that("creating a new R file works", {

  file_name <- file.path(tempdir(check = TRUE), "hello.R")

  expect_false(file.exists(file_name))

  file_new(file_name, "R", open = FALSE)

  expect_true(file.exists(file_name))

  on.exit(unlink(tempdir(), recursive = TRUE), add = TRUE)

})

test_that("creating a new JS file works", {

  file_name <- file.path(tempdir(check = TRUE), "hello.js")

  expect_false(file.exists(file_name))

  file_new(file_name, "js", open = FALSE)

  expect_true(file.exists(file_name))

  on.exit(unlink(tempdir(), recursive = TRUE), add = TRUE)

})

test_that("creating a new markdown file works", {

  file_name <- file.path(tempdir(check = TRUE), "hello.md")

  expect_false(file.exists(file_name))

  file_new_md(file_name, open = FALSE)

  expect_true(file.exists(file_name))

  on.exit(unlink(tempdir(), recursive = TRUE), add = TRUE)

})

test_that("creating a new text file works", {

  file_name <- file.path(tempdir(check = TRUE), "hello.txt")

  expect_false(file.exists(file_name))

  file_new_text(file_name, open = FALSE)

  expect_true(file.exists(file_name))

  on.exit(unlink(tempdir(), recursive = TRUE), add = TRUE)

})

test_that("deleting a file works", {

  file_name <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".R")

  expect_false(file.exists(file_name))

  writeLines("I exist!", con = file_name)

  expect_true(file.exists(file_name))

  file_delete(file_name, ask = FALSE)

  expect_false(file.exists(file_name))

  on.exit(unlink(tempdir(), recursive = TRUE), add = TRUE)

})

test_that("copying files works", {

  file_name <- tempfile(tmpdir = tempdir(check = TRUE), fileext = ".R")
  copy_name <- tempfile(pattern = "copy", tmpdir = tempdir(check = TRUE), fileext = ".R")

  expect_false(file.exists(file_name))
  expect_false(file.exists(copy_name))

  writeLines("I exist!", con = file_name)

  expect_true(file.exists(file_name))
  file_copy(file_name, copy_name)
  expect_true(file.exists(copy_name))

  on.exit(unlink(tempdir(), recursive = TRUE), add = TRUE)

})
