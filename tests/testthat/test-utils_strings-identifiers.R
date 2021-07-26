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


main_str <- "Telomeres are the protective sheaths on the ends of chromosomes that are responsible for cellular aging."
before_answer <- "Telomeres are the protective sheaths on the ends of chromosomes that are responsible for"
after_answer <- "cellular aging."
between_answer <- "are the protective sheaths on the ends of chromosomes that are responsible for"

test_that("extract_before works", {

  local_edition(3)

  before <- extract_before(full_string = main_str,
                           before_string = "cellular aging")

  expect_equal(before, before_answer)


  before_2 <- extract_before("What does the fox say?", before_string = "say")
  expect_equal(before_2, "What does the fox")

})

test_that("extract_after works", {

  after <- extract_after(full_string = main_str,
                         after_string = "responsible for")

  expect_equal(after, after_answer)

  after_2 <- extract_after("What does the fox say?", after_string = "What does the")
  expect_equal(after_2, "fox say?")


})

test_that("extract_between works", {

  between <- extract_between(full_string = main_str,
                             after_string = "Telomeres",
                             before_string = "cellular aging")

  expect_equal(between, between_answer)

  between_2 <- extract_between("What does the fox say?", after_string = "What does the", before_string = "say")

  expect_equal(between_2, "fox")

})
