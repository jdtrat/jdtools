set_opts <- list(setting_option_1 = "one",
                 setting_option_2 = "two",
                 setting_option_3 = "three")

expected_opts <- list("testing.jdtools.setting_option_1" = "one",
                      "testing.jdtools.setting_option_2" = "two",
                      "testing.jdtools.setting_option_3" = "three")

test_that("setting options works", {

  expect_null(getOption("testing.jdtools.setting_option_1"))
  expect_null(getOption("testing.jdtools.setting_option_2"))
  expect_null(getOption("testing.jdtools.setting_option_3"))

  opt_set(set_opts, "testing.jdtools")

  expect_equal(getOption("testing.jdtools.setting_option_1"),"one")
  expect_equal(getOption("testing.jdtools.setting_option_2"),"two")
  expect_equal(getOption("testing.jdtools.setting_option_3"),"three")

  on.exit(options("testing.jdtools.setting_option_1" = NULL,
                  "testing.jdtools.setting_option_2" = NULL,
                  "testing.jdtools.setting_option_3" = NULL),
          add = TRUE)

})

test_that("listing options works", {
  listed <- opt_ls("testing.jdtools")
  names(listed) <- NULL
  expect_equal(list(), listed)

  opt_set(set_opts, "testing.jdtools")

  expect_equal(opt_ls("testing.jdtools"), expected_opts)

  on.exit(options("testing.jdtools.setting_option_1" = NULL,
                  "testing.jdtools.setting_option_2" = NULL,
                  "testing.jdtools.setting_option_3" = NULL),
          add = TRUE)

})

test_that("getting individual options works", {

  opt_set(set_opts, "testing.jdtools")

  one <- opt_get("setting_option_1", "testing.jdtools")
  expect_equal(one, "one")

  two <- opt_get("setting_option_2", "testing.jdtools")
  expect_equal(two, "two")

  three <- opt_get("setting_option_3", "testing.jdtools")
  expect_equal(three, "three")

  on.exit(options("testing.jdtools.setting_option_1" = NULL,
                  "testing.jdtools.setting_option_2" = NULL,
                  "testing.jdtools.setting_option_3" = NULL),
          add = TRUE)

})

test_that("removing options works -- none prefixed", {

  opt_set(set_opts, "testing.jdtools")
  # Retest they're set properly
  expect_equal(opt_ls("testing.jdtools"), expected_opts)

  opt_rm("setting_option_1", "testing.jdtools")

  expect_equal(opt_ls("testing.jdtools"), expected_opts[2:3])

  opt_rm("setting_option_3", "testing.jdtools")

  expect_equal(opt_ls("testing.jdtools"), expected_opts[2])

  opt_rm("setting_option_2", "testing.jdtools")

  listed <- opt_ls("testing.jdtools")
  names(listed) <- NULL
  expect_equal(list(), listed)

  on.exit(options("testing.jdtools.setting_option_1" = NULL,
                  "testing.jdtools.setting_option_2" = NULL,
                  "testing.jdtools.setting_option_3" = NULL),
          add = TRUE)
})


test_that("removing options works -- some prefixed", {

  opt_set(set_opts, "testing.jdtools")
  # Retest they're set properly
  expect_equal(opt_ls("testing.jdtools"), expected_opts)

  opt_rm("setting_option_1", "testing.jdtools")

  expect_equal(opt_ls("testing.jdtools"), expected_opts[2:3])

  opt_rm("testing.jdtools.setting_option_3", "testing.jdtools")

  expect_equal(opt_ls("testing.jdtools"), expected_opts[2])

  opt_rm("setting_option_2", "testing.jdtools")

  listed <- opt_ls("testing.jdtools")
  names(listed) <- NULL
  expect_equal(list(), listed)

  on.exit(options("testing.jdtools.setting_option_1" = NULL,
                  "testing.jdtools.setting_option_2" = NULL,
                  "testing.jdtools.setting_option_3" = NULL),
          add = TRUE)
})

test_that("removing options works -- all prefixed", {

  opt_set(set_opts, "testing.jdtools")
  # Retest they're set properly
  expect_equal(opt_ls("testing.jdtools"), expected_opts)

  opt_rm("testing.jdtools.setting_option_1", "testing.jdtools")

  expect_equal(opt_ls("testing.jdtools"), expected_opts[2:3])

  opt_rm("testing.jdtools.setting_option_3", "testing.jdtools")

  expect_equal(opt_ls("testing.jdtools"), expected_opts[2])

  opt_rm("testing.jdtools.setting_option_2", "testing.jdtools")

  listed <- opt_ls("testing.jdtools")
  names(listed) <- NULL
  expect_equal(list(), listed)

  on.exit(options("testing.jdtools.setting_option_1" = NULL,
                  "testing.jdtools.setting_option_2" = NULL,
                  "testing.jdtools.setting_option_3" = NULL),
          add = TRUE)
})

