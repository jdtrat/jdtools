#' Abort Classes with Informative Message
#'
#' This function is adapted from
#' [gargle](https://github.com/r-lib/gargle/blob/083acb186791c48294fd89f8d4fcb32cedc72aab/R/utils-ui.R#L172)'s
#' `abort_bad_class` function. It provides an informative error message
#' indicating the object and the class it must be for the function to work
#' properly.
#'
#' @param object The object whose class should be checked.
#' @param expected_class A character vector of expected (or allowed) classes.
#'
#' @return NA; triggers an error.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   is_df <- function(x) {
#'     if (is.data.frame(x)) {
#'       cls_abort(mtcars, expected_class = c("character", "numeric"))
#'     }
#'   }
#'
#'   is_df(mtcars)
#'
#' }
#'
#'
cls_abort <- function(object, expected_class) {
  obj_name <- deparse(substitute(object))
  obj_class <- class(object)

  must_be <- glue::glue_collapse(
    glue::glue("{.cls <<expected_class>>}",
               .open = "<<", .close = ">>"
    ),
    sep = ", ", last = " or "
  )

  msg <- glue::glue("{.arg {obj_name}} must be <<must_be>>, not of class {.cls {obj_class}}.",
                    .open = "<<", .close = ">>")

  cli::cli_abort(msg)

}


#' Informative Class Checking
#'
#' Easily check if an object has an expected class. This function checks that
#' (all) the class(es) of the `object` are *not* in the `expected_class` vector.
#'
#' @inheritParams cls_abort
#'
#' @return The object's class(es), invisibly.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   # No error
#'   cls_check(int_vec, expected_class = "integer")
#'   # Error
#'   cls_check(int_vec, expected_class = "character")
#'
#'   # No error
#'   cls_check(chr_vec, expected_class = "character")
#'   # Error
#'   cls_check(chr_vec, expected_class = "integer")
#'
#'   # No error
#'   cls_check(df, expected_class = "data.frame")
#'   cls_check(df, expected_class = c("data.frame", "integer", "character"))
#'   # Error
#'   cls_check(df, expected_class = c("integer", "character"))
#'
#'
#'   if (requireNamespace("tibble")) {
#'     library(tibble)
#'
#'     tbl_cars <- tibble(mtcars)
#'
#'     # See the classes of tbl_cars
#'     # 'tbl_df', 'tbl', and 'data.frame'
#'     class(tbl_cars)
#'
#'     # Check that tbl_cars has at least one of the expected class
#'     # This will return an error
#'     cls_check(object = tbl_cars,
#'               expected_class = c("character", "raw", "logical"))
#'
#'     # This won't return an error since 'data.frame' is a class
#'     # of tbl_cars. It will invisibly return the 'tbl_cars' classes.
#'     cls_check(object = tbl_cars,
#'               expected_class = c("character", "raw", "data.frame"))
#'
#'   }
#'
#' }
#'
cls_check <- function(object, expected_class) {

  .class <- class(object)

  if (all(.class %nin% expected_class)) {
    cls_abort(object = object, expected_class = expected_class)
  }
  invisible(.class)
}
