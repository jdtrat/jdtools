
#' Get a Substituted Name as Character for Subsetting
#'
#' This generic function is primarily useful for functions that accept column names for
#' subsetting data. The goal is to provide methods for users to supply either an
#' unquoted column name or a quoted one (character string). It makes use of the
#' fact that calling `substitute()` on a character string has a class
#' "character", and class "name" otherwise.
#'
#' @param x The substituted column name.
#'
#' @return The character vector for use with subsetting data.
#' @export
#'
#' @examples
#'
#' get_col <- function(data, column) {
#'   name <- substitute(column)
#'   data[, get_sub_name(name)]
#' }
#'
#' get_col(mtcars, hp)
#' get_col(mtcars, "hp")
#'
get_sub_name <- function(x) {
  UseMethod("get_sub_name")
}

#' @export
get_sub_name.name <- function(x) {
  deparse(x)
}

#' @export
get_sub_name.character <- function(x) {
  x
}
