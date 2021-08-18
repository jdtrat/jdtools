#' How Many Unique Observations
#'
#' This generic function calculates how many unique observations are in a vector
#' It is identical to `length(unique(x))`. However, this function can be used on
#' either a vector or a data frame with a specified column. See the examples for
#' more details.
#'
#' @param x Object such as a data.frame or vector
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return An integer indicating how many unique observations are in the object.
#' @export
#'
#' @examples
#'
#' how_many(mtcars$cyl) # Pass in vector directly
#' how_many(mtcars, "cyl") # Subset dataframe with a character string for column name
#' how_many(mtcars, cyl) # Subset dataframe without needing to quote the column name
#'
how_many <- function(x, ...) {
  UseMethod("how_many")
}

#' @export
how_many.default <- function(x) {
  length(unique(x))
}

# How many unique observations in a data frame column
#'
#' This is the S3 method for class 'data.frame'
#'
#' @param x Object of class 'data.frame'
#' @param column The column of the data frame to be indexed into.
#'
#' @return An integer indicating how many unique observations are in the object.
#' @export
#'
#' @examples
#'
#' how_many(mtcars, "cyl") # Subset dataframe with a character string for column name
#' how_many(mtcars, cyl) # Subset dataframe without needing to quote the column name
#'
how_many.data.frame <- function(x, column) {
  col <- get_sub_name(substitute(column))
  vec <- x[,col, drop = FALSE]
  vec <- vec[[col]]

  length(unique(vec))
}
