#' Subset a vector for values within a specified range
#'
#' This function is a modified version of [dplyr::between()]. Unlike `dplyr`'s
#' version, `sub_between` returns the subset of the supplied vector within the
#' specified boundaries. To mimic the behavior of [dplyr::between()], you can
#' set `subset = FALSE`.
#'
#' @param vec A numeric vector
#' @param left The leftmost (lower) boundary value
#' @param right The rightmost (upper) boundary value
#' @param subset Logical: TRUE and only the values within the specified
#'   boundaries will be returned from the supplied vector. FALSE and only
#'   logical indicators will be.
#'
#' @return If `subset = TRUE` (default), this function returns the supplied
#'   vector subsetted within the specified boundaries. If `subset = FALSE`, this
#'   function returns a logical vector indicating whether a specific entry in
#'   the supplied vector is within the specified range.
#'
#' @export
#'
#' @examples
#' # Create a vector from 0 to 20
#' x <- seq(1,20)
#' # Save x_lower as the lower half of the vector
#' x_lower <- sub_between(vec = x, left = min(x), right = median(x))
#'
#' # Save x_upper as the upper half of the vector
#' x_upper <- sub_between(vec = x, left = median(x), right = max(x))
#'
#' # Combine the vectors and check its equal to the original
#' # Using all.equal because `sub_between()` coerces to numeric class
#' all.equal(c(x_lower, x_upper), x)
#'
#' # Get the vector for all values between 5 and 10
#' x5_10 <- sub_between(vec = x, left = 5, right = 10)
#'
#' # To return logical indices, like `dplyr::between()`,
#' # you can set `subset = FALSE`:
#' sub_between(vec = x, left = 5, right = 10, subset = FALSE)
#'
#' # To return the vector, like with `dplyr::between()`, you would subset
#' # it as follows:
#'
#' x_indexed <- x[sub_between(vec = x, left = 5, right = 10, subset = FALSE)]
#'
#' # Check that the values are equal
#' all.equal(x5_10, x_indexed)
#'
sub_between <- function(vec, left, right, subset = TRUE) {

  if (!is.null(attr(vec, "class")) && !inherits(vec, c("Date", "POSIXct"))) {
    warning("vec_between() called on numeric vector with S3 class")
  }

  if (length(left) != 1) {
    stop("`left` must be length 1")
  }
  if (length(right) != 1) {
    stop("`right` must be length 1")
  }
  if (!is.double(vec)) {
    vec <- as.numeric(vec)
  }

  index <- vec >= left & vec <= right

  if (subset) vec[index] else index

}
