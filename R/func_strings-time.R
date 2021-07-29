#' Get a Character String of Seconds since January, 1970
#'
#' This function returns a character string of user-defined length based on the
#' current number of seconds since January 1st, 1970. It is useful for
#' prototyping data storage pipelines where the actual file name may be unknown.
#'
#' @param length Number of characters to make the string.
#'
#' @return A character string based on the current time since January 1st, 1970.
#' @export
#'
#' @examples
#'
#' time_string(length = 4)
#' time_string(length = 6)
#' # Will throw a warning
#' time_string(length = 4646)
#'
time_string <- function(length) {
  time <- format(Sys.time(), "%s")
  n_time <- nchar(time)
  if (length > n_time) {
    warning(paste0("Only ", n_time, " digits make up the number of seconds since January, 1970. Ignoring supplied length, and defaulting to ", n_time, " digits."))
  }
  substr(time, start = (1 + n_time - length), stop = n_time)
}
