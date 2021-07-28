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

#' Extract a portion of a string between two patterns
#'
#' If a string has more than one match, this will only return the first one.
#'
#' @param full_string The full character string to extract from.
#' @param after_string A pattern specifying the string should be extracted after this.
#' @param before_string A pattern specifying the string should be extracted before this.
#' @param trim_spaces logical T/F, if there are any spaces within the extracted
#'   string, they will be removed. Default is TRUE.
#' @return A string
#' @export
#'
#' @examples
#'
#' extract_between("What does the fox say?", after_string = "What does the", before_string = "say")
#'
extract_between <- function(full_string, after_string, before_string, trim_spaces = TRUE) {

  reg_pattern <- paste0("(?<=", after_string, ").*(?=", before_string, ")")

  out <- regmatches(full_string, gregexpr(pattern = reg_pattern,
                                          text = full_string,
                                          perl = T))[[1]]

  if (trim_spaces) {
    out <- trimws(out)
  }

  return(out)
}


#' Extract all characters in a string before a match
#'
#' If a string has more than one match, this will only return the first one.
#'
#' @inheritParams extract_between
#' @return A string
#' @export
#'
#' @examples
#'
#' extract_before("What does the fox say?", before_string = "say")
#'
extract_before <- function(full_string, before_string, trim_spaces = TRUE) {

  reg_pattern <- paste0(".*(?=", before_string, ")")

  out <- regmatches(full_string, gregexpr(pattern = reg_pattern,
                                          text = full_string,
                                          perl = T))[[1]]

  if(length(out > 1)) out <- out[1]
  if (trim_spaces) out <- trimws(out)

  return(out)
}


#' Extract all characters in a string after a match
#'
#' If a string has more than one match, this will only return the first one.
#'
#' @inheritParams extract_between
#' @return A string
#' @export
#'
#' @examples
#'
#' extract_after("What does the fox say?", after_string = "What does the")
#'
extract_after <- function(full_string, after_string, trim_spaces = TRUE) {

  reg_pattern <- paste0("(?<=", after_string, ").*")

  out <- regmatches(full_string, gregexpr(pattern = reg_pattern,
                                          text = full_string,
                                          perl = T))[[1]]

  if (trim_spaces) {
    out <- trimws(out)
  }

  return(out)
}
