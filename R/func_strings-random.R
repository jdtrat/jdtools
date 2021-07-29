#' Get possible values for random string generation
#'
#' @param digits Should digits be an option (0-9). TRUE by default.
#' @param upperalpha Should capital letters be an option (A-Z). TRUE by default.
#' @param loweralpha Should lowercase letters be an option (a-z). TRUE by default.
#' @keywords internal
#'
#' @return A character vector of some combination of digits, uppercase, and lowercase letters.
#'
get_possible_values <- function(digits = TRUE,
                                upperalpha = TRUE,
                                loweralpha = TRUE) {


  if (digits == TRUE) {digits <- as.character(seq(0,9))
  } else if (digits != TRUE) {digits <- NULL}

  if (upperalpha == TRUE) {upperalpha <- LETTERS
  } else if (upperalpha != TRUE) {upperalpha <- NULL}

  if (loweralpha == TRUE) {loweralpha <- letters
  } else if (loweralpha != TRUE) {loweralpha <- NULL}

  possible_values <- c(digits, loweralpha, upperalpha)

  return(possible_values)

}

#' Get individual random string
#'
#' @inheritParams get_possible_values
#' @param length The length of the random string
#' @keywords internal
#'
#' @return One random string
#'
get_individual_string <- function(length = 6,
                                  digits = TRUE,
                                  upperalpha = TRUE,
                                  loweralpha = TRUE) {

  values <- get_possible_values(digits = digits,
                                upperalpha = upperalpha,
                                loweralpha = loweralpha)
  num_possible_values <- length(values)
  positions <- round(stats::runif(n = length,
                                  min = 1,
                                  max = num_possible_values))

  random_string <- paste(values[positions], collapse = "")

  return(random_string)

}


#' Remove duplicate strings from character vectors
#'
#' @param chr_vec The character vector to check for uniqueness
#' @inheritParams get_individual_string
#'
#' @keywords internal
#' @return The character vector without any duplicates
#'

check_unique <- function(chr_vec,
                         length,
                         digits,
                         upperalpha,
                         loweralpha) {

  duplicates <- duplicated(chr_vec)
  num_duplicates <- sum(duplicates)


  if (num_duplicates == 0) {
    vec <- chr_vec
    return(vec)
  } else if (num_duplicates > 0) {
    new_strings <- purrr::map_chr(1:num_duplicates,
                                  ~get_individual_string(
                                    length = length,
                                    digits = digits,
                                    upperalpha = upperalpha,
                                    loweralpha = loweralpha
                                  ))
    vec <- c(chr_vec[!duplicates], new_strings)

    return(
      check_unique(chr_vec = vec,
                   length = length,
                   digits = digits,
                   upperalpha = upperalpha,
                   loweralpha = loweralpha)
    )

  }
}


#' Generate Random Strings
#'
#' Generate random strings of any length using a combination of uppercase or
#' lowercase letters and digits 0-9.
#'
#' @param num_strings Number of strings
#' @param length Length of strings
#' @param digits Logical: TRUE and digits will be in string, FALSE and they won't.
#' @param upperalpha Logical: TRUE and capital letters will be in string, FALSE and they won't.
#' @param loweralpha Logical: TRUE and lowercase will be in string, FALSE and they won't.
#' @param unique Logical: TRUE and strings will be unique, FALSE and they may not be.
#'
#' @return A character vector of random strings defined by the parameters as above.
#' @export
#'
#' @examples
#'
#' user_ids <- data.frame(id = rand_str(num_strings = 50))
#'
rand_str <- function(num_strings = 10,
                     length = 6,
                     digits = TRUE,
                     upperalpha = TRUE,
                     loweralpha = TRUE,
                     unique = TRUE) {


  strings <- purrr::map_chr(1:num_strings, ~get_individual_string(length = length,
                                                        digits = digits,
                                                        upperalpha = upperalpha,
                                                        loweralpha = loweralpha))

  if (unique) {
    return(
      check_unique(chr_vec = strings,
                   length = length,
                   digits = digits,
                   upperalpha = upperalpha,
                   loweralpha = loweralpha)
    )
  } else {
    return(strings)
  }

}
