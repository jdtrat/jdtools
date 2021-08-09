
#' Pull the development version of jdtools from GitHub
#'
#' This is a simple utility function to easily install jdtools from GitHub. It
#' is equivalent to `remotes::install_github(repo = "jdtrat/jdtools")`.
#'
#' @param ... Additional arguments to pass along to [remotes::install_github()].
#'
#' @return NA; used for side effects to install the development version of jdtools from GitHub.
#' @export
#'
pull_jdtools <- function(...) {
  remotes::install_github(repo = "jdtrat/jdtools", ...)
}

#' Restart the R Session in RStudio
#'
#' This function is simply a wrapper around [rstudioapi::restartSession()] and,
#' if called within RStudio, will restart the R session. It's a convenient way
#' to restart the current R Session.
#'
#' @param command A command (as a character string) to be run after restarting.
#'
#' @return NA; used for side effects.
#'
#' @export
#'
rs <- function(command = "") {
  check_rstudio()
  rstudioapi::restartSession(command = command)
}

#' Inverse Value Matching
#'
#' This operator is simply the inverse of the native `%in%` operator, and will
#' return true if the left-hand side is **not** in the right hand side. See the
#' examples for more details.
#'
#' @param x vector or `NULL`; the values to be checked.
#' @param table vector or `NULL`, the vectors to be checked against.
#'
#' @return A logical vector indicating if a match was **not** located for each
#'   element of `x`.
#'
#' @export
#'
#' @examples
#'
#' best_cats <- c("Tucker", "Oliver", "Mr. Loon")
#'
#' # Returns TRUE, obviously
#' "Tucker" %in% best_cats
#'
#' # Returns FALSE (because Tucker is the best cat)
#' "Tucker" %nin% best_cats
#'
#'
#' # Create a vector of vowels
#' vowels <- c("a", "e", "i", "o", "u")
#'
#' # Create a sentence
#' sentence <- "Sushi is great!"
#' # Split sentence into individual letters
#' sentence_letters <- strsplit(sentence, "")[[1]]
#'
#' # Subset the sentence for all vowels
#' sentence_letters[sentence_letters %in% vowels]
#' # Subset the sentence for all consonants
#' sentence_letters[sentence_letters %nin% vowels]
#'
#'
#' # Vector of numbers 1 through 10
#' all_nums <- seq(0,10)
#' # Vector of a few numbers between 1 through 10
#' sub_nums <- c(1,3,6)
#'
#' # Which numbers between 1 and 10 are in the subset
#' all_nums[all_nums %in% sub_nums]
#'
#' # Which numbers between 1 and 10 are not in the subset
#' all_nums[all_nums %nin% sub_nums]
#'
`%nin%` <- function(x, table) {
  !match(x, table, nomatch = 0L) > 0L
}

# Utility function to check if current RStudio browser is in dark mode.
# Returns TRUE or FALSE. If not using RStudio, return FALSE.
is_rstudio_dark <- function() {
  if (!rstudioapi::isAvailable()) FALSE else rstudioapi::getThemeInfo()$dark
}
