
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
  if (!rstudioapi::isAvailable()) {
    cli::cli_abort("Oops. Looks like you're not using RStudio.")
  }
  rstudioapi::restartSession(command = command)
}

