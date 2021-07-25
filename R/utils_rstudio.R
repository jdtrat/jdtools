
#' Restart the R Session in RStudio
#'
#' This function is simply a wrapper around [rstudioapi::restartSession()] and,
#' if called within RStudio, will restart the R session.
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
