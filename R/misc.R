
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
