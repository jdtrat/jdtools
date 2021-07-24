
jt <- new.env(parent = emptyenv())

unknown_error_msg <- function() {
  cli::cli_abort("Unknown error. Please try supplying a different object, or filing an issue at {.url https://github.com/jdtrat/jdtools/issues}.")
}


#' Set global path option for saving/loading objects
#'
#' @param path A directory within the current RStudio Project
#'
#' @return NA; used for side effects to set the global option
#'   `jdtools.rds.path`.
#'
#' @family utils-save-load
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' fs::dir_create(here::here("test-folder"))
#' jdtools::set_object_path("test-folder")
#'
#' }
#'
set_object_path <- function(path) {

  options(jdtools.rds.path = path)

  if (getOption("jdtools.rds.path") == path) {
    cli::cli_alert_success("Calling {.code jdtools::save_object()} will save objects to {.file {here::here(path)}}.")
  }
}

#' Get global path option for saving/loading objects
#'
#' @return NA; used for side effects to get the global option
#'   `jdtools.rds.path` if applicable.
#'
#' @family utils-save-load
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' fs::dir_create(here::here("test-folder"))
#' # Will state option is unset
#' jdtools::get_object_path()
#'
#' jdtools::set_object_path("test-folder")
#' # Will return path
#' jdtools::get_object_path()
#'
#' }
#'
get_object_path <- function() {

  path <- getOption("jdtools.rds.path")

  if (is.null(path)) {
    cli::cli_alert_danger("Option {.envvar jdtools.rds.path} is not set. \n To set the directory for saving objects, call {.code jdtools::set_object_path()}")
  } else {
    cli::cli_alert_success("{.envvar jdtools.rds.path} option is set to {.file {here::here(path)}}.")
  }

}

#' Unset global path option for saving/loading objects
#'
#' @return NA; used for side effects to unset the global option
#'   `jdtools.rds.path`.
#'
#' @family utils-save-load
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' fs::dir_create(here::here("test-folder"))
#' # Will get message saying no path is set.
#' jdtools::unset_object_path("test-folder")
#' jdtools::set_object_path("test-folder")
#' # Will actually unset the path.
#' jdtools::unset_object_path("test-folder")
#'
#' }
#'
unset_object_path <- function() {

  path <- getOption("jdtools.rds.path")

  if (is.null(path)) {
    cli::cli_alert_danger("Option {.envvar jdtools.rds.path} is not defined, so it cannot be unset.")
  } else if (!is.null(path)) {
    options("jdtools.rds.path" = NULL)
    cli::cli_alert_success("Option {.envvar jdtools.rds.path} has been unset.")
  }

}
