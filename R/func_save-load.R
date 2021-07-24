
#' Get Name of Object to Save/Load
#' @keywords internal
#' @noRd
get_name <- function(modifier = NULL, .sep = "_") {

  if (is.null(modifier)) {
    out_name <- paste0(jt$name, ".RDS")
  } else if (!is.null(modifier)) {
    out_name <- paste0(paste(jt$name, modifier, sep = .sep), ".RDS")
  }

  # If the out_name has length 3, it's most likely an R object from a package
  # with an explicit namespace call via ::
  if (length(out_name) == 3) {
    # Check that it is, if so, return the object name (third entry)
    if (grepl("\\:\\:", out_name[1])) {
      out_name[3]
      # if not, unknown error
    } else {
      unknown_error_msg()
    }
    # if length out_name is 1, then return as expected
  } else if (length(out_name) == 1) {
    out_name
  } else {
    unknown_error_msg()
  }

}

#' Get Path of Object to Save/Load
#' @keywords internal
#' @noRd
get_path <- function(.path) {

  option <- getOption("jdtools.rds.path")

  if (!is.null(option) && !missing(.path)) {
    path <- .path
    cli::cli_alert("{.code jdtools.rds.path} option is set to {.code {option}}. \n Since you supplied a path, however, the current object will be saved to and/or loaded from \n {.file {here::here(.path)}}.")
  } else if (!is.null(option) && missing(.path)) {
    path <- option
  } else if (is.null(option) && !missing(.path)) {
    path <- .path
  } else if (is.null(option) && missing(.path)) {
    cli::cli_abort("Oops! Option {.code jdtools.rds.path} is not set, nor was the {.code path} argument supplied.")
    cli::cli_alert("To set the directory for saving objects, call {.code jdtools::set_object_path()}.")
  }

  return(path)

}

#' Save/Load R Objects as .RDS files.
#'
#' These functions wrap [base::saveRDS()] and [base::readRDS()] and are
#' augmented with utility functions to make saving and loading R (.RDS) objects
#' easier.
#'
#' Specifically, these functions assume the use of an RStudio Project. The main
#' benefit of these functions is the consistency between the name of the R
#' object and the .RDS file name.
#'
#' Additionally, for analyses where there are lots of objects to be
#' saved/loaded, there are utilities to specify where these objects will be, e.g.
#' \code{\link{set_object_path}}.
#'
#' @param x The R object to save.
#' @param path A directory within the current RStudio Project.
#' @param filename_modifier The saved .RDS file name takes on the R object's
#'   name by default (when `filename_modifier` = NULL). Optionally, a filename
#'   extension can be added, such as a random string or date. See examples for
#'   more details.
#' @param sep Ignored by default. If `filename_modifier` is used, this is a
#'   character string used to separate the object name and modifier when writing
#'   the .RDS file.
#'
#' @return Either a .RDS file written to the specified location or an R object
#'   containing the contents of the .RDS file.
#'
#' @seealso utils-save-load
#'
#' @export
#'
#' @examples
#'
#'
#' if (interactive()) {
#'   library(fs)
#'   library(here)
#'   library(waldo)
#'
#'   # Create a directory test-folder in the current RStudio Project path
#'   dir_create(here("test-folder"))
#'
#'   # Save `mtcars` object to test-folder `here::here()` is run within
#'   # save_object, so it doesn't need to be supplied here, though it can be.
#'   save_object(mtcars, path = "test-folder")
#'
#'   # Check the objects saved. The .RDS file is automatically named with the object's name
#'   # `mtcars.RDS`
#'   dir_ls(here('test-folder'))
#'
#'   # The object can be loaded easily as:
#'   mtcars <- load_object(mtcars, path = "test-folder")
#'
#'   # .RDS files can also have a modifier to distinguish between similar R objects.
#'   mtcars <- mtcars[,1:3]
#'   save_object(mtcars, path = "test-folder",
#'               filename_modifier = "cols_1-3", sep = "_")
#'
#'   # It can be loaded back with the same arguments
#'   load_object(mtcars, path = "test-folder",
#'               filename_modifier = "cols_1-3", sep = "_")
#'
#'   # It could also be loaded back directly by name (should be character vector)
#'   load_object("mtcars_cols_1-3", path = "test-folder")
#'
#'   # For analyses where lots of objects will be reused, you can set the
#'   # global path option and not specifically include the path.
#'   set_object_path("test-folder")
#'
#'   # This now works! No path necessary.
#'   save_object(iris)
#'   load_object(iris)
#'
#'   # Custom filenames also work:
#'   save_object(iris, filename_modifier = "123")
#'   load_object(iris, filename_modifier = "123")
#'
#'   # You can still specify an alternative path even with that global option set.
#'   dir_create("another-test-folder")
#'
#'   # Modify iris for example
#'   iris$message <- "I'm in a new folder"
#'
#'   # Save modified iris to the new path
#'   save_object(iris, path = "another-test-folder")
#'   load_object(iris, path = "another-test-folder")
#'
#'   # Let's compare these
#'   orig <- load_object(iris)
#'   new <- load_object(iris, path = "another-test-folder")
#'
#'   # Should not be identical!
#'   compare(orig, new)
#'
#'   # Check the path
#'   get_object_path()
#'
#'   # Unset the path
#'   unset_object_path()
#'
#' }
#'
#'
save_object <- function(x, path, filename_modifier = NULL, sep = "_") {

  jt$name <- substitute(x)

  saveRDS(object = x,
          file = here::here(get_path(path), get_name(modifier = filename_modifier,
                                                     .sep = sep))
          )

  on.exit(rm("name", envir = jt), add = TRUE)

}

#' @rdname save_object
load_object <- function(x, path, filename_modifier = NULL, sep = "_") {

  jt$name <- substitute(x)

  object <- readRDS(here::here(get_path(path), get_name(modifier = filename_modifier,
                                              .sep = sep)))

  return(object)

  on.exit(rm("name", envir = jt), add = TRUE)

}
