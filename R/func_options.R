#' Set, Get, and Remove (Package-Specific) Options
#'
#' @description These are utility functions for setting, getting, and removing
#'   package-specific options. The
#'   [withr](https://withr.r-lib.org/articles/changing-and-restoring-state.html)
#'   package describes the dangers of changing the landscape of R by modifying
#'   things like search paths, global options, or working directories.
#'   Specifically, they write "If the behavior of *other* functions differs
#'   before and after running your function, you've modified the landscape."
#'
#'   The `withr` package provides elegant solutions that are more in-line with
#'   best practices. However, I found that there options I'd like to use within
#'   personal packages, such as `jdtools`, and I believe it makes sense to
#'   modify the R landscape. To avoid potential issues as best as possible, the
#'   set of functions here prefix any options with a package name.
#'
#'   This allows users to make some changes to the R landscape but in a more
#'   controlled manner that should avoid many conflicts.
#'
#'   Thanks to [TJ Mahr](https://github.com/tjmahr), [Tan
#'   Ho](https://github.com/tanho63), and Tyler Grant Smith for providing code
#'   that helped solve option-setting issues.
#'
#' @param option A named vector or list corresponding to the options and values.
#' @param package A character string corresponding to the package name whose
#'   prefix you would like to set.If the package parameter is not supplied,
#'   these functions assume you are working on a package and will prefix the
#'   options with the name of the package via [desc::desc_get()]. If you do not
#'   pass in a package parameter and are not working on a package, an error will
#'   be thrown. Can also set the package parameter globally with
#'   `options(jdtools.opts_package_name = "THE_PACKAGE_NAME")`.
#'
#' @return Either nothing, when used for option manipulations, or character
#'   vectors of existing options.
#'
#' @export
#'
#' @examples
#'
#' #' ## Always define package parameter
#' # List current {jdtools} options
#' opt_ls("jdtools")
#'
#' # Set new options
#' # Either a list
#' opt_set(list(favorite_color = "black",
#'              favorite_food = "sushi",
#'              favorite_pet = "tucker"),
#'         "jdtools")
#' # Or a character vector
#' opt_set(c(favorite_color = "black",
#'           favorite_food = "sushi",
#'           favorite_pet = "tucker"),
#'         "jdtools")
#'
#' opt_ls("jdtools")
#'
#' # Either a list
#' opt_rm(list("favorite_color", "favorite_food"), "jdtools")
#'
#' # Or a character vector
#' opt_rm(c("favorite_color", "favorite_food"), "jdtools")
#'
#' opt_ls("jdtools")
#'
#' # Can remove all at once in an interactive session:
#' if (interactive()) opt_rm_all("jdtools")
#'
#'
#' ## Can set global package option to avoid specifying it in the future:
#' opt_set(c(opts_package_name = "jdtools"), "jdtools")
#'
#' # List current {jdtools} options
#' opt_ls()
#'
#' # Set new options
#' opt_set(c(favorite_color = "black",
#'           favorite_food = "sushi",
#'           favorite_pet = "tucker"))
#'
#' opt_ls()
#'
#' # Either a list
#' opt_rm(list("favorite_color", "favorite_food"))
#'
#' opt_ls()
#'
#' # Can remove all at once in an interactive session:
#' if (interactive()) opt_rm_all("jdtools")
#'
opt_set <- function(option, package) {

  new <- if (!is.list(option)) as.list(option) else option
  names(new) <- paste0(pkg_spec(package), ".", names(new))
  options(new)

}

#' @export
#' @rdname opt_set
opt_get <- function(option, package) {

  if (grepl(pkg_spec(package), option)) {
    getOption(option)
  } else {
    getOption(paste0(pkg_spec(package), ".", option))
  }
}

#' @export
#' @rdname opt_set
opt_ls <- function(package) {
  pkg <- pkg_spec(package)
  options()[grepl(pkg, names(options()))]
}

#' @export
#' @rdname opt_set
opt_rm <- function(option, package) {

  new <- if (!is.list(option)) as.list(option) else option

  names(new) <-  vapply(
    X = vapply(option, "[", FUN.VALUE = character(1)),
    FUN = function(name, package) {
      if (grepl(pkg_spec(package), name)) {
        name
      } else {
        paste0(pkg_spec(package), ".", name)
      }
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    package = package
  )

  new <- lapply(new, function(x) return(NULL))
  options(new)
}

#' @export
#' @rdname opt_set
opt_rm_all <- function(package) {
  pkg <- pkg_spec(package)
  pkg_opts <- vapply(opt_ls(package), "[", FUN.VALUE = character(1))
  cli::cli_text("Are you sure you want to remove the {.pkg {pkg}} options? Currently, they are: \n\n")
  cli::cli_ul(paste0(color_options(names(pkg_opts)), ": ", pkg_opts))
  cl_yes_no_action(prompt = " ",
                   yes_action = opt_rm(names(pkg_opts), package = package),
                   yes_message = "Okay, the {.envvar {names(pkg_opts)}} option{?s} {?has/have} been removed.
                   You can verify this by calling {.code jdtools::opt_ls(package = \"{pkg}\")}",
                   no_message = "Okay, not deleting the {.envvar {names(pkg_opts)}} option{?s}.")
}



