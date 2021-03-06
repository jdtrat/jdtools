#' Create a new file
#'
#' @description These functions are inspired by [usethis::use_r()] to create .R files in the
#' R/ directory for packages. Unlike `use_r()`, these functions allow you to
#' pass in any path within your RStudio Project. It will then create and
#' (optionally) open the file within RStudio.
#'
#' There is both a generic `file_new()` function, which accepts paths and the
#' file extension, as well as several utility functions that simply require a
#' path. See usage or examples for more details.
#'
#' @param path A character vector of the file path to be created.
#' @param ext The file extension, e.g. "R" or "Rmd" for .R and .Rmd files,
#'   respectively.
#' @param open Logical: whether to open the file for editing. Default calls
#'   [base::interactive()].
#'
#' @return NA; used to create and (optionally) open a new file of the specified
#'   type.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   # Create a new R file, specifying the extension
#'   file_new(path = "test-folder/test-file", ext = "R")
#'
#'   # Create a new R file with the shortcut:
#'   file_new_r(path = "test-folder/test-file")
#'
#'   # Create a new Stan file
#'   file_new(path = "test-folder/test-file", ext = "stan")
#'   # Alternatively:
#'   # file_new_stan(path = "test-folder/test-file")
#'
#'   # Create a new JavaScript file
#'   file_new(path = "test-folder/test-file", ext = "js")
#'   # Alternatively:
#'   # file_new_js(path = "test-folder/test-file")
#'
#'   # Create a new CSS file
#'   file_new(path = "test-folder/test-file", ext = "css")
#'   # Alternatively:
#'   # file_new_css(path = "test-folder/test-file")
#'
#'   # Create a new HTML file
#'   file_new(path = "test-folder/test-file", ext = "html")
#'   # Alternatively:
#'   # file_new_html(path = "test-folder/test-file")
#'
#'   # Create a new Text file
#'   file_new(path = "test-folder/test-file", ext = "txt")
#'   # Alternatively:
#'   # file_new_text(path = "test-folder/test-file")
#'
#'   # Create a new Markdown file
#'   file_new(path = "test-folder/test-file", ext = "md")
#'   # Alternatively:
#'   # file_new_md(path = "test-folder/test-file")
#'
#'   # Create a new RMarkdown file
#'   file_new(path = "test-folder/test-file", ext = "Rmd")
#'   # Alternatively:
#'   # file_new_rmd(path = "test-folder/test-file")
#'
#' }
#'
file_new <- function(path, ext, open = interactive()) {

  path <- check_file_ext(.path = path,
                         .ext = ext)

  file_dir <- get_file_dir(.path = path)

  if (dir_exists_prompt(.path = path)) {
    create_file(path, open)
  }

}


# Function factory version of file_new
file_new_ff <- function(ext) {

  function(path, open = interactive()) {

    path <- check_file_ext(.path = path,
                           .ext = ext)

    file_dir <- get_file_dir(.path = path)

    if (dir_exists_prompt(.path = path)) {
      create_file(path, open)
    }

  }

}

#' @rdname file_new
#' @export
file_new_r <- file_new_ff("R")
#'
#' @rdname file_new
#' @export
file_new_stan <- file_new_ff("stan")

#' @rdname file_new
#' @export
file_new_js <- file_new_ff("js")

#' @rdname file_new
#' @export
file_new_css <- file_new_ff("css")

#' @rdname file_new
#' @export
file_new_html <- file_new_ff("html")

#' @rdname file_new
#' @export
file_new_text <- file_new_ff("txt")

#' @rdname file_new
#' @export
file_new_md <- file_new_ff("md")

#' @rdname file_new
#' @export
file_new_rmd <- file_new_ff("Rmd")

#' Open a File in RStudio
#'
#' This function is simply a wrapper around [rstudioapi::navigateToFile()] and
#' will open the file specified by the `path` argument in the current RStudio
#' window.
#'
#' @param path A character vector of the file to be opened.
#' @param ... Additional arguments to be passed into
#'   [rstudioapi::navigateToFile()].
#'
#' @return NA; used for side effects to open a file in RStudio.
#' @export
#'
file_open <- function(path, ...) {
  if (!rstudioapi::isAvailable()) {
    cli::cli_abort("Oops. Looks like you're not using RStudio.")
  }

  path <- here::here(path)

  if (fs::file_exists(path = path)) {
    rstudioapi::navigateToFile(file = path, ...)
  } else if (!fs::file_exists(path = path)) {
    cl_yes_no_action(
      prompt = "It looks like you're trying to open a file {.file {path}} that does not exist. Would you like to create it?",
      yes_action = {
        fs::file_create(path = path)
        cli::cli_alert_success("Created {.file {path}}. Opening now!")
        rstudioapi::navigateToFile(path, ...)
      },
      no_action = return(invisible()),
      no_message = "Okay, not creating {.file {path}}."
    )
  }
}

#' Copy Existing Files to a new Directory
#'
#' This function is a wrapper around [fs::file_copy()], with the modification that,
#' if a file exists at the `new_path` the user will be prompted to overwrite it.
#'
#' @param path A character string of the file path to be copied.
#' @param new_path A character string of the path where the file should be
#'   copied to.
#'
#' @return The new path (invisibly) if the file is copied. Nothing otherwise.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   # Create a test file
#'   jdtools::file_new_text("test-file_copy", open = FALSE)
#'
#'   # Copy the file with no problem
#'   jdtools::file_copy(path = "test-file_copy.txt",
#'                      new_path = "test-file_copy-2.txt")
#'   # Upon repeating, this will prompt the user whether or nott
#'   # test-file_copy-2.txt should be overwritten.
#'   jdtools::file_copy(path = "test-file_copy.txt",
#'                      new_path = "test-file_copy-2.txt")
#'
#'   # A copied file can be piped into `jdtools::file_open()`
#'   jdtools::file_copy(path = "test-file_copy.txt",
#'                      new_path = "test-file_copy-3.txt") %>%
#'     jdtools::file_open()
#'
#' }
#'
#'
file_copy <- function(path, new_path) {

  tryCatch(fs::file_copy(path = path,
                new_path = new_path,
                overwrite = overwrite_file(new_path)),
           error = function(e) invisible(),
           finally = invisible(new_path)
  )

}

#' Delete Files
#'
#' This function is a wrapper around [fs::file_delete()], with the a
#' console-based confirmation and, if `ask = TRUE`, the user
#' will be prompted to confirm the deletion.
#'
#' @param path A character vector of the file path(s) to be deleted.
#' @param ask Logical: FALSE by default. Should the user be prompted whether or
#'   not the file(s) should be deleted?
#'
#' @return The deleted file path(s) (invisibly).
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   # Create multiple files -- not opening them
#'   jdtools::file_new_text("test-file_delete", open = FALSE)
#'   jdtools::file_new_r("test-file_delete", open = FALSE)
#'   jdtools::file_new_md("test-file_delete", open = FALSE)
#'   jdtools::file_new_js("test-file_delete", open = FALSE)
#'
#'   # Just delete the text file
#'   file_delete(path = "test-file_delete.txt",
#'               ask = FALSE)
#'
#'   # Ask about whether to delete the R file
#'   file_delete(path = "test-file_delete.R",
#'               ask = TRUE)
#'
#'   # Ask about whether to delete the markdown and JavaScript files
#'   file_delete(path = c("test-file_delete.md", "test-file_delete.js"),
#'               ask = TRUE)
#' }
#'
#'
file_delete <- function(path, ask = FALSE) {
  if (ask) {
    cl_yes_no_action("Are you sure you want to delete the file{?s} {.file {path}}?",
                     yes_action = {
                       fs::file_delete(path)
                       invisible(path)
                       },
                     yes_message = "Okay, the file{?s} {.file {path}} {?has/have} been deleted.",
                     no_message = "Okay, not deleting {?/the/the} {.file {path}} file{?s}.")
  } else {
    fs::file_delete(path)
    cli::cli_alert_success("Okay, the file{?s} {.file {path}} {?has/have} been deleted.")
    invisible(path)
  }
}
