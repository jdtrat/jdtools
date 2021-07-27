

get_file_dir <- function(.path) {
  extract_between(full_string = .path,
                  after_string = paste0(here::here(), "/"),
                  before_string = "\\/")
}


create_file <- function(.path, .open) {
  if (fs::file_exists(.path)) {
    yes_no("The file {.file {.path}} already exists. Would you like to overwrite it?",
           yes_action = {
             fs::file_create(here::here(.path))
             if (.open) rstudioapi::navigateToFile(.path)
           },
           no_action = return(invisible()),
           no_message = "Okay, not overwriting {.file {.path}}.")
  }
  fs::file_create(.path)
  cli::cli_alert_success("Created {.file {.path}}. Opening now!")
  if (.open) rstudioapi::navigateToFile(.path)
}

# Function factory version of file_new
file_new_ff <- function(ext) {

  function(path, open = interactive()) {
    path <- here::here(paste0(path, ".", ext))

    file_dir <- get_file_dir(.path = path)

    if (!fs::dir_exists(file_dir)) {
      yes_no("It looks like you're trying to create a file in a directory {.file {file_dir}} that doesn't exist yet. Would you like to create it?",
             yes_action = {
               fs::dir_create(file_dir)
               create_file(path, open)
             },
             yes_message = "Directory {.file {file_dir}} created.",
             no_message = "Please try creating a file in an existing directory.")

    } else if (fs::dir_exists(file_dir)) {
      create_file(path, open)
    }
  }

}

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

  path <- here::here(paste0(path, ".", ext))

  file_dir <- get_file_dir(.path = path)

  if (!fs::dir_exists(file_dir)) {
    yes_no("It looks like you're trying to create a file in a directory {.file {file_dir}} that doesn't exist yet. Would you like to create it?",
           yes_action = {
             fs::dir_create(file_dir)
             create_file(path, open)
           },
           yes_message = "Directory {.file {file_dir}} created.",
           no_message = "Please try creating a file in an existing directory.")

  } else if (fs::dir_exists(file_dir)) {
    create_file(path, open)
  }
}

#' @rdname file_new
#' @export
file_new_r <- file_new_ff("R")
#'
#' #' @rdname file_new
#' #' @export
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
