#' Check a directory exists
#'
#' This function is a simple utility for checking whether a directory exists. It
#' is used internally with the `file_new_*` family of functions.
#'
#' @param .path The path where an action is to be executed.
#'
#' @return LOGICAL; TRUE if the directory already exists  *or* TRUE if the user
#'   indicates they want to create the directory (in which case it will be
#'   created in this function). FALSE if the directory does not exist and the
#'   user indicates they don't want to create it.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   fs::dir_create("test-folder")
#'   if (dir_exists_prompt(here("test-folder/test-file.R"))) {
#'     fs::file_create("test-folder/test-file.R")
#'   }
#' }
#'
dir_exists_prompt <- function(.path) {

  file_dir <- get_file_dir(.path = .path)

  if (!fs::dir_exists(file_dir)) {
    cl_yes_no_action("It looks like you're attempting something in a directory {.file {file_dir}} that doesn't exist yet. Would you like to create it?",
           yes_action = {
             fs::dir_create(file_dir)
             return(TRUE)
           },
           yes_message = "Created directory {.file {file_dir}}.",
           no_action = return(FALSE),
           no_message = "Please retry with an existing directory.")
  } else if (fs::dir_exists(file_dir)) {
    return(TRUE)
  }

}


#' Check a file exists and, if so, prompt to overwrite
#'
#' This function is a simple utility for checking whether a file exists and
#' should be overwritten. It is used internally with the `file_new_*` family of
#' functions.
#'
#' @param .path The path where an action is to be executed.
#'
#' @return LOGICAL; TRUE if the file does not exist and should be "overwritten"
#'   *or* TRUE if the user indicates they want to overwrite the file. FALSE if
#'   the user indicates they don't want to overwrite the file.
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   fs::dir_create("test-folder")
#'   fs::file_create("test-folder/test-file.R")
#'   if (overwrite_file("test-folder/test-file.R")) {
#'     fs::file_create("test-folder/test-file.R")
#'   }
#' }
#'
overwrite_file <- function(.path) {

  # If file does exist, ask user if they want to overwrite it.
  if (fs::file_exists(.path)) {
    cl_yes_no_action("The file {.file {.path}} already exists. Would you like to overwrite it?",
           yes_action = return(TRUE),
           no_action = return(FALSE),
           no_message = "Okay, not overwriting {.file {.path}}.")
  }

  # If file doesn't exist, we can "Overwrite" it
  if (!fs::file_exists(.path)) {
    return(TRUE)
  }

}

get_file_dir <- function(.path) {
  dir <- extract_between(full_string = .path,
                         after_string = paste0(here::here(), "/"),
                         before_string = "\\/")

  if (length(dir) == 0) {
    cli::cli_abort("Parent directory of {.file .path} not detected.")
  } else {
    return(dir)
  }

}

create_file <- function(.path, .open) {
  if (overwrite_file(.path = .path)) {
    fs::file_create(.path)
    cli::cli_alert_success("Created {.file {.path}}. Opening now!")
    if (.open) rstudioapi::navigateToFile(.path)
  }
}

# Does the path passed in already have the extension?
check_file_ext <- function(.path, .ext) {

  path <- here::here(.path)

  current_ext <- fs::path_ext(path = path)

  if (current_ext == .ext) {
    path
  } else if (current_ext == "") {
    paste0(path, ".", .ext)
  }
  else if (current_ext != .ext & current_ext != "") {
    cli::cli_abort("Oops. You want to create a {.file .{.ext}} file, but have specified a path for a file of type {.file .{current_ext}}.")
  }

}
