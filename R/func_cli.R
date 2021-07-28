#' Ask Yes/No Questions via the console
#'
#' This function allows you to interact with a user by posing yes/no questions.
#' It's inspired by [usethis::ui_yeah()], allowing for you to specify code that
#' will be executed upon a user responding "yes" or "no."
#'
#' @param prompt A character string with the yes/no question to be asked.
#' @param yes_action Code to execute upon a "yes" answer.
#' @param yes_message (Optional) message to display upon a "yes" answer. Passed
#'   into [cli::cli_alert_success()] and can use its theming syntax.
#' @param no_action Code to execute upon a "no" answer. Default is
#'   `invisible()`, i.e. do nothing.
#' @param no_message (Optional) message to display upon a "yes" answer. Passed
#'   into [cli::cli_alert_danger()] and can use its theming syntax.
#' @param yes_opts A character vector of "yes" strings, randomly sampled to
#'   display to the user.
#' @param no_opts A character vector of "no" strings, randomly sampled to
#'   display to the user.
#' @param n_yes An integer defining how many "yes" strings to include when
#'   asking the user a question. Default is 1.
#' @param n_no An integer defining how many "no" strings to include when asking
#'   the user a question. Default is 2.
#' @param shuffle Logical: TRUE by default. Should the order of the yes/no
#'   options be shuffled before being presented to the user?
#' @param .envir Used to ensure that [cli::cli_text()] evaluates the prompt,
#'   yes_message, and no_message in the appropriate environment. Expert use
#'   only.
#'
#' @return NA; used to execute specified code depending on a user's response.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   # define a function to create a directory with approval.
#'   yn_create_dir <- function(path) {
#'     path <- here::here(path)
#'     yes_no(prompt = "The directory {.file {path}} does not exist. Would you like to create it?",
#'            yes_action = fs::dir_create(path),
#'            yes_message = "Okay, creating {.file {path}}.",
#'            no_message ="Okay, not creating {.file {path}}.")
#'   }
#'
#'   # Test the function
#'   yn_create_dir("new-folder")
#'
#'   # Ask a simple yes/no question that prints a conditional response
#'   yes_no(prompt = "Do you love sushi?",
#'          yes_action = print("I'm not surprised, it's great!"),
#'          no_action = print("Hmmm...have you tried it?"))
#'
#'   # Simple yes/no with content-related yes/no options
#'   yes_no(prompt = "Is your favorite cat Tucker?",
#'          yes_action = print("Correct answer!"),
#'          no_action = print("Wrong answer..."),
#'          yes_opts = "Duh, he's the cutest",
#'          no_opts = c("I'm a dog person", "I'm allergic to cats"))
#'
#'   # Add some color (indicating the correct answer in this case)
#'   yes_no(prompt = "Is your favorite cat Tucker?",
#'          yes_action = print("Correct answer!"),
#'          no_action = print("Wrong answer..."),
#'          yes_opts = crayon::green("Duh, he's the cutest"),
#'          no_opts = c("I'm a dog person", "I'm allergic to cats"))
#'
#'
#'   # Conduct multiple actions like a normal R script upon a "yes" (no would work similarly)
#'   path <- here::here("another-test-folder")
#'   yes_no(prompt = "The directory {.file {path}} does not exist. Would you like to create it?",
#'          yes_action = {
#'            # create path
#'            fs::dir_create(path)
#'            # indicate path was created
#'            cli::cli_alert_success("Created directory at {.file {path}}")
#'            # assign saved_path as path
#'            saved_path <- path
#'            # print a random letter just because we can!
#'            print(sample(letters,1))
#'          },
#'          no_message ="The directory {.file {path}} was not created."
#'          )
#' }
#'
yes_no <- function(prompt,
                   yes_action, yes_message,
                   no_action = invisible(), no_message,
                   yes_opts = c("Yes", "Duh!", "Absolutely", "Please", "Obvi, yeah."),
                   no_opts = c("No", "Nah", "Not now", "Not today", "No, thanks."),
                   n_yes = 1,
                   n_no = 2,
                   shuffle = TRUE,
                   .envir = parent.frame(1)) {

  if (!interactive()) cli::cli_abort("Non-interactive session.")


  ys <- sample(yes_opts, n_yes)
  ns <- sample(no_opts, n_no)

  opts <- c(ys, ns)

  if (shuffle) {
    opts <- sample(opts)
  }

  cli::cli_text(prompt, .envir = .envir)

  out <- utils::menu(choices = opts)

  if (out != 0L && opts[[out]] %in% ys) {
    if (!missing(yes_message)) cli::cli_alert_success(yes_message, .envir = .envir)
    yes_action
  } else {
    if (!missing(no_message)) cli::cli_alert_danger(no_message, .envir = .envir)
    no_action
  }

}


