#' Ask Yes/No Questions via the console
#'
#' This function allows you to interact with a user by posing yes/no questions.
#' It's inspired by [usethis::ui_yeah()], allowing for you to specify code that
#' will be executed upon a user responding "yes" or "no."
#'
#' To ask yes/no questions and return a logical value, please see
#' \code{\link{cl_yes_no_lgl}}.
#'
#' @param prompt A character string with the yes/no question to be asked. Passed
#'   into [cli::cli_text()] and can use its theming syntax.
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
#' @family command-line-tools
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   # define a function to create a directory with approval.
#'   yn_create_dir <- function(path) {
#'     path <- here::here(path)
#'     cl_yes_no_action(prompt = "The directory {.file {path}} does not exist.
#'     Would you like to create it?",
#'            yes_action = fs::dir_create(path),
#'            yes_message = "Okay, creating {.file {path}}.",
#'            no_message ="Okay, not creating {.file {path}}.")
#'   }
#'
#'   # Test the function
#'   yn_create_dir("new-folder")
#'
#'   # Ask a simple yes/no question that prints a conditional response
#'   cl_yes_no_action(prompt = "Do you love sushi?",
#'          yes_action = print("I'm not surprised, it's great!"),
#'          no_action = print("Hmmm...have you tried it?"))
#'
#'   # Simple yes/no with content-related yes/no options
#'   cl_yes_no_action(prompt = "Is your favorite cat Tucker?",
#'          yes_action = print("Correct answer!"),
#'          no_action = print("Wrong answer..."),
#'          yes_opts = "Duh, he's the cutest",
#'          no_opts = c("I'm a dog person", "I'm allergic to cats"))
#'
#'   # Add some color (indicating the correct answer in this case)
#'   cl_yes_no_action(prompt = "Is your favorite cat Tucker?",
#'          yes_action = print("Correct answer!"),
#'          no_action = print("Wrong answer..."),
#'          yes_opts = crayon::green("Duh, he's the cutest"),
#'          no_opts = c("I'm a dog person", "I'm allergic to cats"))
#'
#'
#'   # Conduct multiple actions like a normal R script upon a "yes" (no would work similarly)
#'   path <- here::here("another-test-folder")
#'   cl_yes_no_action(prompt = "The directory {.file {path}} does not exist.
#'   Would you like to create it?",
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
cl_yes_no_action <- function(prompt,
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


#' Ask Text Questions via the Console
#'
#' This function allows you to interact with a user by posing questions that
#' accept text input, allowing for you to specify code that will be executed if
#' their response satisfies a condition you define via `correct_logic` argument.
#' This could either be inline of code or a previously defined function that
#' returns `TRUE` or `FALSE`. See examples for a demonstration of both.
#'
#' To simply save the value of a user's input in response to a prompt, please
#' see \code{\link{cl_text_input}}.
#'
#' It's inspired by [usethis::ui_yeah()] and [shiny::textInput()]. It wraps
#' [base::readline()] with [cli::cli_text()] allowing you to format questions
#' using the `cli` package.
#'
#' @param prompt A character string with the yes/no question to be asked. Passed
#'   into [cli::cli_text()] and can use its theming syntax.
#' @param correct_logic Code that evaluates the user's text input and defines a
#'   correct input by returning a `TRUE` and an incorrect input by returning
#'   `FALSE`. The user's input is stored in the variable `answer`.
#' @param correct_action Code to execute upon a "yes" answer.
#' @param correct_message (Optional) message to display upon a "yes" answer.
#'   Passed into [cli::cli_alert_success()] and can use its theming syntax.
#' @param incorrect_action Code to execute upon a "no" answer. Default is
#'   `invisible()`, i.e. do nothing.
#' @param incorrect_message (Optional) message to display upon a "yes" answer.
#'   Passed into [cli::cli_alert_danger()] and can use its theming syntax.
#' @param .envir Used to ensure that [cli::cli_text()] evaluates the prompt,
#'   yes_message, and no_message in the appropriate environment. Expert use
#'   only.
#'
#' @return NA; used to execute specified code depending on a user's response.
#' @family command-line-tools
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   ## Define correct_logic inline
#'
#'   # Ask who are the best pets. If they say Tucker or Ella in their response,
#'   print
#'   # "Correct answer!.
#'   cl_text_action("Who are the best pets?",
#'                  correct_logic = {
#'                    if (grepl("tucker|ella", tolower(answer))) {
#'                      TRUE
#'                    } else {
#'                      FALSE
#'                    }
#'                  },
#'                  # Nothing to do, just want to print a message!
#'                  correct_action = invisible(),
#'                  correct_message = "\U1F63B Correct answer! \U1F436",
#'                  incorrect_message = "\U1F44E \U1F63E Come meet the best dog, Ella,
#'               or the best cat, Tucker, and I think you'll change your mind!")
#'
#'
#'   ## Use a function to define correct logic
#'
#'   has_tucker_ella <- function(x) {
#'     if (grepl("tucker|ella", tolower(x))) {
#'       TRUE
#'     } else {
#'       FALSE
#'     }
#'   }
#'
#'
#'   # Ask who are the best pets. If they say Tucker or Ella in their response,
#'   # print "Correct answer!.
#'   cl_text_action("Who are the best pets?",
#'                  # Pass in the user's input with the variable `answer`.
#'                  correct_logic = has_tucker_ella(answer),
#'                  # Nothing to do, just want to print a message!
#'                  correct_action = invisible(),
#'                  correct_message = "\U1F63B Correct answer! \U1F436",
#'                  incorrect_message = "\U1F44E \U1F63E Come meet the best dog, Ella,
#'                or the best cat, Tucker, and I think you'll change your mind!")
#' }
#'
cl_text_action <- function(prompt,
                           correct_logic,
                           correct_action, correct_message,
                           incorrect_action = invisible(), incorrect_message,
                           .envir = parent.frame(1)) {

  if (!interactive()) cli::cli_abort("Non-interactive session.")

  is_correct <- substitute(correct_logic)

  answer <- readline(prompt = cli::cli_text(prompt, .envir = .envir))

  if (eval(is_correct)) {
    if (!missing(correct_message)) cli::cli_alert_success(correct_message, .envir = .envir)
    correct_action
  } else {
    if (!missing(incorrect_message)) cli::cli_alert_danger(incorrect_message, .envir = .envir)
    incorrect_action
  }

}
