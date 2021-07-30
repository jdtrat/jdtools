#' Ask Yes/No Questions via the console
#'
#' This function allows you to interact with a user by posing yes/no questions.
#' It's inspired by [usethis::ui_yeah()], and returns `TRUE` if the user
#' responds affirmatively and `FALSE` if they respond negatively.
#'
#' In general, I would recommend using \code{\link{cl_yes_no_action}} as it
#' accepts code to execute depending on the user's response, but this function is
#' perhaps more adaptable.
#'
#' @param prompt A character string with the yes/no question to be asked. Passed
#'   into [cli::cli_text()] and can use its theming syntax.
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
#' @return Logical: `TRUE` if the user answers affirmatively and `FALSE` otherwise.
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
#'     if (cl_yes_no_lgl(prompt = "The directory {.file {path}} does not exist.
#'     Would you like to create it?")) {
#'       fs::dir_create(path)
#'     }
#'   }
#'
#'   # Test the function
#'   yn_create_dir("new-folder")
#'
#'   # Ask a simple yes/no question that prints a conditional response
#'   if (cl_yes_no_lgl(prompt = "Do you love sushi?")) print("Yay!")
#'
#'   # Create a function that prints different answers depending on user response
#'   ask_about_tucker <- function() {
#'
#'     # Simple yes/no with content-related yes/no options
#'     tucker_response <- cl_yes_no_lgl(prompt = "Is your favorite cat Tucker?",
#'                                      yes_opts = "Duh, he's the cutest",
#'                                      no_opts = c("I'm a dog person",
#'                                      "I'm allergic to cats"))
#'
#'     if (tucker_response) print("Great answer!") else print("...")
#'
#'   }
#'
#' }
#'
#'
cl_yes_no_lgl <- function(prompt,
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

  out != 0L && opts[[out]] %in% ys

}

#' Ask Text Questions via the Console
#'
#' This function allows you to interact with a user by posing questions that
#' accept text input. It's inspired by [usethis::ui_yeah()] and
#' [shiny::textInput()]. It wraps [base::readline()] with [cli::cli_text()]
#' allowing you to format questions using the `cli` package.
#'
#' In general, I would recommend using \code{\link{cl_text_action}} as it
#' accepts code to execute depending on the user's input, but this function is
#' perhaps more adaptable.
#'
#' @param prompt A character string with the question to be asked. Passed into
#'   [cli::cli_text()] and can use its theming syntax.
#' @param .envir Used to ensure that [cli::cli_text()] evaluates the prompt,
#'   yes_message, and no_message in the appropriate environment. Expert use
#'   only.
#'
#' @return A character string containing the user's response.
#' @family command-line-tools
#' @export
#'
#' @examples
#'
#' favorite_cat_question <- function() {
#'   answer <- cl_text_input("Name your {.strong favorite} cat.")
#'   if (tolower(answer) != "tucker") {
#'     cli::cli_alert_danger("\U1F63E Come meet my cat, Tucker,
#'     and I think you'll change your mind!")
#'   } else {
#'     cli::cli_alert_success("Correct answer! \U1F63B")
#'   }
#' }
#'
#' favorite_dog_question <- function() {
#'   answer <- cl_text_input("Name your {.strong favorite} dog.")
#'   if (tolower(answer) != "ella") {
#'     cli::cli_alert_danger("\U1F44E Come meet my dog, Ella,
#'     and I think you'll change your mind!")
#'   } else {
#'     cli::cli_alert_success("Correct answer!\U1F436")
#'   }
#' }
#'
#' if (interactive()) favorite_cat_question()
#'
#' if (interactive()) favorite_dog_question()
#'
cl_text_input <- function(prompt, .envir = parent.frame(1)) {
  if (!interactive()) cli::cli_abort("Non-interactive session.")
  readline(prompt = cli::cli_text(prompt, .envir = .envir))
}

