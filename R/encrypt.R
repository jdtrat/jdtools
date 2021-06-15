secret_can_decrypt <- utils::getFromNamespace("secret_can_decrypt", "gargle")
secret_read <- utils::getFromNamespace("secret_read", "gargle")
secret_pw_gen <- utils::getFromNamespace("secret_pw_gen", "gargle")
secret_pw_get <- utils::getFromNamespace("secret_pw_get", "gargle")
secret_nonce <- utils::getFromNamespace("secret_nonce", "gargle")
secret_path <- utils::getFromNamespace("secret_path", "gargle")

stop_bad_class <- utils::getFromNamespace("stop_bad_class", "gargle")
stop_secret <-  utils::getFromNamespace("stop_secret", "gargle")


# Modified slightly from the {boxr} package
# https://github.com/r-box/boxr/blob/28ccd2610922b53e0275d4d128f29781b92970e0/R/boxr_auth.R#L580
auth_message <- function(pw_info) {

  if (requireNamespace("usethis", quietly = TRUE)) {
    usethis::ui_todo("You may wish to add to your {usethis::ui_code('.Renviron')} file:")
    usethis::ui_code_block(pw_info)
    usethis::ui_todo(c("To edit your {usethis::ui_code('.Renviron')} file:",
                       "  - {usethis::ui_code('usethis::edit_r_environ()')}",
                       "  - check that {usethis::ui_code('.Renviron')} ends with a newline"))
  }
  else {
    message(glue::glue_collapse(c("\nYou may wish to add the following to your `.Renviron` file:",
                                  "  - check that `.Renviron` ends with a newline",
                                  "", pw_info, ""), sep = "\n"))
  }

}

#' Create environment password
#'
#' Following the conventions of gargle's `secret_*()` family of functions, this
#' function will generate a random password that can be added to your R
#' enviornment. This will allow you to pass the system variable to external
#' services such as GitHub Actions, which can be useful for encrypting and
#' decrypting authentication tokens.
#'
#' @param service Identifier of the service whose token will be encrypted.
#'
#' @return NA; used for side effects to create environmental variable.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#' create_env_pw("testing-jdtools")
#' }
#'
#'
create_env_pw <- function(service) {
  pw <- paste0(toupper(service), "_PASSWORD=", secret_pw_gen())
  auth_message(pw_info = pw)
}

#' Encrypt a token file
#'
#' Following
#' \url{https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html#encrypt-the-secret-file},
#' this function writes an encrypted version of the input file to a specified
#' directory, such as a Shiny Web App's "www" subfolder. The supplied
#' destination path is suffixed with a "secret" folder.
#'
#' @inheritParams create_env_pw
#' @param input The token file to encrypt, typically a ".json" file.
#' @param destination The output directory you would like to store this file,
#'   e.g. in a Shiny Web App's "www" subdirectory.
#'
#' @return NA; used for side effects to create an encrypted token file.
#' @export
#'
encrypt_token <- function(service, input, destination) {

  if (is.character(input)) {
    input <- readBin(input, "raw", file.size(input))
  }
  else if (!is.raw(input)) {
    stop_bad_class(input, c("character", "raw"))
  }
  destdir <- fs::path(destination, "secret")
  fs::dir_create(destdir)
  destpath <- fs::path(destdir, service)
  enc <- sodium::data_encrypt(msg = input, key = secret_pw_get(service),
                              nonce = secret_nonce())
  attr(enc, "nonce") <- NULL
  writeBin(enc, destpath)
  invisible(destpath)

}

#' Decrypt an encrypted token file
#'
#' @inheritParams create_env_pw
#' @param path The location of the encrypted token file.
#' @param complete Logical: FALSE by default and will return decrypted raw
#'   bytes. TRUE and will decrypt to character string via [base::rawToChar()].
#'
#' @return Either the raw bytes of the decrypted file or a character string to
#'   pass into authentication functions such as [boxr::box_auth()].
#' @export
#'
decrypt_token <- function(service, path, complete = FALSE) {

  if (!secret_can_decrypt(service)) {
    stop_secret(message = "Decryption not available", package = service)
  }
  raw <- readBin(path, "raw", file.size(path))
  output <- sodium::data_decrypt(bin = raw, key = secret_pw_get(service),
                                 nonce = secret_nonce())

  if (complete) {
    output <- rawToChar(output)
  }

  return(output)

}
