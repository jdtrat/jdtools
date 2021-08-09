
pkg_spec <- function(pkg) {

  pkg_option <- getOption("jdtools.opts_package_name")

  if (!is.null(pkg_option)) {
    return(pkg_option)
  } else if (missing(pkg)) {
    tryCatch(desc::desc_get("Package"), error = function(e) cli::cli_abort("Oops. It looks like you're not working in on a package right now. Please specify the package whose option(s) you would like to utilize."))
  } else {
    pkg
  }
}

color_options <- function(...) {
  out <- paste0("`", ..., "`")
  if (is_rstudio_dark()) options_bg_dark(out) else options_bg_light(out)
}
