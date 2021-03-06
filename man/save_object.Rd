% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_save-load.R
\name{save_object}
\alias{save_object}
\alias{load_object}
\title{Save/Load R Objects as .RDS files.}
\usage{
save_object(x, path, filename_modifier = NULL, sep = "_")

load_object(x, path, filename_modifier = NULL, sep = "_", auto_assign = TRUE)
}
\arguments{
\item{x}{The R object to save.}

\item{path}{A directory within the current RStudio Project.}

\item{filename_modifier}{The saved .RDS file name takes on the R object's
name by default (when \code{filename_modifier} = NULL). Optionally, a filename
extension can be added, such as a random string or date. See examples for
more details.}

\item{sep}{Ignored by default. If \code{filename_modifier} is used, this is a
character string used to separate the object name and modifier when writing
the .RDS file.}

\item{auto_assign}{\strong{Only for \code{load_object()}} and \code{TRUE} by default. Should
the object loaded automatically be assigned to the parent environment?}
}
\value{
Either a .RDS file written to the specified location or an R object
containing the contents of the .RDS file.
}
\description{
These functions wrap \code{\link[base:readRDS]{base::saveRDS()}} and \code{\link[base:readRDS]{base::readRDS()}} and are
augmented with utility functions to make saving and loading R (.RDS) objects
easier.
}
\details{
Specifically, these functions assume the use of an RStudio Project. The main
benefit of these functions is the consistency between the name of the R
object and the .RDS file name.

Additionally, for analyses where there are lots of objects to be
saved/loaded, there are utilities to specify where these objects will be,
e.g. \code{\link{set_object_path}}.
}
\examples{

if (interactive()) {
  library(fs)
  library(here)
  library(waldo)

  # Create a directory test-folder in the current RStudio Project path
  dir_create(here("test-folder"))

  # Save `mtcars` object to test-folder `here::here()` is run within
  # save_object, so it doesn't need to be supplied here, though it can be.
  save_object(mtcars, path = "test-folder")

  # Check the objects saved. The .RDS file is automatically named with the object's name
  # `mtcars.RDS`
  dir_ls(here('test-folder'))

  # The object can be loaded easily as:
  # This automatically assigns the object loaded as mtcars.
  load_object(mtcars, path = "test-folder")

  # If you wish to assign an object yourself,
  # for example as a different name, this can be done as follows:
  carsmt <- load_object(mtcars, path = "test-folder", auto_assign = FALSE)

  # .RDS files can also have a modifier to distinguish between similar R objects.
  mtcars <- mtcars[,1:3]
  save_object(mtcars, path = "test-folder",
              filename_modifier = "cols_1-3", sep = "_")

  # It can be loaded back with the same arguments
  load_object(mtcars, path = "test-folder",
              filename_modifier = "cols_1-3", sep = "_")

  # It could also be loaded back directly by name (should be character vector)
  load_object("mtcars_cols_1-3", path = "test-folder")

  # For analyses where lots of objects will be reused, you can set the
  # global path option and not specifically include the path.
  set_object_path("test-folder")

  # This now works! No path necessary.
  save_object(iris)
  load_object(iris)

  # Custom filenames also work:
  save_object(iris, filename_modifier = "123")
  load_object(iris, filename_modifier = "123")

  # You can still specify an alternative path even with that global option set.
  dir_create("another-test-folder")

  # Modify iris for example
  iris$message <- "I'm in a new folder"

  # Save modified iris to the new path
  save_object(iris, path = "another-test-folder")
  load_object(iris, path = "another-test-folder")

  # Let's compare these
  orig <- load_object(iris, auto_assign = FALSE)
  new <- load_object(iris, path = "another-test-folder", auto_assign = FALSE)

  # Should not be identical!
  compare(orig, new)

  # Check the path
  get_object_path()

  # Unset the path
  unset_object_path()

}
}
\seealso{
set_object_path, get_object_oath, unset_object_path
}
