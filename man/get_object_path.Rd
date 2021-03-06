% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_save-load.R
\name{get_object_path}
\alias{get_object_path}
\title{Get global path option for saving/loading objects}
\usage{
get_object_path()
}
\value{
NA; used for side effects to get the global option
\code{jdtools.rds.path} if applicable.
}
\description{
Get global path option for saving/loading objects
}
\examples{

if (interactive()) {

fs::dir_create(here::here("test-folder"))
# Will state option is unset
jdtools::get_object_path()

jdtools::set_object_path("test-folder")
# Will return path
jdtools::get_object_path()

}

}
\seealso{
Other utils-save-load: 
\code{\link{set_object_path}()},
\code{\link{unset_object_path}()}
}
\concept{utils-save-load}
