% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{pull_jdtools}
\alias{pull_jdtools}
\title{Pull the development version of jdtools from GitHub}
\usage{
pull_jdtools(...)
}
\arguments{
\item{...}{Additional arguments to pass along to \code{\link[remotes:install_github]{remotes::install_github()}}.}
}
\value{
NA; used for side effects to install the development version of jdtools from GitHub.
}
\description{
This is a simple utility function to easily install jdtools from GitHub. It
is equivalent to \code{remotes::install_github(repo = "jdtrat/jdtools")}.
}
