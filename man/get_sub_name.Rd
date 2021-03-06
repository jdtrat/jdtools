% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_subsets.R
\name{get_sub_name}
\alias{get_sub_name}
\title{Get a Substituted Name as Character for Subsetting}
\usage{
get_sub_name(x)
}
\arguments{
\item{x}{The substituted column name.}
}
\value{
The character vector for use with subsetting data.
}
\description{
This generic function is primarily useful for functions that accept column names for
subsetting data. The goal is to provide methods for users to supply either an
unquoted column name or a quoted one (character string). It makes use of the
fact that calling \code{substitute()} on a character string has a class
"character", and class "name" otherwise.
}
\examples{

get_col <- function(data, column) {
  name <- substitute(column)
  data[, get_sub_name(name)]
}

get_col(mtcars, hp)
get_col(mtcars, "hp")

}
