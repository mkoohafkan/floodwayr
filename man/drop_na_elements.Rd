% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractors.r
\name{drop_na_elements}
\alias{drop_na_elements}
\title{Drop NA Elements}
\usage{
drop_na_elements(shape, field)
}
\arguments{
\item{shape}{A \code{SpatVector} object or file path to a vector layer.}

\item{field}{The field name to search for NA values.}
}
\value{
The \code{SpatVector} object.
}
\description{
Drop elements with NA values for the specified field.
}
\keyword{internal}
