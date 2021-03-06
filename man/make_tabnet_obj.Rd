% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabnet_obj.R
\name{make_tabnet_obj}
\alias{make_tabnet_obj}
\title{Create a TABNET Object}
\usage{
make_tabnet_obj(indicator_url, timeout = 1)
}
\arguments{
\item{indicator_url}{Base url of the TABNET indicator.}
}
\value{
An object containing information about the indicator url, its parameters, available years and files. This object is used to perform requests for data.
}
\description{
Create an object containing a certain indicator's informations.
}
\keyword{tabnet}
