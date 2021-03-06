% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/web_handler.R
\name{safe_GET}
\alias{safe_GET}
\title{Safe GET Operation}
\usage{
safe_GET(url, timeout = 1, num_attempts = 5)
}
\arguments{
\item{url}{GET request url.}

\item{timeout}{Time (seconds) to wait in each attempt.}

\item{num_attempts}{Maximum number of attempts to be made.}
}
\value{
Response instance containing possible error message and the http response content.
}
\description{
Perform several attempts of an HTTP GET operation.
}
\keyword{tabnet}
