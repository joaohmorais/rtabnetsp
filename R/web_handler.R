library(purrr)
library(httr)

#' Safe GET Operation
#'
#' Perform several attempts of an HTTP GET operation.
#' @param url GET request url.
#' @param timeout Time (seconds) to wait in each attempt.
#' @param num_attempts Maximum number of attempts to be made.
#' @keywords tabnet
#' @return Response instance containing possible error message and the http response content.
#' @examples
safe_GET <- function(url, timeout = 1, num_attempts = 5) {
  response <- NULL
  safely_get <- safely(httr::GET)
  i <- 1
  repeat {
    response <- safely_get(url, timeout(timeout))
    i <- i + 1
    if (is.null(response$error) | i > num_attempts){
      break
    }
  }
  
  return(response)
}

#' Safe POST Operation
#'
#' Perform several attempts of an HTTP POST operation.
#' @param url POST request url.
#' @param timeout Time (seconds) to wait in each attempt.
#' @param num_attempts Maximum number of attempts to be made.
#' @keywords tabnet
#' @return Response instance containing possible error message and the http response content.
safe_POST <- function(url, body, encode="raw", timeout = 1, num_attempts = 5) {
  response <- NULL
  safely_post <- safely(httr::POST)
  
  i <- 1
  repeat {
    response <- safely_post(url, body=body, encode=encode, config = timeout(timeout))
    i <- i + 1
    if (is.null(response$error) | i > num_attempts){
      break
    }
  }
  
  return(response)
}