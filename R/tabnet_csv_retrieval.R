library(httr)
library(xml2)

#' Retrieve TABNET data in .csv format
#'
#' Retrieve the indicator data from its url and parameters.
#' @param post_url Indicator POST url.
#' @param body Request body containing required information.
#' @param colname The specified region name, like city.
#' @param skip Numeric interval from which the .csv will start to be read.
#' @param stopDelta Numeric interval from which the .csv reading will stop. 
#' @keywords tabnet
#' @export
#' @return Data frame containing data from the indicator specified by the url.
#' @examples
tabnet_csv_retrieval <- function(post_url, body, colname = "Região", skip = 3, stopDelta = 6) {
  base_url <- "http://tabnet.saude.sp.gov.br"
  response <- POST(post_url,body = body, encode="raw")
  response_text <- content(response, as="text", encoding = "latin1")
  start <- regexpr("/csv", response_text)[1]
  end <- regexpr("csv>", response_text)[1] + 2
  path <- substr(response_text, start, end)
  stopPoint <- min(which(grepl("Fonte", readLines(paste0(base_url, path), encoding = "latin1")) == TRUE))
  data <- read.csv(paste0(base_url, path), skip = skip, sep=";", nrow = stopPoint - stopDelta, encoding = "latin1")
  colnames(data) <- c(colname, gsub("X", "", colnames(data)[-1]))
  
  
  #formatting
  
  #numbers
  for (i in c(2:ncol(data))) {
    data[,i] <- as.numeric(gsub(",", ".", gsub("\\.", "", data[,i])))
  }
  
  #ids
  data$id <- data$Município
  data$id <- as.numeric(gsub("([0-9]+).*$", "\\1", data$Município))
  data$Município <- gsub('[0-9]+ ', '', data$Município)
  
  #remove total
  data <- data[,c(ncol(data), 1:ncol(data)-1)]
  
  return(data)
}