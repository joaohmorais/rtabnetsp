library(httr)
library(rvest)
library(xml2)
library(tidyr)
library(purrr)

#' Retrieve TABNET data in .csv format
#'
#' Retrieve the indicator data from its url and parameters.
#' @param post_url Indicator POST url.
#' @param body Request body containing required information.
#' @param colname The specified region name, like city.
#' @param skip Numeric interval from which the .csv will start to be read.
#' @param stopDelta Numeric interval from which the .csv reading will stop. 
#' @param timeout Maximum time interval POST request should take.
#' @keywords tabnet
#' @export
#' @return Data frame containing data from the indicator specified by the url.
#' @examples
tabnet_csv_retrieval <- function(post_url, body, colname = "Região", ind_name = "Valor", skip = 3, stopDelta = 7, timeout = 4) {
  base_url <- "http://tabnet.saude.sp.gov.br"
  data <- NULL
  response <- NULL
  attempt <- 1
  response <- safe_POST(post_url, body, timeout = timeout)
  
  if (!is.null(response$result) && is.null(response$error)) {
    response_text <- content(response$result, as="text", encoding = "latin1")
    start <- regexpr("/csv", response_text)[1]
    end <- regexpr("csv>", response_text)[1] + 2
    path <- substr(response_text, start, end)
    #stopPoint <- min(which(grepl("Fonte", readLines(paste0(base_url, path), encoding = "latin1")) == TRUE))
    download.file(paste0(base_url, path), "indicator_data.csv")
    stopPoint <- min(which(grepl("Fonte", readLines("indicator_data.csv", encoding = "latin1")) == TRUE))
    data <- read.csv("indicator_data.csv", skip = skip, sep=";", nrow = stopPoint - stopDelta, encoding = "latin1")
    colnames(data) <- c(colnames(data)[1], gsub("X", "", colnames(data)[-1]))
    #formatting
    
    #names
    data[,1] <- as.character(data[,1])
    
    #numbers
    for (i in c(2:ncol(data))) {
      data[,i] <- as.numeric(gsub(",", ".", gsub("\\.", "", data[,i])))
    }
    
    #ids
    
    div_index <- unlist(gregexpr("[0-9] ", data[,1]))
    data$id <- as.numeric(substr(data[,1], 1, div_index))
    data[,1] <- substr(data[,1], div_index + 2, nchar(data[,1]))
    data <- data[,c(ncol(data), 1:ncol(data)-1)]
    
    #remove total
    if (colnames(data)[length(colnames(data))] == "Total") {
      data <- data[,c(1:ncol(data)-1)]
    }
    
    
    data <- data %>% gather(Ano, Valor, 3:ncol(data))
    colnames(data)[colnames(data) == "Valor"] <- ind_name
    
    data$Ano <- as.factor(data$Ano)
  
  } else {
    stop("Could not connect to TabNet: Connection timed out several times.")
  }
  return(data)
}