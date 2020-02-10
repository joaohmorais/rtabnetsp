library(stringi)
library(rvest)
library(xml2)
library(httr)
#' Indicators Names and URLs Matrix
#'
#' Retrieve a matrix from TABNET of available indicators and their web links.
#' @param url TABNET Url. 
#' @keywords tabnet
#' @export
#' @return If provided a valid link, a data frame with names and links of each indicator will be returned.
#' @examples

tabnet_index <- function(url = "http://portal.saude.sp.gov.br/links/matriz") {
  html_obj <- read_html(url)
  index <- html_obj %>% html_nodes(".publish") %>% html_children()
  index <- as.character(index)
  index_links <- stri_detect_fixed(index, "href")
  
  index <- index[index_links]
  
  #s and e: start and end indexes
  
  s1 <- unlist(gregexpr("href=", index, fixed=TRUE))
  e1 <- unlist(gregexpr(".def", index, fixed=TRUE))
  links <- substr(index, s1 + 6, unlist(e1) + 3)
  
  nomes_raw <- substr(index, e1 + 6, nchar(index))
  
  s2 <- unlist(gregexpr("<u>", nomes_raw))
  s2[s2 == -1] <- 0
  s2[s2 == 1] <- 4
  e2u <- unlist(gregexpr("</u>", nomes_raw))
  e2a <- unlist(gregexpr("</a>", nomes_raw))
  e2 <- e2a
  e2[e2u>0] <- e2u[e2u>0]
  nomes <- substr(nomes_raw, s2, e2-1)
  validLinks <- !stri_isempty(links) & !stri_detect_fixed(links, "datasus")
  
  matriz <- data.frame(nomes[validLinks], links[validLinks])
  colnames(matriz) <- c("Nomes", "Links")
  matriz$Nomes <- as.character(matriz$Nomes)
  matriz$Links <- as.character(matriz$Links)
  return (matriz)
}
#' Indicator List
#'
#' Retrieve a vector of available indicators from link provided.
#' @param url TABNET Url. 
#' @keywords tabnet
#' @export
#' @return If provided a valid link, a vector of available TABNET indicators.
#' @examples
indicator_list <- function(url = "http://portal.saude.sp.gov.br/links/matriz") {
  list <- "No indicators found from link provided. Try a different link or check your connection."
  matrix <- tabnet_index(url)
  if (length(matrix$Links > 0)) {
    list <- paste0(c(1:length(matrix$Links)), " - ", matrix$Nomes)
  }
  return (list)
}


#' Search Indicators' Names from a Keyword
#'
#' Retrieve a subset of available indicators from list that match the specified keyword.
#' @param keyword Keyword to be searched.
#' @param url TABNET Url. 
#' @keywords tabnet
#' @export
#' @return Subset of vector of indicator names that match specified keyword.
#' @examples
#' \dontrun{indicator_search("sífilis")}
indicator_search <- function(keywords, url = "http://portal.saude.sp.gov.br/links/matriz") {
  indicators <- indicator_list(url)
  return (indicators[stri_detect_fixed(indicators, keywords, opts_fixed = stri_opts_fixed(case_insensitive = TRUE))])
  
}