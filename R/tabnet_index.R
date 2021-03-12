library(stringi)
library(rvest)
library(xml2)
library(httr)

#' Build TABNET indicators matrix
#'
#' Collects indicators links and names from html scraped object.
#' @param index HTML scraped object.
#' @param link_start List containing start indexes of links, in each item in index object.
#' @param link_end List containing end indexes of links, in each item in index object.
#' @param name_end List containing end indexes of indicator names, in each item in index object.
#' @param n Number of indicators present in each item of index object.
#' @keywords tabnet
#' @return Data frame with indicators names and links found in objects provided.
#' @examples
build_tabnet_matrix <- function(index, link_start, link_end, name_end, n) {
  links <- NULL
  names <- NULL
  for (i in c(1:length(index))) {
    s <- unlist(link_start[i])
    e <- unlist(link_end[i])
    ne <- unlist(name_end[i])
    for (j in c(1:n[i])) {
      links <- c(links, substr(index[i], s[j] + 6, e[j] + 3))
      names <- c(names, substr(index[i], e[j] + 6, ne[j] - 1))
    }
  }

  validLinks <- !stri_isempty(links) & !stri_detect_fixed(links, "datasus")

  return (data.frame(names = names[validLinks], links = links[validLinks]))
}


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

  # link starts after "href="
  s <- gregexpr("href=", index, fixed=TRUE)
  n.s <- lengths(s) # number of link starts in each item
  # link ends after ".def"
  e <- gregexpr(".def", index, fixed=TRUE)
  n.e <- lengths(e) # number of link ends in each item
  # name ends before "</a>"
  ne <- gregexpr("</a>", index, fixed=TRUE)
  n.ne <- lengths(ne) # number of name ends in each item

  n <- pmin(n.s, n.e, n.ne) # minimum of each item

  matriz <- build_tabnet_matrix(index, s, e, ne, n)
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
