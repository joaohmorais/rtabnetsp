library(xml2)
library(rvest)
library(stringi)

#' Get POST Request Body
#'
#' Form the raw post request to be uploaded to TABNET.
#' @param lines The lines to be used.
#' @param indicators The requested indicators.
#' @param years The period of time requested.
#' @keywords tabnet
#' @export
#' @return A string properly formatted and encoded to be sent as body of the POST request to TABNET.
#' @examples
getPostRequestBody <- function(lines, indicators, years) {
  
  body_complement <- switch (lines,
     "Município" = paste0("&pesqmes1=Digite+o+texto+e+ache+f%E1cil&",
                          "SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&",
                          "SDRS_de_Resid=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SRRAS-Resid.=",
                          "TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&",
                          "zeradas=exibirlz&formato=table&mostre=Mostra"),
     "DRS" = paste0("&pesqmes1=Digite+o+texto+e+ache+f%E1cil&",
                    "SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&",
                    "SDRS=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SRRAS=",
                    "TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&",
                    "zeradas=exibirlz&formato=table&mostre=Mostra"),
     "RRAS" = paste0("&pesqmes1=Digite+o+texto+e+ache+f%E1cil&",
                     "SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&",
                     "SDRS=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SRRAS=TODAS_AS_CATEGORIAS__&",
                     "pesqmes4=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&",
                     "zeradas=exibirlz&formato=table&mostre=Mostra"),
     "Região_de_Saúde" = paste0("&pesqmes1=Digite+o+texto+e+ache+f%E1cil&",
                                "SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&",
                                "SDRS=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SRRAS=",
                                "TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde=",
                                "TODAS_AS_CATEGORIAS__&zeradas=exibirlz&formato=table&mostre=Mostra")
  )
  
  body <- paste0("Linha=", W1252.encode(lines), "&Coluna=Ano",
                 "&Incremento=", W1252.encode(indicators), 
                 tabnet_concatFormat(W1252.encode(years)), 
                 body_complement
  )
  return(body)
}

#' Obtain POST url
#'
#' Obtain the url to which the POST request should be made, from the base indicator url.
#' @param indicator_url The base url of the TABNET indicator. 
#' @keywords tabnet
#' @export
#' @return A string representing the POST url of the indicator.
#' @examples
getPostURL <- function(indicator_url) {
  return (gsub("deftohtm.exe?tabnet", "tabcgi.exe?tabnet", indicator_url, fixed = TRUE))
}

#' Create a TABNET Object
#'
#' Create an object containing a certain indicator's informations.
#' @param indicator_url Base url of the TABNET indicator. 
#' @keywords tabnet
#' @export
#' @return An object containing information about the indicator url, its parameters, available years and files. This object is used to perform requests for data.
#' @examples
make_tabnet_obj <- function(indicator_url, timeout = 1) {
  obj <- NULL
  response <- safe_GET(indicator_url, timeout = timeout)
  if(!is.null(response$result) && is.null(response$error)) {
    html_obj <- read_html(content(response$result, as="text", encoding = "latin1"))
    obj$url <- indicator_url
    obj$POST_url <- getPostURL(indicator_url) 
    
    obj$Nome <- (html_obj %>%
      html_nodes(".Nivel0") %>%
      html_text())[1]
    
    obj$NomesLinhas <- strsplit(
      tabnet_breakline_fix(
        html_obj %>%
          html_nodes("#L") %>%
          html_text()
      ),
      "    "
    )[[1]]
    
    obj$Linhas <- tabnet_indicator_catch(html_obj %>%
                                           html_nodes("#L") %>%
                                           html_children()
    )
    
    obj$Info <- stri_remove_empty(
      strsplit(html_obj %>% html_nodes(".rodape_htm") %>% html_text(), "\r\n ")[[1]]
    )
    
    obj$NomesIndicadores <- strsplit(
      tabnet_breakline_fix(
        html_obj %>%
          html_nodes("#I") %>%
          html_text()
      ), "    "
    )[[1]]
    
    
    obj$Indicadores <- tabnet_indicator_catch(
      html_obj %>% 
        html_nodes("#I") %>%
        html_children()
    )
    
    obj$Anos <- stri_trim(
      strsplit(
        tabnet_breakline_fix(
          html_obj %>%
            html_nodes("#A") %>%
            html_text()
        ), "    "
      )[[1]])
    
    obj$Arquivos <- tabnet_indicator_catch(
      html_obj %>%
        html_nodes("#A") %>%
        html_children()
    )
  } else {
    stop("Could not connect to TabNet: Connection timed out several times.")
  }
  
  return(obj)
}

#' View Indicator Information
#'
#' View available indicator regions, years and subindicators.
#' @param indicator_index Index of desired indicator, according to Indicator List.
#' @param url URL for indicator retrieval, defaults to SP TABNET.
#' @param timeout Timeout period, in seconds.
#' @keywords tabnet
#' @export
#' @return An object containing information about the indicator, its parameters, available years, regions and subindicators.
#' @examples
#' \dontrun{view_indicator(21)}
view_indicator <- function(indicator_index, url = "http://portal.saude.sp.gov.br/links/matriz", timeout = 1) {
  links <- tabnet_index(url = url)$Links
  indicator_info <- NULL
  if (length(links) >= indicator_index) {
    obj <- make_tabnet_obj(links[indicator_index])
    name <- obj$Nome
    name_start_index <- unlist(gregexpr("-", name))
    name <- substr(name, name_start_index + 2, nchar(name))
    indicator_info$Indicator.Name <- paste0(indicator_index, " - ", name)
    indicator_info$Indicator.URL <- obj$url
    indicator_info$Available.Regions <- obj$NomesLinhas[obj$NomesLinhas != "Ano"]
    indicator_info$Available.Years <- obj$Anos
    indicator_info$Available.Subindicators <- obj$NomesIndicadores
  }
  
  return(indicator_info)
}