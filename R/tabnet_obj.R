library(xml2)
library(rvest)

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
  
  body <- paste0("Linha=", W1252.encode(lines), "&Coluna=Ano",
                 "&Incremento=", W1252.encode(indicators), 
                 tabnet_concatFormat(W1252.encode(years)), 
                 "&pesqmes1=Digite+o+texto+e+ache+f%E1cil&",
                 "SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&",
                 "SDRS_de_Resid=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SRRAS-Resid.=",
                 "TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&",
                 "zeradas=exibirlz&formato=table&mostre=Mostra"
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
  return (gsub("deftohtm.exe?tabnet", "tabcgi.exe?tabnet", ind_url, fixed = TRUE))
}

#' Create a TABNET Object
#'
#' Create an object containing a certain indicator's informations.
#' @param indicator_url Base url of the TABNET indicator. 
#' @keywords tabnet
#' @export
#' @return An object containing information about the indicator url, its parameters, available years and files. This object is used to perform requests for data.
#' @examples
make_tabnet_obj <- function(indicator_url) {
  obj <- NULL
  html_obj <- read_html(indicator_url)
  if (!is.null(html_obj)) {
    obj$url <- indicator_url
    obj$POST_url <- getPostURL(indicator_url) 
    
    
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
    
    obj$Anos <- strsplit(
      tabnet_breakline_fix(
        html_obj %>%
          html_nodes("#A") %>%
          html_text()
      ), "    "
    )[[1]]
    
    obj$Arquivos <- tabnet_indicator_catch(
      html_obj %>%
        html_nodes("#A") %>%
        html_children()
    )
  }
  
  return (obj)
  
}